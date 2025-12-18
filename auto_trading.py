# auto_trading.py - Auto Trading Engine with AI + DCA Strategy
import logging
import asyncio
from datetime import datetime
from binance_futures import BinanceFuturesTrader
from scanner import MarketScanner
from config import GROQ_API_KEY
import re

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class AutoTradingBot:
    def __init__(self):
        """Initialize Auto Trading Bot"""
        self.futures_trader = BinanceFuturesTrader()
        self.scanner = MarketScanner()
        self.is_running = False
        self.current_position = None
        self.trade_log = []
        
        # Trading Config
        self.config = {
            'max_leverage': 20,
            'position_size_pct': 10,  # 10-20% of balance
            'max_loss_pct': 15,  # 15-20% average = 17.5%
            'min_confidence': 90,  # Minimum 90% for futures
            'timeframe': '1h',
            'scan_interval': 300,  # 5 minutes
            'max_open_positions': 1
        }
    
    def get_balance_info(self):
        """Get current balance information"""
        balance = self.futures_trader.get_futures_balance()
        if balance:
            return {
                'total_balance': balance['balance'],
                'available_balance': balance['available'],
                'unrealized_pnl': balance['unrealized_pnl']
            }
        return None
    
    def calculate_position_size(self, balance):
        """Calculate position size based on balance"""
        position_size = balance * (self.config['position_size_pct'] / 100)
        return round(position_size, 2)
    
    def calculate_prices_from_ai(self, ai_response, current_price):
        """
        Extract entry, TP, and SL prices from AI analysis
        Returns: (side, entry_range, tp_prices, sl_price)
        """
        try:
            # Detect position type (LONG/SHORT)
            if 'LONG' in ai_response.upper():
                side = 'BUY'
            elif 'SHORT' in ai_response.upper():
                side = 'SELL'
            else:
                side = 'WAIT'
            
            if side == 'WAIT':
                return None, None, None, None
            
            # Extract Entry Range
            entry_match = re.search(r'Entry Range.*?\$?([\d.,]+)\s*-\s*\$?([\d.,]+)', ai_response, re.IGNORECASE)
            if entry_match:
                entry_low = float(entry_match.group(1).replace(',', ''))
                entry_high = float(entry_match.group(2).replace(',', ''))
                entry_price = (entry_low + entry_high) / 2  # Use middle of range
            else:
                # Fallback: use current price with 0.5% buffer
                if side == 'BUY':
                    entry_price = current_price * 0.998  # Slightly below current
                else:
                    entry_price = current_price * 1.002  # Slightly above current
            
            # Extract TP prices
            tp_pattern = r'TP[123].*?\$?([\d.,]+)'
            tp_matches = re.findall(tp_pattern, ai_response, re.IGNORECASE)
            
            if tp_matches and len(tp_matches) >= 3:
                tp1 = float(tp_matches[0].replace(',', ''))
                tp2 = float(tp_matches[1].replace(',', ''))
                tp3 = float(tp_matches[2].replace(',', ''))
                # Use TP2 as main target (40% position)
                tp_price = tp2
            else:
                # Fallback: calculate TP based on position type
                if side == 'BUY':
                    tp_price = entry_price * 1.03  # 3% profit target
                else:
                    tp_price = entry_price * 0.97  # 3% profit target
            
            # Extract Stop Loss
            sl_match = re.search(r'Stop Loss.*?\$?([\d.,]+)', ai_response, re.IGNORECASE)
            if sl_match:
                sl_price = float(sl_match.group(1).replace(',', ''))
            else:
                # Fallback: use max loss percentage
                if side == 'BUY':
                    sl_price = entry_price * 0.98  # 2% stop loss
                else:
                    sl_price = entry_price * 1.02  # 2% stop loss
            
            return side, entry_price, tp_price, sl_price
            
        except Exception as e:
            logger.error(f"Error parsing AI response: {e}")
            return None, None, None, None
    
    def validate_risk(self, entry_price, sl_price, side, position_size_usd, leverage):
        """
        Validate if risk is within acceptable limits
        Returns: (is_valid, risk_pct, potential_loss)
        """
        try:
            # Calculate price difference percentage
            if side == 'BUY':
                price_diff_pct = ((entry_price - sl_price) / entry_price) * 100
            else:
                price_diff_pct = ((sl_price - entry_price) / entry_price) * 100
            
            # Calculate actual loss with leverage
            risk_pct = price_diff_pct * leverage
            
            # Calculate potential loss in USD
            potential_loss = position_size_usd * (risk_pct / 100)
            
            # Check if within acceptable range
            balance_info = self.get_balance_info()
            if not balance_info:
                return False, 0, 0
            
            total_balance = balance_info['total_balance']
            max_loss_usd = total_balance * (self.config['max_loss_pct'] / 100)
            
            is_valid = potential_loss <= max_loss_usd
            
            logger.info(f"Risk Validation: Loss ${potential_loss:.2f} vs Max ${max_loss_usd:.2f} ({risk_pct:.2f}%)")
            
            return is_valid, risk_pct, potential_loss
            
        except Exception as e:
            logger.error(f"Error validating risk: {e}")
            return False, 0, 0
    
    def execute_trade(self, signal):
        """Execute trade based on signal"""
        try:
            # Get balance
            balance_info = self.get_balance_info()
            if not balance_info:
                logger.error("Failed to get balance info")
                return None
            
            available_balance = balance_info['available_balance']
            
            # Calculate position size
            position_size_usd = self.calculate_position_size(available_balance)
            
            logger.info(f"📊 Balance: ${available_balance:.2f}, Position Size: ${position_size_usd:.2f}")
            
            # Parse AI response for prices
            side, entry_price, tp_price, sl_price = self.calculate_prices_from_ai(
                signal['ai_analysis'], 
                signal['current_price']
            )
            
            if not side or side == 'WAIT':
                logger.info("AI suggests WAIT, skipping trade")
                return None
            
            # Validate risk
            is_valid, risk_pct, potential_loss = self.validate_risk(
                entry_price, sl_price, side, position_size_usd, self.config['max_leverage']
            )
            
            if not is_valid:
                logger.warning(f"⚠️ Risk too high! Potential loss: ${potential_loss:.2f}")
                return None
            
            # Calculate quantity
            quantity = self.futures_trader.calculate_quantity(
                signal['symbol'], 
                entry_price, 
                position_size_usd
            )
            
            if not quantity:
                logger.error("Failed to calculate quantity")
                return None
            
            logger.info(f"""
🎯 Executing Trade:
Symbol: {signal['symbol']}
Side: {side}
Entry: ${entry_price:.4f}
TP: ${tp_price:.4f}
SL: ${sl_price:.4f}
Quantity: {quantity}
Leverage: {self.config['max_leverage']}x
Position Size: ${position_size_usd:.2f}
Risk: {risk_pct:.2f}% (${potential_loss:.2f})
""")
            
            # Place order
            result = self.futures_trader.place_futures_order(
                symbol=signal['symbol'],
                side=side,
                quantity=quantity,
                entry_price=entry_price,
                tp_price=tp_price,
                sl_price=sl_price,
                leverage=self.config['max_leverage']
            )
            
            if result['success']:
                # Log trade
                trade_record = {
                    'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
                    'symbol': signal['symbol'],
                    'side': side,
                    'entry_price': entry_price,
                    'tp_price': tp_price,
                    'sl_price': sl_price,
                    'quantity': quantity,
                    'leverage': self.config['max_leverage'],
                    'position_size': position_size_usd,
                    'confidence': signal['confidence'],
                    'status': 'OPEN'
                }
                
                self.trade_log.append(trade_record)
                self.current_position = trade_record
                
                logger.info("✅ Trade executed successfully!")
                return trade_record
            else:
                logger.error(f"❌ Trade execution failed: {result.get('error', 'Unknown error')}")
                return None
                
        except Exception as e:
            logger.error(f"Error executing trade: {e}")
            return None
    
    def check_positions(self):
        """Check current open positions and update status"""
        try:
            open_positions = self.futures_trader.get_open_positions()
            
            if not open_positions:
                if self.current_position and self.current_position['status'] == 'OPEN':
                    # Position was closed (TP or SL hit)
                    self._handle_position_closed()
                self.current_position = None
                return None
            
            # Update current position info
            if open_positions and self.current_position:
                pos = open_positions[0]
                self.current_position['unrealized_pnl'] = pos['unrealized_pnl']
                self.current_position['mark_price'] = pos['mark_price']
            
            return open_positions
            
        except Exception as e:
            logger.error(f"Error checking positions: {e}")
            return None
    
    def _handle_position_closed(self):
        """Handle when position is closed"""
        if not self.current_position:
            return
        
        try:
            # Get account info to determine if profit or loss
            # This is simplified - in production you'd want to check actual trade history
            balance_after = self.get_balance_info()
            
            self.current_position['status'] = 'CLOSED'
            self.current_position['closed_at'] = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            
            logger.info(f"Position closed: {self.current_position['symbol']}")
            
        except Exception as e:
            logger.error(f"Error handling position close: {e}")
    
    async def scan_and_trade(self):
        """Main loop: scan market and execute trades"""
        logger.info("🤖 Auto Trading Bot Started!")
        
        while self.is_running:
            try:
                # Check if we already have an open position
                open_positions = self.check_positions()
                
                if open_positions and len(open_positions) >= self.config['max_open_positions']:
                    logger.info("Max positions reached, waiting...")
                    await asyncio.sleep(60)  # Check every minute
                    continue
                
                # Scan for signals
                logger.info("🔍 Scanning market for signals...")
                signals = self.scanner.scan_for_signals(
                    timeframe=self.config['timeframe'],
                    min_confidence=self.config['min_confidence'],
                    top_n=3
                )
                
                if not signals:
                    logger.info("No high confidence signals found")
                    await asyncio.sleep(self.config['scan_interval'])
                    continue
                
                # Execute trade on best signal
                best_signal = signals[0]
                logger.info(f"📊 Best Signal: {best_signal['symbol']} - Confidence: {best_signal['confidence']}%")
                
                trade = self.execute_trade(best_signal)
                
                if trade:
                    logger.info("✅ Trade executed, waiting for completion...")
                    # Wait longer after executing trade
                    await asyncio.sleep(600)  # 10 minutes
                else:
                    logger.info("❌ Trade not executed, continuing scan...")
                    await asyncio.sleep(self.config['scan_interval'])
                
            except Exception as e:
                logger.error(f"Error in trading loop: {e}")
                await asyncio.sleep(60)
    
    def start(self):
        """Start auto trading"""
        self.is_running = True
        logger.info("🟢 Auto Trading STARTED")
    
    def stop(self):
        """Stop auto trading"""
        self.is_running = False
        logger.info("🔴 Auto Trading STOPPED")
    
    def get_status(self):
        """Get current status"""
        balance_info = self.get_balance_info()
        open_positions = self.check_positions()
        
        status = {
            'is_running': self.is_running,
            'balance': balance_info,
            'open_positions': open_positions,
            'current_position': self.current_position,
            'config': self.config,
            'total_trades': len(self.trade_log)
        }
        
        return status
    
    def get_trade_log(self, limit=10):
        """Get recent trade log"""
        return self.trade_log[-limit:]
    
    def update_config(self, **kwargs):
        """Update trading configuration"""
        for key, value in kwargs.items():
            if key in self.config:
                self.config[key] = value
                logger.info(f"Config updated: {key} = {value}")
    
    def emergency_close_all(self):
        """Emergency: Close all positions"""
        try:
            open_positions = self.futures_trader.get_open_positions()
            
            for pos in open_positions:
                logger.warning(f"🚨 Emergency closing: {pos['symbol']}")
                self.futures_trader.close_position(pos['symbol'])
            
            self.stop()
            logger.info("✅ All positions closed, bot stopped")
            
        except Exception as e:
            logger.error(f"Error in emergency close: {e}")
