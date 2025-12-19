# main2.py - Auto Trading Terminal (Standalone)
import logging
import asyncio
import time
from datetime import datetime
from binance_futures import BinanceFuturesTrader
from scanner import MarketScanner
from shared_state import SharedState
from ai import analyze_with_gpt
import re

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('auto_trading.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class AutoTradingTerminal:
    def __init__(self):
        self.futures_trader = BinanceFuturesTrader()
        self.scanner = MarketScanner()
        self.state = SharedState()
        self.is_running = False
        
        state = self.state.get_state()
        self.config = state.get('config', {
            'max_leverage': 20,
            'position_size_pct': 15,
            'max_loss_pct': 17.5,
            'min_confidence': 85,
            'timeframe': '1h',
            'scan_interval': 300
        })
        
        logger.info("="*60)
        logger.info("🤖 AUTO TRADING TERMINAL INITIALIZED")
        logger.info("="*60)
    
    def get_balance_info(self):
        balance = self.futures_trader.get_futures_balance()
        if balance:
            return {
                'total': balance['balance'],
                'available': balance['available'],
                'unrealized_pnl': balance['unrealized_pnl']
            }
        return None
    
    def calculate_prices_from_ai(self, ai_response, current_price):
        try:
            if 'LONG' in ai_response.upper():
                side = 'BUY'
            elif 'SHORT' in ai_response.upper():
                side = 'SELL'
            else:
                return None, None, None, None
            
            entry_match = re.search(r'Entry Range.*?\$?([\d.,]+)\s*-\s*\$?([\d.,]+)', ai_response, re.IGNORECASE)
            if entry_match:
                entry_low = float(entry_match.group(1).replace(',', ''))
                entry_high = float(entry_match.group(2).replace(',', ''))
                entry_price = (entry_low + entry_high) / 2
            else:
                entry_price = current_price * (0.998 if side == 'BUY' else 1.002)
            
            tp_pattern = r'TP[123].*?\$?([\d.,]+)'
            tp_matches = re.findall(tp_pattern, ai_response, re.IGNORECASE)
            if tp_matches and len(tp_matches) >= 2:
                tp_price = float(tp_matches[1].replace(',', ''))
            else:
                tp_price = entry_price * (1.03 if side == 'BUY' else 0.97)
            
            sl_match = re.search(r'Stop Loss.*?\$?([\d.,]+)', ai_response, re.IGNORECASE)
            if sl_match:
                sl_price = float(sl_match.group(1).replace(',', ''))
            else:
                sl_price = entry_price * (0.98 if side == 'BUY' else 1.02)
            
            return side, entry_price, tp_price, sl_price
        except Exception as e:
            logger.error(f"Error parsing AI: {e}")
            return None, None, None, None
    
    def execute_trade(self, signal):
        try:
            logger.info("\n" + "="*60)
            logger.info("🎯 EXECUTING TRADE")
            logger.info("="*60)
            
            balance_info = self.get_balance_info()
            if not balance_info:
                logger.error("❌ Failed to get balance")
                return None
            
            position_size = balance_info['available'] * (self.config['position_size_pct'] / 100)
            
            logger.info(f"💰 Balance: ${balance_info['available']:.2f}")
            logger.info(f"📊 Position Size: ${position_size:.2f}")
            
            side, entry, tp, sl = self.calculate_prices_from_ai(signal['ai_analysis'], signal['current_price'])
            
            if not side:
                logger.warning("⚠️ No valid signal")
                return None
            
            quantity = self.futures_trader.calculate_quantity(signal['symbol'], entry, position_size)
            if not quantity:
                logger.error("❌ Failed to calculate quantity")
                return None
            
            logger.info(f"\n📋 TRADE DETAILS:")
            logger.info(f"   Symbol: {signal['symbol']}")
            logger.info(f"   Side: {side}")
            logger.info(f"   Entry: ${entry:.4f}")
            logger.info(f"   TP: ${tp:.4f}")
            logger.info(f"   SL: ${sl:.4f}")
            logger.info(f"   Quantity: {quantity}")
            logger.info(f"   Leverage: {self.config['max_leverage']}x")
            logger.info(f"   Confidence: {signal['confidence']}%")
            
            result = self.futures_trader.place_futures_order(
                symbol=signal['symbol'],
                side=side,
                quantity=quantity,
                entry_price=entry,
                tp_price=tp,
                sl_price=sl,
                leverage=self.config['max_leverage']
            )
            
            if result['success']:
                trade_record = {
                    'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
                    'symbol': signal['symbol'],
                    'side': side,
                    'entry_price': entry,
                    'tp_price': tp,
                    'sl_price': sl,
                    'quantity': quantity,
                    'leverage': self.config['max_leverage'],
                    'position_size': position_size,
                    'confidence': signal['confidence'],
                    'status': 'OPEN'
                }
                
                self.state.add_trade_log(trade_record)
                logger.info("\n✅ TRADE EXECUTED SUCCESSFULLY!")
                logger.info("="*60 + "\n")
                return trade_record
            else:
                logger.error(f"❌ TRADE FAILED: {result.get('error', 'Unknown')}")
                self.state.add_error(f"Trade failed: {result.get('error')}")
                return None
                
        except Exception as e:
            logger.error(f"❌ Error executing trade: {e}")
            self.state.add_error(f"Execute error: {str(e)}")
            return None
    
    async def trading_loop(self):
        logger.info("\n" + "🟢"*30)
        logger.info("🤖 AUTO TRADING STARTED!")
        logger.info("🟢"*30 + "\n")
        
        self.state.update_status(True, datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
        
        while self.is_running:
            try:
                # Update balance
                balance = self.get_balance_info()
                if balance:
                    self.state.update_balance(balance)
                    logger.info(f"💰 Balance: ${balance['available']:.2f} | PnL: ${balance['unrealized_pnl']:.2f}")
                
                # Check positions
                positions = self.futures_trader.get_open_positions()
                self.state.update_positions(positions if positions else [])
                
                if positions and len(positions) >= 1:
                    logger.info("📊 Max positions reached, waiting...")
                    await asyncio.sleep(60)
                    continue
                
                # Scan market
                logger.info("\n🔍 SCANNING MARKET...")
                logger.info("-"*60)
                
                scan_start = time.time()
                signals = self.scanner.scan_for_signals(
                    timeframe=self.config['timeframe'],
                    min_confidence=self.config['min_confidence'],
                    top_n=3
                )
                scan_time = time.time() - scan_start
                
                scan_info = {
                    'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
                    'signals_found': len(signals) if signals else 0,
                    'scan_time': round(scan_time, 2)
                }
                self.state.update_last_scan(scan_info)
                
                if not signals:
                    logger.info("❌ No high confidence signals found")
                    logger.info(f"⏳ Waiting {self.config['scan_interval']}s for next scan...\n")
                    await asyncio.sleep(self.config['scan_interval'])
                    continue
                
                # Execute best signal
                best_signal = signals[0]
                logger.info(f"\n✨ BEST SIGNAL FOUND:")
                logger.info(f"   {best_signal['symbol']}")
                logger.info(f"   Confidence: {best_signal['confidence']}%")
                logger.info(f"   Tech Score: {best_signal['tech_score']}/100")
                logger.info(f"   24h Change: {best_signal['price_change_24h']:.2f}%")
                
                trade = self.execute_trade(best_signal)
                
                if trade:
                    logger.info("✅ Trade executed, monitoring position...")
                    await asyncio.sleep(600)  # Wait 10 min
                else:
                    logger.info("❌ Trade not executed, continuing scan...")
                    await asyncio.sleep(self.config['scan_interval'])
                
            except Exception as e:
                logger.error(f"❌ Error in trading loop: {e}")
                self.state.add_error(f"Loop error: {str(e)}")
                await asyncio.sleep(60)
        
        logger.info("\n" + "🔴"*30)
        logger.info("🤖 AUTO TRADING STOPPED!")
        logger.info("🔴"*30 + "\n")
        self.state.update_status(False)
    
    def start(self):
        self.is_running = True
        asyncio.run(self.trading_loop())
    
    def stop(self):
        self.is_running = False
        logger.info("🛑 Stopping auto trading...")

if __name__ == "__main__":
    try:
        bot = AutoTradingTerminal()
        bot.start()
    except KeyboardInterrupt:
        logger.info("\n⚠️ Interrupted by user")
        bot.stop()
    except Exception as e:
        logger.error(f"❌ Fatal error: {e}")
