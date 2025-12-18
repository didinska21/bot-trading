# binance_futures.py - Binance Futures API Wrapper
import logging
from binance.client import Client
from binance.exceptions import BinanceAPIException
from decimal import Decimal, ROUND_DOWN
from config import BINANCE_API_KEY, BINANCE_API_SECRET

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class BinanceFuturesTrader:
    def __init__(self):
        """Initialize Binance Futures client"""
        self.client = Client(api_key=BINANCE_API_KEY, api_secret=BINANCE_API_SECRET)
        self.client.API_URL = 'https://fapi.binance.com/fapi'  # Futures API endpoint
        
    def get_futures_balance(self):
        """Get USDT balance in Futures wallet"""
        try:
            account = self.client.futures_account()
            for asset in account['assets']:
                if asset['asset'] == 'USDT':
                    return {
                        'balance': float(asset['walletBalance']),
                        'available': float(asset['availableBalance']),
                        'unrealized_pnl': float(asset['unrealizedProfit'])
                    }
            return None
        except BinanceAPIException as e:
            logger.error(f"Error getting futures balance: {e}")
            return None
    
    def get_symbol_info(self, symbol):
        """Get symbol trading rules and precision"""
        try:
            exchange_info = self.client.futures_exchange_info()
            for s in exchange_info['symbols']:
                if s['symbol'] == symbol:
                    # Extract important info
                    filters = {f['filterType']: f for f in s['filters']}
                    
                    return {
                        'symbol': symbol,
                        'status': s['status'],
                        'price_precision': s['pricePrecision'],
                        'quantity_precision': s['quantityPrecision'],
                        'min_qty': float(filters['LOT_SIZE']['minQty']),
                        'max_qty': float(filters['LOT_SIZE']['maxQty']),
                        'step_size': float(filters['LOT_SIZE']['stepSize']),
                        'min_notional': float(filters['MIN_NOTIONAL']['notional']) if 'MIN_NOTIONAL' in filters else 5.0,
                        'tick_size': float(filters['PRICE_FILTER']['tickSize'])
                    }
            return None
        except Exception as e:
            logger.error(f"Error getting symbol info: {e}")
            return None
    
    def round_step_size(self, quantity, step_size):
        """Round quantity to valid step size"""
        quantity = Decimal(str(quantity))
        step_size = Decimal(str(step_size))
        return float(quantity - (quantity % step_size))
    
    def round_price(self, price, tick_size):
        """Round price to valid tick size"""
        price = Decimal(str(price))
        tick_size = Decimal(str(tick_size))
        return float((price // tick_size) * tick_size)
    
    def set_leverage(self, symbol, leverage):
        """Set leverage for symbol"""
        try:
            result = self.client.futures_change_leverage(
                symbol=symbol,
                leverage=leverage
            )
            logger.info(f"Leverage set to {leverage}x for {symbol}")
            return result
        except BinanceAPIException as e:
            logger.error(f"Error setting leverage: {e}")
            return None
    
    def set_margin_type(self, symbol, margin_type='ISOLATED'):
        """Set margin type (ISOLATED or CROSSED)"""
        try:
            result = self.client.futures_change_margin_type(
                symbol=symbol,
                marginType=margin_type
            )
            logger.info(f"Margin type set to {margin_type} for {symbol}")
            return result
        except BinanceAPIException as e:
            # Error code -4046 means margin type already set
            if e.code == -4046:
                logger.info(f"Margin type already {margin_type} for {symbol}")
                return True
            logger.error(f"Error setting margin type: {e}")
            return None
    
    def calculate_quantity(self, symbol, entry_price, position_size_usd):
        """Calculate quantity based on position size in USD"""
        try:
            symbol_info = self.get_symbol_info(symbol)
            if not symbol_info:
                return None
            
            # Calculate raw quantity
            quantity = position_size_usd / entry_price
            
            # Round to step size
            quantity = self.round_step_size(quantity, symbol_info['step_size'])
            
            # Check minimum quantity
            if quantity < symbol_info['min_qty']:
                logger.error(f"Quantity {quantity} below minimum {symbol_info['min_qty']}")
                return None
            
            # Check minimum notional
            notional = quantity * entry_price
            if notional < symbol_info['min_notional']:
                logger.error(f"Notional {notional} below minimum {symbol_info['min_notional']}")
                return None
            
            return quantity
            
        except Exception as e:
            logger.error(f"Error calculating quantity: {e}")
            return None
    
    def place_futures_order(self, symbol, side, quantity, entry_price, tp_price, sl_price, leverage=10):
        """
        Place futures limit order with TP and SL
        
        Args:
            symbol: Trading pair (e.g., 'BTCUSDT')
            side: 'BUY' for LONG, 'SELL' for SHORT
            quantity: Order quantity
            entry_price: Entry limit price
            tp_price: Take profit price
            sl_price: Stop loss price
            leverage: Leverage to use
        """
        try:
            # 1. Set leverage
            self.set_leverage(symbol, leverage)
            
            # 2. Set margin type to ISOLATED (safer)
            self.set_margin_type(symbol, 'ISOLATED')
            
            # 3. Get symbol info for rounding
            symbol_info = self.get_symbol_info(symbol)
            if not symbol_info:
                return {'success': False, 'error': 'Failed to get symbol info'}
            
            # 4. Round prices
            entry_price = self.round_price(entry_price, symbol_info['tick_size'])
            tp_price = self.round_price(tp_price, symbol_info['tick_size'])
            sl_price = self.round_price(sl_price, symbol_info['tick_size'])
            
            # 5. Place entry limit order
            entry_order = self.client.futures_create_order(
                symbol=symbol,
                side=side,
                type='LIMIT',
                timeInForce='GTC',
                quantity=quantity,
                price=entry_price
            )
            
            logger.info(f"Entry order placed: {entry_order}")
            
            # 6. Place TP order (opposite side, limit order)
            tp_side = 'SELL' if side == 'BUY' else 'BUY'
            tp_order = self.client.futures_create_order(
                symbol=symbol,
                side=tp_side,
                type='TAKE_PROFIT_MARKET',
                stopPrice=tp_price,
                closePosition=True
            )
            
            logger.info(f"TP order placed: {tp_order}")
            
            # 7. Place SL order (opposite side, stop market)
            sl_order = self.client.futures_create_order(
                symbol=symbol,
                side=tp_side,
                type='STOP_MARKET',
                stopPrice=sl_price,
                closePosition=True
            )
            
            logger.info(f"SL order placed: {sl_order}")
            
            return {
                'success': True,
                'entry_order': entry_order,
                'tp_order': tp_order,
                'sl_order': sl_order
            }
            
        except BinanceAPIException as e:
            logger.error(f"Binance API Error: {e}")
            return {'success': False, 'error': str(e)}
        except Exception as e:
            logger.error(f"Error placing futures order: {e}")
            return {'success': False, 'error': str(e)}
    
    def get_open_positions(self):
        """Get all open futures positions"""
        try:
            positions = self.client.futures_position_information()
            open_positions = []
            
            for pos in positions:
                position_amt = float(pos['positionAmt'])
                if position_amt != 0:  # Position is open
                    open_positions.append({
                        'symbol': pos['symbol'],
                        'side': 'LONG' if position_amt > 0 else 'SHORT',
                        'quantity': abs(position_amt),
                        'entry_price': float(pos['entryPrice']),
                        'mark_price': float(pos['markPrice']),
                        'unrealized_pnl': float(pos['unRealizedProfit']),
                        'leverage': int(pos['leverage']),
                        'liquidation_price': float(pos['liquidationPrice']) if pos['liquidationPrice'] else 0
                    })
            
            return open_positions
            
        except Exception as e:
            logger.error(f"Error getting open positions: {e}")
            return []
    
    def get_open_orders(self, symbol=None):
        """Get open orders"""
        try:
            if symbol:
                orders = self.client.futures_get_open_orders(symbol=symbol)
            else:
                orders = self.client.futures_get_open_orders()
            
            return orders
            
        except Exception as e:
            logger.error(f"Error getting open orders: {e}")
            return []
    
    def cancel_all_orders(self, symbol):
        """Cancel all open orders for a symbol"""
        try:
            result = self.client.futures_cancel_all_open_orders(symbol=symbol)
            logger.info(f"All orders cancelled for {symbol}")
            return result
        except Exception as e:
            logger.error(f"Error cancelling orders: {e}")
            return None
    
    def close_position(self, symbol):
        """Close position at market price"""
        try:
            # Get current position
            positions = self.get_open_positions()
            position = next((p for p in positions if p['symbol'] == symbol), None)
            
            if not position:
                logger.info(f"No open position for {symbol}")
                return None
            
            # Determine side (opposite of position)
            side = 'SELL' if position['side'] == 'LONG' else 'BUY'
            
            # Close position with market order
            result = self.client.futures_create_order(
                symbol=symbol,
                side=side,
                type='MARKET',
                quantity=position['quantity']
            )
            
            logger.info(f"Position closed: {result}")
            return result
            
        except Exception as e:
            logger.error(f"Error closing position: {e}")
            return None
    
    def get_current_price(self, symbol):
        """Get current market price"""
        try:
            ticker = self.client.futures_symbol_ticker(symbol=symbol)
            return float(ticker['price'])
        except Exception as e:
            logger.error(f"Error getting price: {e}")
            return None
