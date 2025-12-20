# binance_futures.py - IMPROVED VERSION with Better Error Handling

import logging
import time
from binance.client import Client
from binance.exceptions import BinanceAPIException
from decimal import Decimal, ROUND_DOWN
from config import BINANCE_API_KEY, BINANCE_API_SECRET

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class BinanceFuturesTrader:
    def __init__(self):
        """Initialize Binance Futures client"""
        try:
            self.client = Client(
                api_key=BINANCE_API_KEY,
                api_secret=BINANCE_API_SECRET
            )
            # Set to Futures API endpoint
            self.client.API_URL = "https://fapi.binance.com/fapi"
            logger.info("✅ Binance Futures client initialized successfully")
        except Exception as e:
            logger.error(f"❌ Failed to initialize Binance client: {e}")
            raise

    # ============================================================
    # ========================= BALANCE ==========================
    # ============================================================

    def get_futures_balance(self):
        """Get USDT balance in Futures wallet"""
        try:
            account = self.client.futures_account()
            for asset in account["assets"]:
                if asset["asset"] == "USDT":
                    balance_data = {
                        "balance": float(asset["walletBalance"]),
                        "availableBalance": float(asset["availableBalance"]),
                        "unrealized_pnl": float(asset["unrealizedProfit"]),
                        "crossWalletBalance": float(asset.get("crossWalletBalance", 0)),
                        "marginBalance": float(asset.get("marginBalance", 0)),
                    }
                    logger.info(f"📊 Balance: ${balance_data['availableBalance']:.2f}")
                    return balance_data
            
            logger.warning("⚠️ USDT asset not found in futures account")
            return None
            
        except BinanceAPIException as e:
            logger.error(f"❌ Binance API Error getting balance: {e.message} (Code: {e.code})")
            return None
        except Exception as e:
            logger.error(f"❌ Error getting futures balance: {e}")
            return None

    # ============================================================
    # ========================= SYMBOL INFO ======================
    # ============================================================

    def get_symbol_info(self, symbol):
        """Get symbol trading rules and precision"""
        try:
            exchange_info = self.client.futures_exchange_info()
            
            for s in exchange_info["symbols"]:
                if s["symbol"] == symbol:
                    # Check if symbol is trading
                    if s["status"] != "TRADING":
                        logger.warning(f"⚠️ Symbol {symbol} status: {s['status']}")
                        return None
                    
                    # Extract filters
                    filters = {f["filterType"]: f for f in s["filters"]}
                    
                    # Get min notional (different filter names in different exchanges)
                    min_notional = 5.0  # default
                    if "MIN_NOTIONAL" in filters:
                        min_notional = float(filters["MIN_NOTIONAL"]["notional"])
                    elif "NOTIONAL" in filters:
                        min_notional = float(filters["NOTIONAL"]["notional"])
                    
                    symbol_info = {
                        "symbol": symbol,
                        "status": s["status"],
                        "price_precision": s["pricePrecision"],
                        "quantity_precision": s["quantityPrecision"],
                        "base_asset": s["baseAsset"],
                        "quote_asset": s["quoteAsset"],
                        "min_qty": float(filters["LOT_SIZE"]["minQty"]),
                        "max_qty": float(filters["LOT_SIZE"]["maxQty"]),
                        "step_size": float(filters["LOT_SIZE"]["stepSize"]),
                        "min_notional": min_notional,
                        "tick_size": float(filters["PRICE_FILTER"]["tickSize"]),
                        "max_leverage": s.get("maxLeverage", 125),
                    }
                    
                    logger.info(f"📊 Symbol info loaded for {symbol}")
                    return symbol_info
            
            logger.error(f"❌ Symbol {symbol} not found")
            return None
            
        except BinanceAPIException as e:
            logger.error(f"❌ Binance API Error getting symbol info: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error getting symbol info: {e}")
            return None

    # ============================================================
    # ========================= ROUNDING =========================
    # ============================================================

    def round_step_size(self, quantity, step_size):
        """Round quantity to valid step size"""
        try:
            quantity = Decimal(str(quantity))
            step_size = Decimal(str(step_size))
            rounded = float(quantity - (quantity % step_size))
            return rounded
        except Exception as e:
            logger.error(f"❌ Error rounding step size: {e}")
            return quantity

    def round_price(self, price, tick_size):
        """Round price to valid tick size"""
        try:
            price = Decimal(str(price))
            tick_size = Decimal(str(tick_size))
            rounded = float(price.quantize(tick_size, rounding=ROUND_DOWN))
            return rounded
        except Exception as e:
            logger.error(f"❌ Error rounding price: {e}")
            return price

    # ============================================================
    # ========================= LEVERAGE =========================
    # ============================================================

    def set_leverage(self, symbol, leverage):
        """Set leverage for symbol"""
        try:
            result = self.client.futures_change_leverage(
                symbol=symbol,
                leverage=leverage
            )
            logger.info(f"✅ Leverage set to {leverage}x for {symbol}")
            return result
        except BinanceAPIException as e:
            # -4028: Leverage not changed (already at desired leverage)
            if e.code == -4028:
                logger.info(f"ℹ️ Leverage already at {leverage}x for {symbol}")
                return True
            logger.error(f"❌ Error setting leverage: {e.message} (Code: {e.code})")
            return None
        except Exception as e:
            logger.error(f"❌ Error setting leverage: {e}")
            return None

    def set_margin_type(self, symbol, margin_type="ISOLATED"):
        """Set margin type (ISOLATED or CROSSED)"""
        try:
            result = self.client.futures_change_margin_type(
                symbol=symbol,
                marginType=margin_type
            )
            logger.info(f"✅ Margin type set to {margin_type} for {symbol}")
            return result
        except BinanceAPIException as e:
            # -4046: No need to change margin type
            if e.code == -4046:
                logger.info(f"ℹ️ Margin type already {margin_type} for {symbol}")
                return True
            logger.error(f"❌ Error setting margin type: {e.message} (Code: {e.code})")
            return None
        except Exception as e:
            logger.error(f"❌ Error setting margin type: {e}")
            return None

    # ============================================================
    # ========================= QUANTITY =========================
    # ============================================================

    def calculate_quantity(self, symbol, entry_price, position_size_usd):
        """Calculate valid quantity based on position size in USD"""
        try:
            info = self.get_symbol_info(symbol)
            if not info:
                logger.error(f"❌ Cannot get symbol info for {symbol}")
                return None

            # Calculate raw quantity
            quantity = position_size_usd / entry_price
            
            # Round to step size
            quantity = self.round_step_size(quantity, info["step_size"])

            # Validate minimum quantity
            if quantity < info["min_qty"]:
                logger.error(f"❌ Quantity {quantity} below minimum {info['min_qty']}")
                return None

            # Validate maximum quantity
            if quantity > info["max_qty"]:
                logger.error(f"❌ Quantity {quantity} exceeds maximum {info['max_qty']}")
                return None

            # Validate minimum notional
            notional = quantity * entry_price
            if notional < info["min_notional"]:
                logger.error(f"❌ Notional ${notional:.2f} below minimum ${info['min_notional']:.2f}")
                return None

            logger.info(f"✅ Calculated quantity: {quantity} (Notional: ${notional:.2f})")
            return quantity
            
        except Exception as e:
            logger.error(f"❌ Error calculating quantity: {e}")
            return None

    # ============================================================
    # ========================= VALIDATION =======================
    # ============================================================

    def validate_tp_sl(self, side, entry, tp, sl):
        """
        Validate TP/SL levels - IMPROVED VERSION
        
        Args:
            side: BUY (LONG) or SELL (SHORT)
            entry: Entry price
            tp: Take profit price
            sl: Stop loss price
        
        Returns:
            bool: True if valid, False otherwise
        """
        try:
            entry = float(entry)
            tp = float(tp)
            sl = float(sl)
            
            logger.info(f"Validating: Side={side}, Entry={entry:.4f}, TP={tp:.4f}, SL={sl:.4f}")
            
            if side.upper() in ["BUY", "LONG"]:
                # For LONG: TP must be above entry, SL below
                if tp <= entry:
                    logger.error(f"❌ Invalid TP for LONG: TP {tp:.4f} <= Entry {entry:.4f}")
                    logger.error(f"   For LONG positions, TP must be ABOVE entry price")
                    return False
                if sl >= entry:
                    logger.error(f"❌ Invalid SL for LONG: SL {sl:.4f} >= Entry {entry:.4f}")
                    logger.error(f"   For LONG positions, SL must be BELOW entry price")
                    return False
                    
                logger.info(f"✅ LONG validation passed: Entry < SL < TP")
                
            else:  # SELL or SHORT
                # For SHORT: TP must be below entry, SL above
                if tp >= entry:
                    logger.error(f"❌ Invalid TP for SHORT: TP {tp:.4f} >= Entry {entry:.4f}")
                    logger.error(f"   For SHORT positions, TP must be BELOW entry price")
                    return False
                if sl <= entry:
                    logger.error(f"❌ Invalid SL for SHORT: SL {sl:.4f} <= Entry {entry:.4f}")
                    logger.error(f"   For SHORT positions, SL must be ABOVE entry price")
                    return False
                    
                logger.info(f"✅ SHORT validation passed: TP < Entry < SL")
            
            return True
            
        except Exception as e:
            logger.error(f"❌ Error validating TP/SL: {e}")
            return False

    # ============================================================
    # ========================= ORDER MANAGEMENT =================
    # ============================================================

    def cancel_order(self, symbol, order_id):
        """Cancel an order"""
        try:
            result = self.client.futures_cancel_order(
                symbol=symbol,
                orderId=order_id
            )
            logger.info(f"✅ Order {order_id} cancelled for {symbol}")
            return result
        except BinanceAPIException as e:
            logger.error(f"❌ Error cancelling order: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error cancelling order: {e}")
            return None

    def cancel_all_orders(self, symbol):
        """Cancel all open orders for symbol"""
        try:
            result = self.client.futures_cancel_all_open_orders(symbol=symbol)
            logger.info(f"✅ All orders cancelled for {symbol}")
            return result
        except BinanceAPIException as e:
            logger.error(f"❌ Error cancelling all orders: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error cancelling all orders: {e}")
            return None

    def get_order_status(self, symbol, order_id):
        """Get order status"""
        try:
            order = self.client.futures_get_order(
                symbol=symbol,
                orderId=order_id
            )
            return order
        except BinanceAPIException as e:
            logger.error(f"❌ Error getting order status: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error getting order status: {e}")
            return None

    # ============================================================
    # ========================= MAIN ORDER FUNCTION ==============
    # ============================================================

    def place_futures_order(
        self,
        symbol,
        side,
        quantity,
        entry_price,
        tp_price,
        sl_price,
        leverage=10,
        order_type="MARKET"
    ):
        """
        Place futures order with TP and SL
        
        Args:
            symbol: Trading pair (e.g., BTCUSDT)
            side: BUY (LONG) or SELL (SHORT)
            quantity: Order quantity
            entry_price: Entry price
            tp_price: Take profit price
            sl_price: Stop loss price
            leverage: Leverage (default 10x)
            order_type: MARKET or LIMIT
        """
        entry_order = None
        tp_order = None
        sl_order = None
        
        try:
            logger.info(f"🚀 Starting order placement for {symbol}")
            logger.info(f"📊 Side: {side}, Quantity: {quantity}, Entry: ${entry_price:.4f}")
            logger.info(f"🎯 TP: ${tp_price:.4f}, SL: ${sl_price:.4f}")
            
            # Step 1: Set leverage and margin type
            logger.info(f"⚙️ Setting leverage to {leverage}x...")
            if not self.set_leverage(symbol, leverage):
                return {"success": False, "error": "Failed to set leverage"}
            
            logger.info(f"⚙️ Setting margin type to ISOLATED...")
            if not self.set_margin_type(symbol, "ISOLATED"):
                return {"success": False, "error": "Failed to set margin type"}

            # Step 2: Get symbol info and validate
            info = self.get_symbol_info(symbol)
            if not info:
                return {"success": False, "error": "Failed to get symbol info"}

            # Step 3: Round prices
            entry_price = self.round_price(entry_price, info["tick_size"])
            tp_price = self.round_price(tp_price, info["tick_size"])
            sl_price = self.round_price(sl_price, info["tick_size"])

            logger.info(f"💰 Rounded prices - Entry: ${entry_price:.4f}, TP: ${tp_price:.4f}, SL: ${sl_price:.4f}")

            # Step 4: Validate TP/SL - CRITICAL CHECK
            if not self.validate_tp_sl(side, entry_price, tp_price, sl_price):
                error_msg = f"Invalid price levels for {side}: Entry={entry_price:.4f}, TP={tp_price:.4f}, SL={sl_price:.4f}"
                logger.error(f"❌ {error_msg}")
                return {"success": False, "error": error_msg}

            # Step 5: Place ENTRY order
            logger.info(f"📝 Placing {order_type} entry order...")
            
            if order_type == "MARKET":
                entry_order = self.client.futures_create_order(
                    symbol=symbol,
                    side=side,
                    type="MARKET",
                    quantity=quantity
                )
            else:  # LIMIT
                entry_order = self.client.futures_create_order(
                    symbol=symbol,
                    side=side,
                    type="LIMIT",
                    timeInForce="GTC",
                    quantity=quantity,
                    price=entry_price
                )

            if "orderId" not in entry_order:
                raise Exception("Entry order failed - no orderId returned")

            logger.info(f"✅ Entry order placed: {entry_order['orderId']}")

            # Step 6: Wait a bit if MARKET order
            if order_type == "MARKET":
                time.sleep(1)
                order_status = self.get_order_status(symbol, entry_order['orderId'])
                if order_status and order_status['status'] != 'FILLED':
                    logger.warning(f"⚠️ Entry order not filled yet: {order_status['status']}")

            # Step 7: Determine TP/SL side (opposite of entry)
            tp_sl_side = "SELL" if side == "BUY" else "BUY"

            # Step 8: Place TAKE PROFIT order
            logger.info(f"📝 Placing TP order at ${tp_price:.4f}...")
            try:
                tp_order = self.client.futures_create_order(
                    symbol=symbol,
                    side=tp_sl_side,
                    type="TAKE_PROFIT_MARKET",
                    stopPrice=tp_price,
                    closePosition=True
                )
                logger.info(f"✅ TP order placed: {tp_order['orderId']}")
            except BinanceAPIException as e:
                logger.error(f"❌ TP order failed: {e.message}")
                tp_order = {"orderId": None, "error": str(e)}

            # Step 9: Place STOP LOSS order
            logger.info(f"📝 Placing SL order at ${sl_price:.4f}...")
            try:
                sl_order = self.client.futures_create_order(
                    symbol=symbol,
                    side=tp_sl_side,
                    type="STOP_MARKET",
                    stopPrice=sl_price,
                    closePosition=True
                )
                logger.info(f"✅ SL order placed: {sl_order['orderId']}")
            except BinanceAPIException as e:
                logger.error(f"❌ SL order failed: {e.message}")
                if tp_order and "orderId" in tp_order:
                    self.cancel_order(symbol, tp_order["orderId"])
                sl_order = {"orderId": None, "error": str(e)}

            # Step 10: Return result
            success = all([
                entry_order and "orderId" in entry_order,
                tp_order and "orderId" in tp_order,
                sl_order and "orderId" in sl_order
            ])

            result = {
                "success": success,
                "entry_order": entry_order,
                "tp_order": tp_order,
                "sl_order": sl_order,
                "symbol": symbol,
                "side": side,
                "quantity": quantity,
                "entry_price": entry_price,
                "tp_price": tp_price,
                "sl_price": sl_price,
                "leverage": leverage
            }

            if success:
                logger.info(f"✅ Order placement completed successfully!")
            else:
                logger.warning(f"⚠️ Order placement completed with warnings")

            return result

        except BinanceAPIException as e:
            logger.error(f"❌ Binance API Error: {e.message} (Code: {e.code})")
            
            if entry_order and "orderId" in entry_order:
                logger.info("🧹 Cleaning up - cancelling entry order...")
                self.cancel_order(symbol, entry_order["orderId"])
            
            return {
                "success": False,
                "error": f"Binance API Error: {e.message}",
                "error_code": e.code
            }
            
        except Exception as e:
            logger.error(f"❌ Error placing futures order: {e}")
            
            if entry_order and "orderId" in entry_order:
                logger.info("🧹 Cleaning up - cancelling entry order...")
                self.cancel_order(symbol, entry_order["orderId"])
            
            return {
                "success": False,
                "error": str(e)
            }

    # ============================================================
    # ========================= POSITION MANAGEMENT ==============
    # ============================================================

    def get_open_positions(self):
        """Get all open positions"""
        try:
            positions = self.client.futures_position_information()
            open_positions = []

            for pos in positions:
                amt = float(pos["positionAmt"])
                if amt != 0:
                    position_data = {
                        "symbol": pos["symbol"],
                        "side": "LONG" if amt > 0 else "SHORT",
                        "quantity": abs(amt),
                        "entry_price": float(pos["entryPrice"]),
                        "mark_price": float(pos["markPrice"]),
                        "unrealized_pnl": float(pos["unRealizedProfit"]),
                        "leverage": int(pos["leverage"]),
                        "liquidation_price": float(pos.get("liquidationPrice", 0)),
                        "isolated_wallet": float(pos.get("isolatedWallet", 0)),
                        "position_side": pos.get("positionSide", "BOTH")
                    }
                    open_positions.append(position_data)

            logger.info(f"📊 Found {len(open_positions)} open positions")
            return open_positions
            
        except BinanceAPIException as e:
            logger.error(f"❌ Error getting open positions: {e.message}")
            return []
        except Exception as e:
            logger.error(f"❌ Error getting open positions: {e}")
            return []

    def close_position(self, symbol, quantity=None):
        """Close a position"""
        try:
            positions = self.get_open_positions()
            pos = next((p for p in positions if p["symbol"] == symbol), None)
            
            if not pos:
                logger.warning(f"⚠️ No open position found for {symbol}")
                return None

            side = "SELL" if pos["side"] == "LONG" else "BUY"
            close_quantity = quantity if quantity else pos["quantity"]

            logger.info(f"📝 Closing {pos['side']} position for {symbol}, quantity: {close_quantity}")

            order = self.client.futures_create_order(
                symbol=symbol,
                side=side,
                type="MARKET",
                quantity=close_quantity,
                reduceOnly=True
            )

            logger.info(f"✅ Position closed successfully")
            return order
            
        except BinanceAPIException as e:
            logger.error(f"❌ Error closing position: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error closing position: {e}")
            return None

    def close_all_positions(self):
        """Close all open positions"""
        try:
            positions = self.get_open_positions()
            
            if not positions:
                logger.info("ℹ️ No open positions to close")
                return []

            results = []
            for pos in positions:
                logger.info(f"📝 Closing position for {pos['symbol']}...")
                result = self.close_position(pos['symbol'])
                results.append({
                    "symbol": pos['symbol'],
                    "success": result is not None,
                    "order": result
                })

            logger.info(f"✅ Closed {len(results)} positions")
            return results
            
        except Exception as e:
            logger.error(f"❌ Error closing all positions: {e}")
            return []

    def get_current_price(self, symbol):
        """Get current market price"""
        try:
            ticker = self.client.futures_symbol_ticker(symbol=symbol)
            price = float(ticker["price"])
            logger.info(f"💰 Current price for {symbol}: ${price:.4f}")
            return price
        except BinanceAPIException as e:
            logger.error(f"❌ Error getting price: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error getting price: {e}")
            return None


if __name__ == "__main__":
    """Test the Binance Futures Trader"""
    trader = BinanceFuturesTrader()
    
    print("\n" + "="*50)
    print("TEST: Get Futures Balance")
    print("="*50)
    balance = trader.get_futures_balance()
    if balance:
        print(f"✅ Balance: ${balance['availableBalance']:.2f}")
    
    print("\n" + "="*50)
    print("TEST: Price Level Validation")
    print("="*50)
    
    # Test LONG validation
    print("\nTesting LONG position:")
    valid = trader.validate_tp_sl("BUY", 100, 110, 95)
    print(f"LONG (Entry=100, TP=110, SL=95): {'✅ Valid' if valid else '❌ Invalid'}")
    
    # Test SHORT validation
    print("\nTesting SHORT position:")
    valid = trader.validate_tp_sl("SELL", 100, 95, 105)
    print(f"SHORT (Entry=100, TP=95, SL=105): {'✅ Valid' if valid else '❌ Invalid'}")
    
    # Test invalid LONG
    print("\nTesting invalid LONG:")
    valid = trader.validate_tp_sl("BUY", 100, 90, 95)
    print(f"LONG (Entry=100, TP=90, SL=95): {'✅ Valid' if valid else '❌ Invalid'}")
    
    # Test invalid SHORT
    print("\nTesting invalid SHORT:")
    valid = trader.validate_tp_sl("SELL", 100, 105, 95)
    print(f"SHORT (Entry=100, TP=105, SL=95): {'✅ Valid' if valid else '❌ Invalid'}")
    
    print("\n" + "="*50)
