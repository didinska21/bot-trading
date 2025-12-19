# binance_futures.py - Binance Futures API Wrapper (FIXED & ENHANCED VERSION)

import logging
import time
from decimal import Decimal, ROUND_DOWN

from binance.client import Client
from binance.exceptions import BinanceAPIException

from config import BINANCE_API_KEY, BINANCE_API_SECRET

# ============================================================
# LOGGING
# ============================================================

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
                        "available": float(asset["availableBalance"]),
                        "availableBalance": float(asset["availableBalance"]),
                        "unrealized_pnl": float(asset["unrealizedProfit"]),
                        "crossWalletBalance": float(asset.get("crossWalletBalance", 0)),
                        "marginBalance": float(asset.get("marginBalance", 0)),
                    }
                    logger.info(f"📊 Balance: ${balance_data['available']:.2f}")
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
                    if s["status"] != "TRADING":
                        logger.warning(f"⚠️ Symbol {symbol} status: {s['status']}")
                        return None

                    filters = {f["filterType"]: f for f in s["filters"]}

                    min_notional = 5.0
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
            return float(quantity - (quantity % step_size))
        except Exception as e:
            logger.error(f"❌ Error rounding step size: {e}")
            return quantity

    def round_price(self, price, tick_size):
        """Round price to valid tick size"""
        try:
            price = Decimal(str(price))
            tick_size = Decimal(str(tick_size))
            return float(price.quantize(tick_size, rounding=ROUND_DOWN))
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
            if e.code == -4028:
                logger.info(f"ℹ️ Leverage already at {leverage}x for {symbol}")
                return True
            logger.error(f"❌ Error setting leverage: {e.message} (Code: {e.code})")
            return None
        except Exception as e:
            logger.error(f"❌ Error setting leverage: {e}")
            return None

    def set_margin_type(self, symbol, margin_type="ISOLATED"):
        """Set margin type"""
        try:
            result = self.client.futures_change_margin_type(
                symbol=symbol,
                marginType=margin_type
            )
            logger.info(f"✅ Margin type set to {margin_type} for {symbol}")
            return result
        except BinanceAPIException as e:
            if e.code == -4046:
                logger.info(f"ℹ️ Margin type already {margin_type}")
                return True
            logger.error(f"❌ Error setting margin type: {e.message}")
            return None
        except Exception as e:
            logger.error(f"❌ Error setting margin type: {e}")
            return None

    # ============================================================
    # ========================= QUANTITY =========================
    # ============================================================

    def calculate_quantity(self, symbol, entry_price, position_size_usd):
        """Calculate valid quantity"""
        try:
            info = self.get_symbol_info(symbol)
            if not info:
                return None

            quantity = position_size_usd / entry_price
            quantity = self.round_step_size(quantity, info["step_size"])

            if quantity < info["min_qty"] or quantity > info["max_qty"]:
                return None

            notional = quantity * entry_price
            if notional < info["min_notional"]:
                return None

            return quantity

        except Exception as e:
            logger.error(f"❌ Error calculating quantity: {e}")
            return None

    # ============================================================
    # ========================= VALIDATION =======================
    # ============================================================

    def validate_tp_sl(self, side, entry, tp, sl):
        """Validate TP/SL levels"""
        try:
            if side == "BUY":
                if tp <= entry or sl >= entry:
                    return False
            else:
                if tp >= entry or sl <= entry:
                    return False
            return True
        except Exception as e:
            logger.error(f"❌ Error validating TP/SL: {e}")
            return False

    # ============================================================
    # ========================= ORDER MANAGEMENT =================
    # ============================================================

    def cancel_order(self, symbol, order_id):
        try:
            return self.client.futures_cancel_order(
                symbol=symbol,
                orderId=order_id
            )
        except Exception as e:
            logger.error(f"❌ Error cancelling order: {e}")
            return None

    def cancel_all_orders(self, symbol):
        try:
            return self.client.futures_cancel_all_open_orders(symbol=symbol)
        except Exception as e:
            logger.error(f"❌ Error cancelling all orders: {e}")
            return None

    def get_order_status(self, symbol, order_id):
        try:
            return self.client.futures_get_order(
                symbol=symbol,
                orderId=order_id
            )
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
        entry_order = None
        tp_order = None
        sl_order = None

        try:
            self.set_leverage(symbol, leverage)
            self.set_margin_type(symbol, "ISOLATED")

            info = self.get_symbol_info(symbol)
            if not info:
                return {"success": False}

            entry_price = self.round_price(entry_price, info["tick_size"])
            tp_price = self.round_price(tp_price, info["tick_size"])
            sl_price = self.round_price(sl_price, info["tick_size"])

            if not self.validate_tp_sl(side, entry_price, tp_price, sl_price):
                return {"success": False}

            if order_type == "MARKET":
                entry_order = self.client.futures_create_order(
                    symbol=symbol,
                    side=side,
                    type="MARKET",
                    quantity=quantity
                )
            else:
                entry_order = self.client.futures_create_order(
                    symbol=symbol,
                    side=side,
                    type="LIMIT",
                    timeInForce="GTC",
                    quantity=quantity,
                    price=entry_price
                )

            tp_sl_side = "SELL" if side == "BUY" else "BUY"

            tp_order = self.client.futures_create_order(
                symbol=symbol,
                side=tp_sl_side,
                type="TAKE_PROFIT_MARKET",
                stopPrice=tp_price,
                closePosition=True
            )

            sl_order = self.client.futures_create_order(
                symbol=symbol,
                side=tp_sl_side,
                type="STOP_MARKET",
                stopPrice=sl_price,
                closePosition=True
            )

            return {
                "success": True,
                "entry_order": entry_order,
                "tp_order": tp_order,
                "sl_order": sl_order
            }

        except Exception as e:
            logger.error(f"❌ Error placing futures order: {e}")
            return {"success": False, "error": str(e)}

    # ============================================================
    # ========================= POSITION MANAGEMENT ==============
    # ============================================================

    def get_open_positions(self):
        try:
            positions = self.client.futures_position_information()
            return [p for p in positions if float(p["positionAmt"]) != 0]
        except Exception as e:
            logger.error(f"❌ Error getting positions: {e}")
            return []

    def close_position(self, symbol, quantity=None):
        try:
            positions = self.get_open_positions()
            pos = next((p for p in positions if p["symbol"] == symbol), None)
            if not pos:
                return None

            side = "SELL" if float(pos["positionAmt"]) > 0 else "BUY"
            qty = abs(float(pos["positionAmt"])) if not quantity else quantity

            return self.client.futures_create_order(
                symbol=symbol,
                side=side,
                type="MARKET",
                quantity=qty,
                reduceOnly=True
            )
        except Exception as e:
            logger.error(f"❌ Error closing position: {e}")
            return None


# ============================================================
# ========================= TESTING ==========================
# ============================================================

if __name__ == "__main__":
    trader = BinanceFuturesTrader()
    print(trader.get_futures_balance())
