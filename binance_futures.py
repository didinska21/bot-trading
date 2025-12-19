# binance_futures.py - Binance Futures API Wrapper (FIXED VERSION)

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
        self.client = Client(
            api_key=BINANCE_API_KEY,
            api_secret=BINANCE_API_SECRET
        )
        self.client.API_URL = "https://fapi.binance.com/fapi"

    # ============================================================
    # ========================= BALANCE ==========================
    # ============================================================

    def get_futures_balance(self):
        """Get USDT balance in Futures wallet"""
        try:
            account = self.client.futures_account()
            for asset in account["assets"]:
                if asset["asset"] == "USDT":
                    return {
                        "balance": float(asset["walletBalance"]),
                        "availableBalance": float(asset["availableBalance"]),
                        "unrealized_pnl": float(asset["unrealizedProfit"]),
                    }
            return None
        except BinanceAPIException as e:
            logger.error(f"Error getting futures balance: {e}")
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
                    filters = {f["filterType"]: f for f in s["filters"]}
                    return {
                        "symbol": symbol,
                        "status": s["status"],
                        "price_precision": s["pricePrecision"],
                        "quantity_precision": s["quantityPrecision"],
                        "min_qty": float(filters["LOT_SIZE"]["minQty"]),
                        "max_qty": float(filters["LOT_SIZE"]["maxQty"]),
                        "step_size": float(filters["LOT_SIZE"]["stepSize"]),
                        "min_notional": float(
                            filters["MIN_NOTIONAL"]["notional"]
                        ) if "MIN_NOTIONAL" in filters else 5.0,
                        "tick_size": float(filters["PRICE_FILTER"]["tickSize"]),
                    }
            return None
        except Exception as e:
            logger.error(f"Error getting symbol info: {e}")
            return None

    # ============================================================
    # ========================= ROUNDING =========================
    # ============================================================

    def round_step_size(self, quantity, step_size):
        quantity = Decimal(str(quantity))
        step_size = Decimal(str(step_size))
        return float(quantity - (quantity % step_size))

    def round_price(self, price, tick_size):
        price = Decimal(str(price))
        tick_size = Decimal(str(tick_size))
        return float(price.quantize(tick_size, rounding=ROUND_DOWN))

    # ============================================================
    # ========================= LEVERAGE =========================
    # ============================================================

    def set_leverage(self, symbol, leverage):
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

    def set_margin_type(self, symbol, margin_type="ISOLATED"):
        try:
            result = self.client.futures_change_margin_type(
                symbol=symbol,
                marginType=margin_type
            )
            logger.info(f"Margin type set to {margin_type} for {symbol}")
            return result
        except BinanceAPIException as e:
            if e.code == -4046:
                logger.info(f"Margin type already {margin_type} for {symbol}")
                return True
            logger.error(f"Error setting margin type: {e}")
            return None

    # ============================================================
    # ========================= QUANTITY =========================
    # ============================================================

    def calculate_quantity(self, symbol, entry_price, position_size_usd):
        try:
            info = self.get_symbol_info(symbol)
            if not info:
                return None

            quantity = position_size_usd / entry_price
            quantity = self.round_step_size(quantity, info["step_size"])

            if quantity < info["min_qty"]:
                logger.error("Quantity below minimum")
                return None

            if quantity * entry_price < info["min_notional"]:
                logger.error("Notional below minimum")
                return None

            return quantity
        except Exception as e:
            logger.error(f"Error calculating quantity: {e}")
            return None

    # ============================================================
    # ========================= VALIDATION =======================
    # ============================================================

    def validate_tp_sl(self, side, entry, tp, sl):
        if side == "BUY":
            return tp > entry and sl < entry
        return tp < entry and sl > entry

    # ============================================================
    # ========================= ORDER ============================
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
    ):
        try:
            self.set_leverage(symbol, leverage)
            self.set_margin_type(symbol, "ISOLATED")

            info = self.get_symbol_info(symbol)
            if not info:
                return {"success": False, "error": "Symbol info not found"}

            entry_price = self.round_price(entry_price, info["tick_size"])
            tp_price = self.round_price(tp_price, info["tick_size"])
            sl_price = self.round_price(sl_price, info["tick_size"])

            if not self.validate_tp_sl(side, entry_price, tp_price, sl_price):
                return {"success": False, "error": "Invalid TP/SL configuration"}

            entry_order = self.client.futures_create_order(
                symbol=symbol,
                side=side,
                type="LIMIT",
                timeInForce="GTC",
                quantity=quantity,
                price=entry_price
            )

            if "orderId" not in entry_order:
                return {"success": False, "error": "Entry order failed"}

            tp_side = "SELL" if side == "BUY" else "BUY"

            tp_order = self.client.futures_create_order(
                symbol=symbol,
                side=tp_side,
                type="TAKE_PROFIT_MARKET",
                stopPrice=tp_price,
                closePosition=True,
                reduceOnly=True
            )

            sl_order = self.client.futures_create_order(
                symbol=symbol,
                side=tp_side,
                type="STOP_MARKET",
                stopPrice=sl_price,
                closePosition=True,
                reduceOnly=True
            )

            return {
                "success": True,
                "entry_order": entry_order,
                "tp_order": tp_order,
                "sl_order": sl_order,
            }

        except BinanceAPIException as e:
            logger.error(f"Binance API Error: {e}")
            return {"success": False, "error": str(e)}
        except Exception as e:
            logger.error(f"Error placing futures order: {e}")
            return {"success": False, "error": str(e)}

    # ============================================================
    # ========================= POSITION =========================
    # ============================================================

    def get_open_positions(self):
        try:
            positions = self.client.futures_position_information()
            open_positions = []

            for pos in positions:
                amt = float(pos["positionAmt"])
                if amt != 0:
                    open_positions.append({
                        "symbol": pos["symbol"],
                        "side": "LONG" if amt > 0 else "SHORT",
                        "quantity": abs(amt),
                        "entry_price": float(pos["entryPrice"]),
                        "mark_price": float(pos["markPrice"]),
                        "unrealized_pnl": float(pos["unRealizedProfit"]),
                        "leverage": int(pos["leverage"]),
                        "liquidation_price": float(pos["liquidationPrice"]) if pos["liquidationPrice"] else 0,
                    })

            return open_positions
        except Exception as e:
            logger.error(f"Error getting open positions: {e}")
            return []

    def close_position(self, symbol):
        try:
            positions = self.get_open_positions()
            pos = next((p for p in positions if p["symbol"] == symbol), None)
            if not pos:
                return None

            side = "SELL" if pos["side"] == "LONG" else "BUY"

            return self.client.futures_create_order(
                symbol=symbol,
                side=side,
                type="MARKET",
                quantity=pos["quantity"],
                reduceOnly=True
            )
        except Exception as e:
            logger.error(f"Error closing position: {e}")
            return None

    # ============================================================
    # ========================= PRICE ============================
    # ============================================================

    def get_current_price(self, symbol):
        try:
            ticker = self.client.futures_symbol_ticker(symbol=symbol)
            return float(ticker["price"])
        except Exception as e:
            logger.error(f"Error getting price: {e}")
            return None
