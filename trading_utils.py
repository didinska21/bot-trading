# trading_utils.py - Trading Utility Functions
import logging
from datetime import datetime

logger = logging.getLogger(__name__)

def validate_price_levels_util(entry_price: float, tp_price: float, sl_price: float, side: str) -> tuple:
    """
    Validate if price levels are correct for LONG/SHORT
    Returns: (is_valid: bool, error_message: str)
    """
    try:
        if side == "BUY":  # LONG
            # For LONG: TP must be above entry, SL must be below entry
            if tp_price <= entry_price:
                return False, f"TP (${tp_price:.4f}) must be above entry (${entry_price:.4f}) for LONG"
            if sl_price >= entry_price:
                return False, f"SL (${sl_price:.4f}) must be below entry (${entry_price:.4f}) for LONG"
        
        else:  # SELL (SHORT)
            # For SHORT: TP must be below entry, SL must be above entry
            if tp_price >= entry_price:
                return False, f"TP (${tp_price:.4f}) must be below entry (${entry_price:.4f}) for SHORT"
            if sl_price <= entry_price:
                return False, f"SL (${sl_price:.4f}) must be above entry (${entry_price:.4f}) for SHORT"
        
        return True, ""
    
    except Exception as e:
        return False, f"Validation error: {str(e)}"


def calculate_pnl_percentage(entry_price: float, exit_price: float, side: str) -> float:
    """
    Calculate PnL percentage
    Returns positive for profit, negative for loss
    """
    if side == "BUY":  # LONG
        return ((exit_price - entry_price) / entry_price) * 100
    else:  # SHORT
        return ((entry_price - exit_price) / entry_price) * 100


def calculate_risk_reward(entry_price: float, tp_price: float, sl_price: float, side: str) -> float:
    """
    Calculate Risk/Reward ratio
    """
    profit_pct = abs(calculate_pnl_percentage(entry_price, tp_price, side))
    loss_pct = abs(calculate_pnl_percentage(entry_price, sl_price, side))
    
    if loss_pct == 0:
        return 0
    
    return profit_pct / loss_pct


def format_currency(amount: float) -> str:
    """Format currency with proper decimals"""
    if amount < 0.01:
        return f"${amount:.8f}"
    elif amount < 1:
        return f"${amount:.6f}"
    elif amount < 100:
        return f"${amount:.4f}"
    else:
        return f"${amount:.2f}"


def format_percentage(pct: float) -> str:
    """Format percentage with sign"""
    sign = "+" if pct > 0 else ""
    return f"{sign}{pct:.2f}%"


def format_error_message(error: Exception) -> str:
    """Format error message untuk display"""
    error_str = str(error)
    
    # Common error patterns
    if "insufficient" in error_str.lower():
        return "⚠️ <b>Balance tidak cukup</b>\nTransfer dana ke Futures wallet terlebih dahulu."
    elif "invalid symbol" in error_str.lower():
        return "⚠️ <b>Symbol tidak valid</b>\nPastikan symbol tersedia di Binance Futures."
    elif "price" in error_str.lower() and "level" in error_str.lower():
        return f"⚠️ <b>Price level tidak valid</b>\n{error_str}"
    elif "permission" in error_str.lower() or "api" in error_str.lower():
        return "⚠️ <b>API key tidak memiliki akses</b>\nPastikan futures trading enabled."
    elif "network" in error_str.lower() or "timeout" in error_str.lower():
        return "⚠️ <b>Koneksi bermasalah</b>\nSilakan coba lagi."
    else:
        return f"Error: {error_str}"


def log_user_action(user, action: str, details: str = ""):
    """Log user actions for monitoring"""
    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    user_id = user.id if user else "Unknown"
    username = user.username if user and user.username else "NoUsername"
    
    log_msg = f"[{timestamp}] User {user_id} (@{username}) | {action} {details}"
    
    if "ERROR" in action or "FAILED" in action:
        logger.error(log_msg)
    elif "SUCCESS" in action:
        logger.info(f"✅ {log_msg}")
    else:
        logger.info(log_msg)


async def safe_edit_message(query, text: str, reply_markup=None):
    """Safely edit message with error handling"""
    try:
        await query.edit_message_text(
            text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
    except Exception as e:
        logger.error(f"Failed to edit message: {e}")
        # Try to send new message if edit fails
        try:
            await query.message.reply_text(
                text,
                parse_mode="HTML",
                reply_markup=reply_markup
            )
        except:
            pass
