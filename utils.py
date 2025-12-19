# utils.py - ENHANCED VERSION with Better Formatting & Error Handling

import re
import logging
from telegram import Update
from telegram.ext import ContextTypes
from config import ALLOWED_USER_IDS
from functools import wraps
from datetime import datetime

logger = logging.getLogger(__name__)


# ============================================================
# ========================= NUMBER FORMATTING ================
# ============================================================

def format_number(val):
    """
    Format angka untuk display yang lebih readable
    Handles various edge cases
    """
    try:
        # Handle None
        if val is None:
            return "N/A"
        
        # Convert to float
        val = float(val)
        
        # Handle infinity and NaN
        if not (-float('inf') < val < float('inf')):
            return "∞" if val > 0 else "-∞"
        
        # Handle very small numbers (scientific notation)
        if abs(val) < 0.00000001 and val != 0:
            return f"{val:.2e}"
        
        # Format based on magnitude
        if abs(val) < 0.01:
            formatted = f"{val:.8f}".rstrip("0").rstrip(".")
        elif abs(val) < 1:
            formatted = f"{val:.6f}".rstrip("0").rstrip(".")
        elif abs(val) < 100:
            formatted = f"{val:.4f}".rstrip("0").rstrip(".")
        elif abs(val) < 10000:
            formatted = f"{val:.2f}".rstrip("0").rstrip(".")
        else:
            # Add thousands separator for large numbers
            formatted = f"{val:,.2f}".rstrip("0").rstrip(".")
        
        return formatted
        
    except (ValueError, TypeError) as e:
        logger.warning(f"Cannot format value '{val}': {e}")
        return str(val)


def format_percentage(value):
    """Format percentage with proper sign"""
    try:
        val = float(value)
        sign = "+" if val > 0 else ""
        return f"{sign}{val:.2f}%"
    except:
        return "N/A"


def format_currency(value, symbol="$"):
    """Format currency with symbol"""
    try:
        val = float(value)
        formatted = format_number(val)
        return f"{symbol}{formatted}"
    except:
        return "N/A"


def format_large_number(value):
    """Format large numbers with K, M, B suffixes"""
    try:
        val = float(value)
        
        if abs(val) >= 1_000_000_000:
            return f"{val/1_000_000_000:.2f}B"
        elif abs(val) >= 1_000_000:
            return f"{val/1_000_000:.2f}M"
        elif abs(val) >= 1_000:
            return f"{val/1_000:.2f}K"
        else:
            return format_number(val)
    except:
        return str(value)


# ============================================================
# ========================= ACCESS CONTROL ===================
# ============================================================

def only_allowed(func):
    """
    Decorator untuk membatasi akses hanya user yang diizinkan
    dengan logging untuk security monitoring
    """
    @wraps(func)
    async def wrapper(update: Update, context: ContextTypes.DEFAULT_TYPE):
        user = update.effective_user
        if not user:
            logger.warning("Access attempt with no user info")
            return

        user_id = user.id
        username = user.username or "Unknown"
        full_name = user.full_name or "Unknown"
        
        # Check if user is allowed
        if user_id not in ALLOWED_USER_IDS:
            logger.warning(
                f"🚫 Access DENIED - User: {full_name} (@{username}, ID: {user_id})"
            )
            
            if update.message:
                await update.message.reply_text(
                    "⛔ <b>Akses Ditolak</b>\n\n"
                    "Maaf, kamu tidak memiliki akses ke bot ini.\n"
                    "Hubungi admin untuk mendapatkan akses.",
                    parse_mode="HTML"
                )
            elif update.callback_query:
                await update.callback_query.answer(
                    "⛔ Akses ditolak!",
                    show_alert=True
                )
            return

        # Log successful access
        logger.info(
            f"✅ Access GRANTED - User: {full_name} (@{username}, ID: {user_id})"
        )
        
        return await func(update, context)
    
    return wrapper


# ============================================================
# ========================= TEXT FORMATTING ==================
# ============================================================

def escape_html(text: str) -> str:
    """Escape HTML special characters for Telegram"""
    if not text:
        return ""
    
    return (
        text.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )


def format_result_for_telegram(text: str) -> str:
    """
    Format AI result untuk Telegram HTML
    Converts markdown-style formatting to HTML
    FIXED: No more double bold tags
    """
    if not text:
        return ""

    # Function to convert scientific notation to decimal
    def sci_to_decimal(match):
        try:
            return format_number(float(match.group()))
        except Exception:
            return match.group()

    # Convert scientific notation to readable decimal
    text = re.sub(
        r"\b\d+\.\d+e[+-]?\d+\b",
        sci_to_decimal,
        text,
        flags=re.IGNORECASE
    )

    # ✅ STEP 1: Convert **bold** to <b>bold</b> FIRST
    text = re.sub(r'\*\*(.+?)\*\*', r'<b>\1</b>', text)
    
    # ✅ STEP 2: Convert *italic* to <i>italic</i>
    text = re.sub(r'\*(.+?)\*', r'<i>\1</i>', text)
    
    # ✅ STEP 3: Convert __underline__ to <u>underline</u>
    text = re.sub(r'__(.+?)__', r'<u>\1</u>', text)
    
    # ✅ STEP 4: Convert `code` to <code>code</code>
    text = re.sub(r'`(.+?)`', r'<code>\1</code>', text)

    # ✅ STEP 5: Now do specific replacements (no more ** to replace)
    replacements = {
        # Signals - These are now already <b>LONG</b> etc
        "<b>LONG</b>": "<b>🟢 LONG</b>",
        "<b>SHORT</b>": "<b>🔴 SHORT</b>",
        "<b>BUY</b>": "<b>🟢 BUY</b>",
        "<b>SELL</b>": "<b>🔴 SELL</b>",
        "<b>WAIT</b>": "<b>⏸️ WAIT</b>",
        "<b>HOLD</b>": "<b>✋ HOLD</b>",
        
        # Headers
        "<b>FUTURES</b>": "<b>📊 REKOMENDASI SETUP FUTURES</b>",
        "<b>SPOT</b>": "<b>💼 REKOMENDASI SETUP SPOT</b>",
        "<b>Catatan</b>": "<b>📝 CATATAN TAMBAHAN</b>",
        "<b>Note</b>": "<b>📝 CATATAN TAMBAHAN</b>",
        
        # Metrics
        "<b>Risk Reward Ratio</b>": "<b>📈 RISK REWARD RATIO</b>",
        "<b>Confidence Level</b>": "<b>🔍 CONFIDENCE LEVEL</b>",
        "<b>Win Rate</b>": "<b>🎯 WIN RATE</b>",
        "<b>Risk Level</b>": "<b>⚠️ RISK LEVEL</b>",
        
        # Signals
        "<b>Sinyal Aksi</b>": "<b>🔔 SINYAL AKSI</b>",
        "<b>Trading Signal</b>": "<b>🔔 SINYAL TRADING</b>",
        "<b>Recommendation</b>": "<b>💡 REKOMENDASI</b>",
        
        # Analysis sections
        "<b>Analisis tren pasar</b>": "<b><u>📊 ANALISIS TREN PASAR</u></b>",
        "<b>Market Analysis</b>": "<b><u>📊 ANALISIS PASAR</u></b>",
        "<b>Technical Analysis</b>": "<b><u>🔍 ANALISIS TEKNIKAL</u></b>",
        "<b>Fundamental Analysis</b>": "<b><u>📰 ANALISIS FUNDAMENTAL</u></b>",
        
        # Price levels - Add emoji WITHOUT bold wrapper
        "Entry Range": "💰 <b>Entry Range</b>",
        "Entry Price": "💰 <b>Entry Price</b>",
        "Take Profit": "🎯 <b>Take Profit</b>",
        "Stop Loss": "🛑 <b>Stop Loss</b>",
        "Target": "🎯 <b>Target</b>",
    }

    # Apply all replacements
    for old, new in replacements.items():
        text = text.replace(old, new)
    
    # Add line breaks for better readability
    text = text.replace("\n\n", "\n━━━━━━━━━━━━━━━\n")

    return text


# ============================================================
# ========================= TESTING ==========================
# ============================================================

if __name__ == "__main__":
    """Test the fixed formatting"""
    
    print("="*60)
    print("TESTING FIXED FORMAT FUNCTION")
    print("="*60)
    
    # Test case 1: Basic formatting
    print("\n1. Basic Bold/Italic/Code:")
    sample1 = "This is **bold**, *italic*, and `code`"
    print(f"   Input:  {sample1}")
    print(f"   Output: {format_result_for_telegram(sample1)}")
    
    # Test case 2: Trading signals
    print("\n2. Trading Signals:")
    sample2 = "**LONG** at Entry Range: $100-$105\nTake Profit: $120\nStop Loss: $95"
    print(f"   Input:  {sample2}")
    print(f"   Output: {format_result_for_telegram(sample2)}")
    
    # Test case 3: Multiple signals
    print("\n3. Multiple Signals:")
    sample3 = """
**LONG** position recommended
Entry Range: $50000-$51000
Take Profit: $55000
Stop Loss: $48000
**Risk Reward Ratio**: 1:2.5
**Confidence Level**: High
"""
    print(f"   Input:  {sample3.strip()}")
    print(f"   Output: {format_result_for_telegram(sample3)}")
    
    # Test case 4: SHORT signal
    print("\n4. SHORT Signal:")
    sample4 = "**SHORT** at Entry Price: $100\nTake Profit: $90\nStop Loss: $105"
    print(f"   Input:  {sample4}")
    print(f"   Output: {format_result_for_telegram(sample4)}")
    
    print("\n" + "="*60)
    print("✅ All formatting tests completed!")
    print("="*60)


def clean_ai_response(text: str) -> str:
    """
    Clean and normalize AI response
    Remove excessive whitespace and normalize formatting
    """
    if not text:
        return ""
    
    # Remove excessive newlines (more than 2)
    text = re.sub(r'\n{3,}', '\n\n', text)
    
    # Remove trailing whitespace from each line
    lines = [line.rstrip() for line in text.split('\n')]
    text = '\n'.join(lines)
    
    # Remove leading/trailing whitespace from entire text
    text = text.strip()
    
    return text


# ============================================================
# ========================= TIME FORMATTING ==================
# ============================================================

def format_timeframe(timeframe: str) -> str:
    """Format timeframe untuk display"""
    timeframe_map = {
        "15m": "15 Menit",
        "1h": "1 Jam",
        "4h": "4 Jam",
        "1d": "1 Hari",
        "1w": "1 Minggu",
        "1M": "1 Bulan"
    }
    return timeframe_map.get(timeframe, timeframe)


def format_timestamp(timestamp=None):
    """Format timestamp untuk display"""
    if timestamp is None:
        timestamp = datetime.now()
    elif isinstance(timestamp, (int, float)):
        timestamp = datetime.fromtimestamp(timestamp)
    
    return timestamp.strftime("%Y-%m-%d %H:%M:%S")


# ============================================================
# ========================= TRADING CALCULATIONS =============
# ============================================================

def calculate_pnl_percentage(entry_price, exit_price, side="LONG"):
    """Calculate PnL percentage"""
    try:
        entry = float(entry_price)
        exit = float(exit_price)
        
        if side.upper() == "LONG":
            pnl_pct = ((exit - entry) / entry) * 100
        else:  # SHORT
            pnl_pct = ((entry - exit) / entry) * 100
        
        return pnl_pct
    except:
        return 0.0


def calculate_risk_reward(entry, tp, sl, side="LONG"):
    """Calculate risk/reward ratio"""
    try:
        entry = float(entry)
        tp = float(tp)
        sl = float(sl)
        
        if side.upper() == "LONG":
            reward = abs(tp - entry)
            risk = abs(entry - sl)
        else:  # SHORT
            reward = abs(entry - tp)
            risk = abs(sl - entry)
        
        if risk == 0:
            return 0
        
        return reward / risk
    except:
        return 0.0


def calculate_position_size(balance, risk_percentage, entry, sl):
    """
    Calculate position size based on risk management
    
    Args:
        balance: Available balance
        risk_percentage: % of balance to risk (e.g., 1 = 1%)
        entry: Entry price
        sl: Stop loss price
    
    Returns:
        Position size in quote currency
    """
    try:
        balance = float(balance)
        risk_pct = float(risk_percentage)
        entry = float(entry)
        sl = float(sl)
        
        # Amount willing to risk
        risk_amount = balance * (risk_pct / 100)
        
        # Price distance to stop loss
        price_distance = abs(entry - sl)
        
        # Position size
        if price_distance == 0:
            return 0
        
        position_size = (risk_amount / price_distance) * entry
        
        return position_size
    except:
        return 0.0


def validate_price_levels(entry, tp, sl, side="LONG"):
    """
    Validate that TP and SL are correctly positioned relative to entry
    
    Returns:
        (is_valid, error_message)
    """
    try:
        entry = float(entry)
        tp = float(tp)
        sl = float(sl)
        
        if side.upper() == "LONG":
            if tp <= entry:
                return False, "TP must be above entry for LONG"
            if sl >= entry:
                return False, "SL must be below entry for LONG"
        else:  # SHORT
            if tp >= entry:
                return False, "TP must be below entry for SHORT"
            if sl <= entry:
                return False, "SL must be above entry for SHORT"
        
        return True, "Valid"
    except Exception as e:
        return False, f"Validation error: {str(e)}"


# ============================================================
# ========================= SYMBOL HELPERS ===================
# ============================================================

def normalize_symbol(symbol: str) -> str:
    """Normalize symbol format (add USDT if needed)"""
    symbol = symbol.upper().strip()
    
    if not symbol.endswith("USDT"):
        symbol += "USDT"
    
    return symbol


def extract_base_currency(symbol: str) -> str:
    """Extract base currency from trading pair (e.g., BTCUSDT -> BTC)"""
    return symbol.replace("USDT", "").replace("BUSD", "").replace("USD", "")


# ============================================================
# ========================= ERROR FORMATTING =================
# ============================================================

def format_error_message(error, context=""):
    """Format error message for user display"""
    error_msg = str(error)
    
    # Common error patterns and user-friendly messages
    error_patterns = {
        "APIError(code=-1121)": "❌ Symbol tidak valid atau tidak tersedia",
        "APIError(code=-2015)": "❌ API key tidak valid atau expired",
        "APIError(code=-1003)": "⚠️ Terlalu banyak request, coba lagi sebentar",
        "APIError(code=-4046)": "ℹ️ Margin type sudah sesuai",
        "Timeout": "⏱️ Request timeout, coba lagi",
        "Connection": "🔌 Koneksi bermasalah, coba lagi",
        "insufficient balance": "💰 Balance tidak cukup",
        "MIN_NOTIONAL": "⚠️ Nilai order terlalu kecil (minimum notional)",
    }
    
    for pattern, friendly_msg in error_patterns.items():
        if pattern in error_msg:
            return f"{friendly_msg}\n\n<i>Detail: {error_msg}</i>"
    
    return f"❌ Error: {error_msg}"


# ============================================================
# ========================= VALIDATION =======================
# ============================================================

def is_valid_timeframe(timeframe: str) -> bool:
    """Check if timeframe is valid"""
    valid_timeframes = ["15m", "1h", "4h", "1d", "1w", "1M"]
    return timeframe in valid_timeframes


def is_valid_side(side: str) -> bool:
    """Check if trading side is valid"""
    valid_sides = ["BUY", "SELL", "LONG", "SHORT"]
    return side.upper() in valid_sides


# ============================================================
# ========================= DEBUG HELPERS ====================
# ============================================================

def log_user_action(user, action, details=""):
    """Log user action for monitoring"""
    username = user.username or "Unknown"
    user_id = user.id
    full_name = user.full_name or "Unknown"
    
    logger.info(
        f"👤 User Action - {full_name} (@{username}, ID: {user_id}) - "
        f"Action: {action} {details}"
    )


def format_dict_for_log(data: dict, indent=2) -> str:
    """Format dictionary for readable logging"""
    import json
    try:
        return json.dumps(data, indent=indent, ensure_ascii=False)
    except:
        return str(data)


# ============================================================
# ========================= TESTING ==========================
# ============================================================

if __name__ == "__main__":
    """Test utility functions"""
    
    print("="*50)
    print("TESTING UTILS.PY")
    print("="*50)
    
    # Test 1: Number formatting
    print("\n1. Number Formatting:")
    print(f"   0.00000123 -> {format_number(0.00000123)}")
    print(f"   1234567.89 -> {format_number(1234567.89)}")
    print(f"   0.5 -> {format_number(0.5)}")
    print(f"   None -> {format_number(None)}")
    
    # Test 2: Currency formatting
    print("\n2. Currency Formatting:")
    print(f"   1234.56 -> {format_currency(1234.56)}")
    print(f"   0.00123 -> {format_currency(0.00123)}")
    
    # Test 3: Large number formatting
    print("\n3. Large Number Formatting:")
    print(f"   1500000 -> {format_large_number(1500000)}")
    print(f"   2500000000 -> {format_large_number(2500000000)}")
    
    # Test 4: PnL calculation
    print("\n4. PnL Calculation:")
    pnl = calculate_pnl_percentage(100, 110, "LONG")
    print(f"   LONG: Entry 100, Exit 110 -> {pnl:.2f}%")
    pnl = calculate_pnl_percentage(100, 90, "SHORT")
    print(f"   SHORT: Entry 100, Exit 90 -> {pnl:.2f}%")
    
    # Test 5: Risk/Reward
    print("\n5. Risk/Reward Ratio:")
    rr = calculate_risk_reward(100, 110, 95, "LONG")
    print(f"   LONG: Entry 100, TP 110, SL 95 -> {rr:.2f}")
    
    # Test 6: Price validation
    print("\n6. Price Level Validation:")
    valid, msg = validate_price_levels(100, 110, 95, "LONG")
    print(f"   LONG (100/110/95): {valid} - {msg}")
    valid, msg = validate_price_levels(100, 90, 95, "LONG")
    print(f"   LONG (100/90/95): {valid} - {msg}")
    
    # Test 7: Symbol normalization
    print("\n7. Symbol Normalization:")
    print(f"   'btc' -> {normalize_symbol('btc')}")
    print(f"   'ETHUSDT' -> {normalize_symbol('ETHUSDT')}")
    
    # Test 8: AI text formatting
    print("\n8. AI Text Formatting:")
    sample = "**LONG** at Entry Range: $100-$105\n**Take Profit**: $120\n**Stop Loss**: $95"
    formatted = format_result_for_telegram(sample)
    print(f"   Original: {sample}")
    print(f"   Formatted: {formatted}")
    
    print("\n" + "="*50)
    print("All tests completed!")
    print("="*50)
