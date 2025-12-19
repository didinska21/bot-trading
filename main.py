# main.py - COMPLETE UPDATED VERSION (PART 1/3)
# Bot Telegram untuk Crypto Trading Signal dengan AI

import logging
import asyncio
from telegram import Update, InlineKeyboardButton, InlineKeyboardMarkup
from telegram.ext import (
    ApplicationBuilder, CommandHandler, CallbackQueryHandler,
    MessageHandler, ConversationHandler, ContextTypes, filters
)
from binance.client import Client
from binance.exceptions import BinanceAPIException

from config import TELEGRAM_TOKEN, BINANCE_API_KEY, BINANCE_API_SECRET, TOP_TOKENS

# ⭐ UPDATED IMPORTS - Menggunakan fungsi baru dari utils.py
from utils import (
    only_allowed, 
    format_result_for_telegram,
    format_currency,
    format_percentage,
    calculate_pnl_percentage,
    calculate_risk_reward,
    validate_price_levels as validate_price_levels_util,
    normalize_symbol,
    format_error_message,
    log_user_action,
    format_number
)

from ai import analyze_with_gpt
from shared_state import SharedState

# Setup logging
logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)
logger = logging.getLogger(__name__)

# Initialize Binance client
binance_client = Client(
    api_key=BINANCE_API_KEY,
    api_secret=BINANCE_API_SECRET
)

# Shared state for auto trading monitoring
shared_state = SharedState()

# Conversation states
SELECTING_MODE, SELECTING_TOKEN, SELECTING_STRATEGY = range(3)

# Binance timeframe mapping
TIMEFRAME_MAP = {
    "15m": Client.KLINE_INTERVAL_15MINUTE,
    "1h": Client.KLINE_INTERVAL_1HOUR,
    "4h": Client.KLINE_INTERVAL_4HOUR,
    "1d": Client.KLINE_INTERVAL_1DAY,
    "1w": Client.KLINE_INTERVAL_1WEEK,
}


# ============================================================
# ===================== HELPER FUNCTIONS =====================
# ============================================================

def validate_symbol(symbol: str) -> bool:
    """Validate if symbol exists on Binance"""
    try:
        binance_client.get_symbol_ticker(symbol=symbol)
        return True
    except BinanceAPIException:
        return False


async def safe_edit_message(query, text, parse_mode="HTML", reply_markup=None):
    """Safely edit message with error handling"""
    try:
        await query.edit_message_text(
            text,
            parse_mode=parse_mode,
            reply_markup=reply_markup
        )
    except Exception as e:
        logger.error(f"Failed to edit message: {e}")
        try:
            await query.message.reply_text(
                text, 
                parse_mode=parse_mode, 
                reply_markup=reply_markup
            )
        except Exception as e2:
            logger.error(f"Failed to send fallback message: {e2}")


# ============================================================
# ================= FUTURES EXECUTION ========================
# ============================================================

@only_allowed
async def handle_execute_confirm(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Execute futures order after confirmation - FIXED VERSION"""
    query = update.callback_query
    await query.answer()
    
    user = update.effective_user

    analysis = context.user_data.get("analysis")
    if not analysis:
        log_user_action(user, "EXECUTE_FAILED", "- No analysis data")
        await query.answer("❌ Data tidak ditemukan! Silakan analisis ulang.", show_alert=True)
        return await start(update, context)

    log_user_action(
        user, 
        "EXECUTE_FUTURES_START", 
        f"- {analysis['symbol']} {analysis['mode'].upper()}"
    )

    await safe_edit_message(
        query,
        "⏳ <b>Memasang posisi...</b>\n\nMohon tunggu...",
    )

    try:
        from binance_futures import BinanceFuturesTrader
        import re

        futures_trader = BinanceFuturesTrader()

        ai_result = analysis["ai_result"]
        current_price = analysis["current_price"]

        # Validate AI response
        if not ai_result or len(ai_result.strip()) < 20:
            raise Exception("AI response tidak valid atau terlalu pendek")

        # Detect LONG / SHORT
        ai_upper = ai_result.upper()
        if "LONG" in ai_upper:
            side = "BUY"
        elif "SHORT" in ai_upper:
            side = "SELL"
        else:
            await safe_edit_message(
                query,
                "❌ <b>AI Menyarankan WAIT</b>\n\n"
                "Tidak ada sinyal trading yang jelas.\n"
                "Silakan coba analisis ulang atau pilih timeframe lain.",
                reply_markup=InlineKeyboardMarkup([
                    [InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{analysis['timeframe']}")],
                    [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
                ])
            )
            log_user_action(user, "EXECUTE_SKIPPED", "- AI suggests WAIT")
            return SELECTING_MODE

        # ✅ FIXED: Parse entry price with better logic
        entry_match = re.search(
            r'Entry\s*(?:Range|Price)?.*?\$?\s*([\d.,]+)\s*(?:-|to)?\s*\$?\s*([\d.,]+)?',
            ai_result,
            re.IGNORECASE
        )
        if entry_match:
            entry_low = float(entry_match.group(1).replace(',', ''))
            entry_high = float(entry_match.group(2).replace(',', '')) if entry_match.group(2) else entry_low
            entry_price = (entry_low + entry_high) / 2
        else:
            # Fallback: use current price with small buffer
            entry_price = current_price * (0.998 if side == 'BUY' else 1.002)

        # ✅ FIXED: Parse Take Profit - Better regex
        tp_matches = re.findall(
            r'(?:TP|Take\s*Profit)\s*\d*\s*[:=]?\s*\$?\s*([\d.,]+)', 
            ai_result, 
            re.IGNORECASE
        )
        
        if tp_matches and len(tp_matches) >= 1:
            # Use TP2 if available, otherwise TP1
            tp_raw = tp_matches[1] if len(tp_matches) >= 2 else tp_matches[0]
            tp_price = float(tp_raw.replace(',', ''))
        else:
            # ✅ FIXED: Fallback dengan logika yang benar untuk SHORT
            if side == 'BUY':  # LONG
                tp_price = entry_price * 1.03  # TP 3% di atas entry
            else:  # SHORT
                tp_price = entry_price * 0.97  # TP 3% di bawah entry

        # ✅ FIXED: Parse Stop Loss - Better regex
        sl_match = re.search(
            r'(?:Stop\s*Loss|SL)\s*[:=]?\s*\$?\s*([\d.,]+)', 
            ai_result, 
            re.IGNORECASE
        )
        
        if sl_match:
            sl_price = float(sl_match.group(1).replace(',', ''))
        else:
            # ✅ FIXED: Fallback dengan logika yang benar untuk SHORT
            if side == 'BUY':  # LONG
                sl_price = entry_price * 0.98  # SL 2% di bawah entry
            else:  # SHORT
                sl_price = entry_price * 1.02  # SL 2% di atas entry

        # ✅ ADDITIONAL FIX: Swap TP/SL jika masih terbalik
        if side == 'BUY':  # LONG
            # TP harus di atas entry, SL harus di bawah entry
            if tp_price < entry_price or sl_price > entry_price:
                logger.warning(f"Detected inverted TP/SL for LONG. Swapping...")
                tp_price, sl_price = max(tp_price, sl_price), min(tp_price, sl_price)
                tp_price = max(tp_price, entry_price * 1.01)  # Ensure TP > entry
                sl_price = min(sl_price, entry_price * 0.99)  # Ensure SL < entry
        else:  # SHORT
            # TP harus di bawah entry, SL harus di atas entry
            if tp_price > entry_price or sl_price < entry_price:
                logger.warning(f"Detected inverted TP/SL for SHORT. Swapping...")
                tp_price, sl_price = min(tp_price, sl_price), max(tp_price, sl_price)
                tp_price = min(tp_price, entry_price * 0.99)  # Ensure TP < entry
                sl_price = max(sl_price, entry_price * 1.01)  # Ensure SL > entry

        # ✅ Validate price levels
        is_valid, error_msg = validate_price_levels_util(entry_price, tp_price, sl_price, side)
        if not is_valid:
            raise Exception(f"Invalid price levels: {error_msg}")

        # Log parsed values for debugging
        logger.info(f"Parsed prices - Entry: ${entry_price:.2f}, TP: ${tp_price:.2f}, SL: ${sl_price:.2f}, Side: {side}")

        # Get balance
        balance = futures_trader.get_futures_balance()
        if not balance or balance.get("availableBalance", 0) <= 0:
            raise Exception("Insufficient balance atau gagal mendapatkan balance")

        # Calculate position size (15% of available balance)
        position_size = balance["availableBalance"] * 0.15

        # Calculate quantity
        quantity = futures_trader.calculate_quantity(
            analysis["symbol"],
            entry_price,
            position_size
        )
        if not quantity or quantity <= 0:
            raise Exception("Gagal menghitung quantity atau quantity terlalu kecil")

        # Place order
        result = futures_trader.place_futures_order(
            symbol=analysis["symbol"],
            side=side,
            quantity=quantity,
            entry_price=entry_price,
            tp_price=tp_price,
            sl_price=sl_price,
            leverage=10,
            order_type="MARKET"
        )

        if not result.get("success"):
            raise Exception(result.get("error", "Unknown error from Binance"))

        # Calculate PnL using utility functions
        profit_pct = calculate_pnl_percentage(entry_price, tp_price, side)
        loss_pct = abs(calculate_pnl_percentage(entry_price, sl_price, side))
        rr = calculate_risk_reward(entry_price, tp_price, sl_price, side)

        # Calculate potential profit/loss with leverage
        leverage = 10
        max_profit = position_size * (profit_pct / 100) * leverage
        max_loss = position_size * (loss_pct / 100) * leverage

        # Success message
        success_msg = f"""
✅ <b>POSISI BERHASIL DIPASANG!</b>

📊 <b>Detail Order:</b>
Symbol: {analysis['symbol']}
Side: <b>{"🟢 LONG" if side == "BUY" else "🔴 SHORT"}</b>
Leverage: {leverage}x

💰 <b>Prices:</b>
Entry: {format_currency(entry_price)}
TP: {format_currency(tp_price)} ({format_percentage(profit_pct)})
SL: {format_currency(sl_price)} ({format_percentage(-loss_pct)})

📦 <b>Position:</b>
Quantity: {quantity}
Size: {format_currency(position_size)}

📈 <b>Risk/Reward:</b>
Ratio: <b>1:{rr:.2f}</b>
Max Profit: <b>{format_currency(max_profit)}</b>
Max Loss: {format_currency(max_loss)}

🎯 <b>Order IDs:</b>
Entry: {result['entry_order']['orderId']}
TP: {result.get('tp_order', {}).get('orderId', 'N/A')}
SL: {result.get('sl_order', {}).get('orderId', 'N/A')}

⚠️ <b>Reminder:</b>
Monitor posisi Anda secara berkala di Binance Futures.
"""

        keyboard = [
            [InlineKeyboardButton("📊 Lihat di Binance", url=f"https://www.binance.com/en/futures/{analysis['symbol']}")],
            [InlineKeyboardButton("📈 Trade Lagi", callback_data=f"mode_{analysis['mode']}")],
            [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
        ]

        await safe_edit_message(
            query,
            success_msg,
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

        log_user_action(
            user, 
            "EXECUTE_SUCCESS", 
            f"- {analysis['symbol']} {side} Entry: ${entry_price:.2f}"
        )

    except Exception as e:
        logger.error(f"Execute futures error: {e}")
        
        error_display = format_error_message(e)
        
        await safe_edit_message(
            query,
            f"""
❌ <b>GAGAL MEMASANG POSISI!</b>

{error_display}

<b>Debug Info:</b>
• Symbol: {analysis.get('symbol', 'N/A')}
• Side: {side if 'side' in locals() else 'N/A'}
• Entry: ${entry_price:.2f if 'entry_price' in locals() else 'N/A'}
• TP: ${tp_price:.2f if 'tp_price' in locals() else 'N/A'}
• SL: ${sl_price:.2f if 'sl_price' in locals() else 'N/A'}

<b>Kemungkinan penyebab:</b>
• Balance tidak cukup
• Symbol tidak tersedia untuk futures
• Price level tidak valid
• Koneksi ke Binance bermasalah

Silakan cek balance dan coba lagi.
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data=f"strategy_{analysis['timeframe']}")],
                [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
            ])
        )
        
        log_user_action(user, "EXECUTE_ERROR", f"- {str(e)}")

    return SELECTING_MODE


@only_allowed
async def handle_execute_cancel(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Cancel futures order execution"""
    query = update.callback_query
    user = update.effective_user
    
    await query.answer("❌ Dibatalkan", show_alert=True)
    log_user_action(user, "EXECUTE_CANCELLED", "")
    
    return await start(update, context)


@only_allowed
async def handle_execute_futures(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show confirmation before executing futures order"""
    query = update.callback_query
    await query.answer()
    
    user = update.effective_user

    analysis = context.user_data.get("analysis")
    if not analysis:
        await query.answer("❌ Data analisis tidak ditemukan! Silakan analisis ulang.", show_alert=True)
        return await start(update, context)

    log_user_action(user, "EXECUTE_CONFIRM_SCREEN", f"- {analysis['symbol']}")

    await safe_edit_message(
        query,
        f"""
📋 <b>KONFIRMASI FUTURES ORDER</b>

<b>Trading Details:</b>
Symbol: {analysis['symbol']}
Timeframe: {analysis['timeframe']}
Mode: FUTURES

<b>Risk Settings:</b>
• Leverage: 10x
• Position Size: 15% dari available balance
• Auto TP & SL berdasarkan analisis AI

⚠️ <b>Peringatan:</b>
Trading futures memiliki risiko tinggi karena menggunakan leverage. 
Pastikan Anda memahami risikonya dan hanya gunakan dana yang 
siap untuk hilang.

<b>Lanjutkan eksekusi order?</b>
""",
        reply_markup=InlineKeyboardMarkup([
            [
                InlineKeyboardButton("✅ YA, PASANG ORDER", callback_data="execute_confirm"),
                InlineKeyboardButton("❌ BATAL", callback_data="execute_cancel")
            ]
        ])
    )

    return SELECTING_MODE


# ==================== END OF PART 1 ====================
# LANJUT KE PART 2: Main Menu, Mode Selection, Token Selection
# main.py - COMPLETE UPDATED VERSION (PART 2/3)
# Main Menu, Mode Selection, Token Selection, Auto Trading Monitor

# ============================================================
# ====================== MAIN MENU FLOW ======================
# ============================================================

@only_allowed
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Main menu - entry point of the bot"""
    user = update.effective_user
    log_user_action(user, "START", "- Opening main menu")
    
    keyboard = [
        [
            InlineKeyboardButton("💼 SPOT TRADING", callback_data="mode_spot"),
            InlineKeyboardButton("📈 FUTURES TRADING", callback_data="mode_futures")
        ],
        [
            InlineKeyboardButton("🤖 AUTO TRADING MONITOR", callback_data="mode_auto")
        ],
        [
            InlineKeyboardButton("ℹ️ Help & Info", callback_data="help")
        ]
    ]

    reply_markup = InlineKeyboardMarkup(keyboard)

    welcome_text = """
🤖 <b>CRYPTO SIGNAL BOT - AI POWERED</b>

Selamat datang! Bot ini memberikan sinyal trading crypto dengan analisis AI menggunakan GPT-4.

📊 <b>Pilih Mode Trading:</b>

💼 <b>SPOT TRADING</b>
Analisis untuk trading spot (tanpa leverage)

📈 <b>FUTURES TRADING</b>
Analisis + eksekusi otomatis untuk futures trading dengan leverage 10x

🤖 <b>AUTO TRADING MONITOR</b>
Monitor performa auto trading bot

Silakan pilih mode yang diinginkan:
"""

    if update.message:
        await update.message.reply_text(
            welcome_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
    else:
        await safe_edit_message(
            update.callback_query,
            welcome_text,
            reply_markup=reply_markup
        )

    return SELECTING_MODE


# ============================================================
# ==================== MODE SELECTION ========================
# ============================================================

@only_allowed
async def handle_mode_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle mode selection (spot/futures/auto)"""
    query = update.callback_query
    await query.answer()
    
    user = update.effective_user

    mode = query.data.split("_")[1]
    
    log_user_action(user, "MODE_SELECTED", f"- {mode.upper()}")

    if mode == "auto":
        return await show_auto_trading_menu(update, context)

    context.user_data["mode"] = mode

    # Create token buttons (5 per row)
    buttons = []
    for i in range(0, len(TOP_TOKENS[:50]), 5):
        row = [
            InlineKeyboardButton(
                token.replace("USDT", ""),
                callback_data=f"token_{token}"
            )
            for token in TOP_TOKENS[i:i + 5]
        ]
        buttons.append(row)

    buttons.append([InlineKeyboardButton("🔙 Kembali", callback_data="back_to_start")])

    mode_emoji = "💼" if mode == "spot" else "📈"
    mode_text = "SPOT TRADING" if mode == "spot" else "FUTURES TRADING"

    await safe_edit_message(
        query,
        f"""
{mode_emoji} <b>MODE: {mode_text}</b>

📌 Pilih token yang ingin dianalisis dari daftar di bawah.

💡 <i>Atau ketik manual nama token (contoh: BTCUSDT, ETHUSDT)</i>
""",
        reply_markup=InlineKeyboardMarkup(buttons)
    )

    return SELECTING_TOKEN


# ============================================================
# ================ AUTO TRADING MENU =========================
# ============================================================

@only_allowed
async def show_auto_trading_menu(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show auto trading monitor dashboard"""
    query = update.callback_query
    user = update.effective_user
    
    log_user_action(user, "AUTO_MONITOR", "- Viewing dashboard")

    try:
        state = shared_state.get_state()
        balance = state.get("balance", {})
        stats = state.get("stats", {})
        is_running = state.get("is_running", False)

        status_emoji = "🟢" if is_running else "🔴"
        status_text = "RUNNING" if is_running else "STOPPED"

        # Calculate win rate
        total_trades = stats.get("total_trades", 0)
        winning_trades = stats.get("winning_trades", 0)
        win_rate = (winning_trades / total_trades * 100) if total_trades > 0 else 0

        text = f"""
🤖 <b>AUTO TRADING MONITOR</b>

<b>Status:</b> {status_emoji} {status_text}

💰 <b>Balance</b>
Total: {format_currency(balance.get("total", 0))}
Available: {format_currency(balance.get("available", 0))}
Unrealized PnL: {format_currency(balance.get("unrealized_pnl", 0))}

📊 <b>Statistics</b>
Total Trades: {total_trades}
Winning: {winning_trades}
Losing: {stats.get("losing_trades", 0)}
Win Rate: {win_rate:.1f}%

💹 <b>Profit/Loss</b>
Total Profit: {format_currency(stats.get("total_profit", 0))}
Total Loss: {format_currency(abs(stats.get("total_loss", 0)))}
Net P/L: {format_currency(stats.get("total_profit", 0) + stats.get("total_loss", 0))}

⏰ <b>Last Update:</b> {state.get("last_update", "N/A")}
"""

        keyboard = [
            [
                InlineKeyboardButton("📊 Open Positions", callback_data="monitor_positions"),
                InlineKeyboardButton("📜 Trade Log", callback_data="monitor_log")
            ],
            [
                InlineKeyboardButton("📈 Performance", callback_data="monitor_performance"),
                InlineKeyboardButton("⚠️ Error Log", callback_data="monitor_errors")
            ],
            [
                InlineKeyboardButton("🔄 Refresh", callback_data="mode_auto"),
                InlineKeyboardButton("🔙 Back", callback_data="back_to_start")
            ]
        ]

        await safe_edit_message(
            query,
            text,
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

    except Exception as e:
        logger.error(f"Auto trading menu error: {e}")
        await safe_edit_message(
            query,
            "❌ <b>Gagal memuat data auto trading</b>\n\n"
            "Silakan coba lagi atau hubungi admin.",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


# ============================================================
# ================ AUTO MONITOR ACTIONS ======================
# ============================================================

@only_allowed
async def handle_monitor_actions(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle auto trading monitor actions"""
    query = update.callback_query
    await query.answer()
    
    user = update.effective_user

    action = query.data.split("_")[1]
    
    log_user_action(user, f"MONITOR_{action.upper()}", "")
    
    try:
        state = shared_state.get_state()

        if action == "positions":
            positions = state.get("open_positions", [])

            if not positions:
                text = "📊 <b>Open Positions</b>\n\n✅ Tidak ada posisi terbuka saat ini."
            else:
                text = "📊 <b>Open Positions</b>\n\n"
                for pos in positions:
                    pnl = pos.get("unrealized_pnl", 0)
                    emoji = "🟢" if pnl > 0 else "🔴" if pnl < 0 else "⚪"
                    text += f"""
<b>{pos['symbol']}</b> {pos['side']}
Entry: {format_currency(pos['entry_price'])}
Mark: {format_currency(pos['mark_price'])}
PnL: {emoji} {format_currency(pnl)}
Leverage: {pos['leverage']}x
━━━━━━━━━━━━━━━
"""

        elif action == "log":
            trades = state.get("trade_log", [])
            text = "📜 <b>Trade Log (Last 10)</b>\n\n"

            if not trades:
                text += "✅ Belum ada trade yang tercatat."
            else:
                for trade in trades[-10:]:
                    text += f"""
<b>{trade['symbol']}</b> {trade['side']}
Entry: {format_currency(trade['entry_price'])}
TP: {format_currency(trade['tp_price'])}
SL: {format_currency(trade['sl_price'])}
Size: {format_currency(trade['position_size'])}
Time: {trade['timestamp']}
━━━━━━━━━━━━━━━
"""

        elif action == "performance":
            stats = state.get("stats", {})
            total = stats.get("total_trades", 0)
            wins = stats.get("winning_trades", 0)
            losses = stats.get("losing_trades", 0)
            winrate = (wins / total * 100) if total > 0 else 0
            
            total_profit = stats.get("total_profit", 0)
            total_loss = stats.get("total_loss", 0)
            net_profit = total_profit + total_loss

            text = f"""
📈 <b>PERFORMANCE SUMMARY</b>

<b>Trade Statistics:</b>
Total Trades: {total}
Winning Trades: {wins}
Losing Trades: {losses}
Win Rate: {winrate:.1f}%

<b>Financial Performance:</b>
Total Profit: {format_currency(total_profit)}
Total Loss: {format_currency(abs(total_loss))}
Net P/L: {format_currency(net_profit)}

<b>Average per Trade:</b>
Avg Win: {format_currency(total_profit/wins if wins > 0 else 0)}
Avg Loss: {format_currency(abs(total_loss)/losses if losses > 0 else 0)}

<b>Profit Factor:</b>
{(abs(total_profit/total_loss) if total_loss != 0 else 0):.2f}
"""

        elif action == "errors":
            errors = state.get("errors", [])
            text = "⚠️ <b>Error Log (Last 10)</b>\n\n"

            if not errors:
                text += "✅ Tidak ada error yang tercatat."
            else:
                for err in errors[-10:]:
                    text += f"""
⏰ {err['timestamp']}
❌ {err['message']}
━━━━━━━━━━━━━━━
"""

        else:
            text = "❌ Unknown action"

        await safe_edit_message(
            query,
            text,
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back to Monitor", callback_data="mode_auto")]
            ])
        )

    except Exception as e:
        logger.error(f"Monitor action error: {e}")
        await safe_edit_message(
            query,
            "❌ <b>Gagal memuat data</b>\n\nSilakan coba lagi.",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]
            ])
        )

    return SELECTING_MODE


# ============================================================
# ==================== TOKEN SELECTION =======================
# ============================================================

@only_allowed
async def handle_token_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle token selection from button"""
    query = update.callback_query
    await query.answer()
    
    user = update.effective_user

    token = query.data.split("_")[1]
    
    # Validate symbol
    if not validate_symbol(token):
        await query.answer(f"❌ Symbol {token} tidak ditemukan di Binance!", show_alert=True)
        log_user_action(user, "INVALID_SYMBOL_BUTTON", f"- {token}")
        return SELECTING_TOKEN
    
    log_user_action(user, "TOKEN_SELECTED", f"- {token}")
    
    context.user_data["symbol"] = token
    mode = context.user_data.get("mode", "spot")
    emoji = "💼" if mode == "spot" else "📈"

    keyboard = [
        [
            InlineKeyboardButton("⚡ Scalping (15m)", callback_data="strategy_15m"),
            InlineKeyboardButton("📊 Day Trade (1h)", callback_data="strategy_1h")
        ],
        [
            InlineKeyboardButton("🎯 Multi TF (4h)", callback_data="strategy_4h"),
            InlineKeyboardButton("🔄 Swing (1d)", callback_data="strategy_1d")
        ],
        [
            InlineKeyboardButton("📈 Long-Term (1w)", callback_data="strategy_1w")
        ],
        [
            InlineKeyboardButton("🔙 Pilih Token Lain", callback_data=f"mode_{mode}")
        ]
    ]

    await safe_edit_message(
        query,
        f"""
{emoji} <b>{token}</b>

📊 Pilih timeframe untuk analisis:

⚡ <b>Scalping (15m)</b> - Quick trades, high frequency
📊 <b>Day Trade (1h)</b> - Intraday trading
🎯 <b>Multi TF (4h)</b> - Medium term analysis
🔄 <b>Swing (1d)</b> - Multi-day positions
📈 <b>Long-Term (1w)</b> - Position trading
""",
        reply_markup=InlineKeyboardMarkup(keyboard)
    )

    return SELECTING_STRATEGY


@only_allowed
async def handle_manual_token(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle manual token input via text message"""
    user = update.effective_user
    
    # ⭐ UPDATED: Use normalize_symbol
    token = normalize_symbol(update.message.text)
    
    log_user_action(user, "MANUAL_TOKEN_INPUT", f"- {token}")
    
    # Validate symbol exists
    if not validate_symbol(token):
        await update.message.reply_text(
            f"❌ <b>Symbol {token} tidak ditemukan!</b>\n\n"
            "Pastikan nama token benar.\n"
            "Contoh: <code>BTC</code>, <code>ETH</code>, <code>SOL</code>",
            parse_mode="HTML"
        )
        log_user_action(user, "INVALID_SYMBOL_MANUAL", f"- {token}")
        return SELECTING_TOKEN

    context.user_data["symbol"] = token
    mode = context.user_data.get("mode", "spot")
    emoji = "💼" if mode == "spot" else "📈"

    await update.message.reply_text(
        f"""
{emoji} <b>{token}</b>

📊 Pilih timeframe untuk analisis:
""",
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup([
            [
                InlineKeyboardButton("⚡ Scalping (15m)", callback_data="strategy_15m"),
                InlineKeyboardButton("📊 Day Trade (1h)", callback_data="strategy_1h")
            ],
            [
                InlineKeyboardButton("🎯 Multi TF (4h)", callback_data="strategy_4h"),
                InlineKeyboardButton("🔄 Swing (1d)", callback_data="strategy_1d")
            ],
            [
                InlineKeyboardButton("📈 Long-Term (1w)", callback_data="strategy_1w")
            ],
            [
                InlineKeyboardButton("🔙 Kembali", callback_data=f"mode_{mode}")
            ]
        ])
    )

    return SELECTING_STRATEGY


# ==================== END OF PART 2 ====================
# LANJUT KE PART 3: Strategy Selection, Help, Button Handler, Main Function
# main.py - COMPLETE UPDATED VERSION (PART 3/3)
# Strategy Selection, Analysis, Help, Button Router, Main Function

# ============================================================
# ================ STRATEGY / ANALYSIS =======================
# ============================================================

@only_allowed
async def handle_strategy_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle timeframe selection and perform AI analysis"""
    query = update.callback_query
    await query.answer()
    
    user = update.effective_user

    timeframe = query.data.split("_")[1]
    symbol = context.user_data.get("symbol")
    mode = context.user_data.get("mode", "spot")

    if not symbol:
        await query.answer("❌ Symbol tidak ditemukan!", show_alert=True)
        return await start(update, context)

    log_user_action(
        user, 
        "ANALYSIS_START", 
        f"- {symbol} {timeframe} {mode.upper()}"
    )

    await safe_edit_message(
        query,
        f"""
⏳ <b>Menganalisis {symbol}</b>

Mode: {mode.upper()}
Timeframe: {timeframe}

📊 Mengambil data dari Binance...
🤖 Memproses dengan AI...

<i>Mohon tunggu 10-30 detik...</i>
"""
    )

    try:
        # Get klines data from Binance
        klines = binance_client.get_klines(
            symbol=symbol,
            interval=TIMEFRAME_MAP[timeframe],
            limit=100
        )

        if not klines:
            raise Exception("Gagal mendapatkan data dari Binance")

        # Parse OHLC data
        ohlc = [{
            "open": float(k[1]),
            "high": float(k[2]),
            "low": float(k[3]),
            "close": float(k[4]),
            "volume": float(k[5])
        } for k in klines]

        # Get AI analysis (run in executor to avoid blocking)
        loop = asyncio.get_running_loop()
        gpt_result = await loop.run_in_executor(
            None,
            analyze_with_gpt,
            symbol,
            timeframe,
            ohlc[-1],
            mode
        )

        if not gpt_result:
            raise Exception("AI analysis gagal atau timeout")

        # Format result for Telegram
        formatted = format_result_for_telegram(gpt_result)
        current_price = ohlc[-1]["close"]

        # Save analysis to context for later use (e.g., execution)
        context.user_data["analysis"] = {
            "symbol": symbol,
            "timeframe": timeframe,
            "mode": mode,
            "ai_result": gpt_result,
            "current_price": current_price
        }

        # Build keyboard based on mode
        keyboard = [
            [
                InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{timeframe}"),
                InlineKeyboardButton("⏱️ Timeframe Lain", callback_data=f"token_{symbol}")
            ]
        ]

        # ⭐ Add execute button for futures mode
        if mode == "futures":
            keyboard.insert(0, [
                InlineKeyboardButton("🚀 EXECUTE FUTURES", callback_data="execute_multi_tf")
            ])

        keyboard.append([
            InlineKeyboardButton("🔙 Token Lain", callback_data=f"mode_{mode}"),
            InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")
        ])

        # ⭐ UPDATED: Better formatted result with currency formatting
        await safe_edit_message(
            query,
            f"""
<b>📊 ANALISIS {symbol}</b>
━━━━━━━━━━━━━━━━━━━━

<b>Mode:</b> {mode.upper()}
<b>Timeframe:</b> {timeframe}
<b>Current Price:</b> {format_currency(current_price)}

━━━━━━━━━━━━━━━━━━━━
{formatted}
━━━━━━━━━━━━━━━━━━━━

⚠️ <i>Disclaimer: Analisis ini hanya referensi. DYOR (Do Your Own Research) before trading!</i>
""",
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

        log_user_action(user, "ANALYSIS_SUCCESS", f"- {symbol}")

    except BinanceAPIException as e:
        logger.error(f"Binance API error: {e}")
        
        # ⭐ UPDATED: Better error display
        error_display = format_error_message(e, f"analyzing {symbol}")
        
        await safe_edit_message(
            query,
            f"""
❌ <b>Error dari Binance API</b>

<b>Symbol:</b> {symbol}

{error_display}

<b>Kemungkinan penyebab:</b>
• Symbol tidak valid atau tidak tersedia
• API rate limit exceeded
• Koneksi bermasalah

Silakan coba lagi atau pilih token lain.
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data=f"token_{symbol}")],
                [InlineKeyboardButton("🔙 Pilih Token Lain", callback_data=f"mode_{mode}")],
                [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
            ])
        )
        
        log_user_action(user, "ANALYSIS_ERROR", f"- Binance API: {str(e)}")

    except Exception as e:
        logger.error(f"Analysis error: {e}")
        
        await safe_edit_message(
            query,
            f"""
❌ <b>Gagal Melakukan Analisis</b>

<b>Error:</b> {format_error_message(e)}

<b>Kemungkinan penyebab:</b>
• AI service timeout atau bermasalah
• Data tidak lengkap dari Binance
• Error parsing data

Silakan coba lagi dalam beberapa saat.
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data=f"strategy_{timeframe}")],
                [InlineKeyboardButton("🔙 Kembali", callback_data=f"token_{symbol}")],
                [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
            ])
        )
        
        log_user_action(user, "ANALYSIS_ERROR", f"- {str(e)}")

    return SELECTING_MODE


# ============================================================
# ======================= HELP ===============================
# ============================================================

@only_allowed
async def help_command(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show help and information"""
    user = update.effective_user
    log_user_action(user, "HELP", "")
    
    text = """
📚 <b>PANDUAN PENGGUNAAN BOT</b>

<b>🎯 MODE TRADING:</b>

<b>💼 SPOT TRADING</b>
• Trading tanpa leverage
• Risiko lebih rendah
• Hanya mendapat sinyal analisis
• Cocok untuk pemula

<b>📈 FUTURES TRADING</b>
• Trading dengan leverage 10x
• Auto execution order ke Binance
• Risiko tinggi, potensi profit tinggi
• Auto TP & SL berdasarkan AI
• Requires sufficient balance

<b>🤖 AUTO TRADING MONITOR</b>
• Monitor performa bot auto trading
• Lihat open positions real-time
• Track profit/loss
• View trade history & error logs

<b>📊 TIMEFRAME YANG TERSEDIA:</b>
• <b>15m</b> - Scalping (quick trades, 1-15 menit)
• <b>1h</b> - Day trading (beberapa jam)
• <b>4h</b> - Multi timeframe (beberapa hari)
• <b>1d</b> - Swing trading (beberapa hari - minggu)
• <b>1w</b> - Position trading (weeks-months)

<b>💡 TIPS PENGGUNAAN:</b>
1. Pilih mode trading (Spot/Futures)
2. Pilih token yang ingin dianalisis
3. Pilih timeframe sesuai strategi
4. Tunggu analisis AI (10-30 detik)
5. Untuk Futures: klik EXECUTE untuk pasang order

<b>⚠️ DISCLAIMER & RISIKO:</b>
Trading cryptocurrency memiliki risiko tinggi, terutama 
futures trading dengan leverage. Bot ini hanya memberikan 
analisis dan sinyal berdasarkan AI. Keputusan akhir ada 
di tangan Anda.

<b>DYOR (Do Your Own Research) sebelum trading!</b>
Jangan invest lebih dari yang Anda siap untuk hilang.

<b>📞 SUPPORT:</b>
Jika ada masalah teknis atau pertanyaan, hubungi admin.

<b>⚙️ TECHNICAL INFO:</b>
• AI Model: GPT-4
• Exchange: Binance
• Leverage: 10x (Futures)
• Position Size: 15% of balance
• Order Type: Market (instant execution)
"""

    keyboard = [[InlineKeyboardButton("🔙 Kembali ke Menu", callback_data="back_to_start")]]

    if update.callback_query:
        await safe_edit_message(
            update.callback_query,
            text,
            reply_markup=InlineKeyboardMarkup(keyboard)
        )
    else:
        await update.message.reply_text(
            text,
            parse_mode="HTML",
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

    return SELECTING_MODE


# ============================================================
# =================== BUTTON ROUTER ==========================
# ============================================================

async def button_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Central router for all button callbacks"""
    query = update.callback_query
    
    try:
        await query.answer()

        callback_data = query.data
        
        # Route to appropriate handler based on callback data
        if callback_data == "back_to_start":
            return await start(update, context)
        elif callback_data == "help":
            return await help_command(update, context)
        elif callback_data.startswith("monitor_"):
            return await handle_monitor_actions(update, context)
        elif callback_data.startswith("mode_"):
            return await handle_mode_selection(update, context)
        elif callback_data.startswith("token_"):
            return await handle_token_selection(update, context)
        elif callback_data.startswith("strategy_"):
            return await handle_strategy_selection(update, context)
        elif callback_data == "execute_confirm":
            return await handle_execute_confirm(update, context)
        elif callback_data == "execute_cancel":
            return await handle_execute_cancel(update, context)
        elif callback_data == "execute_multi_tf":
            return await handle_execute_futures(update, context)
        else:
            logger.warning(f"Unknown callback data: {callback_data}")
            await query.answer("❌ Invalid action", show_alert=True)
            return SELECTING_MODE

    except Exception as e:
        logger.error(f"Button handler error: {e}")
        await query.answer("❌ Terjadi kesalahan. Silakan coba lagi.", show_alert=True)
        return await start(update, context)


# ============================================================
# =================== ERROR HANDLER ==========================
# ============================================================

async def error_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Global error handler for the bot"""
    logger.error(f"Update {update} caused error: {context.error}")
    
    try:
        if update and update.effective_message:
            await update.effective_message.reply_text(
                "❌ <b>Terjadi kesalahan!</b>\n\n"
                "Bot akan restart. Silakan /start kembali.",
                parse_mode="HTML"
            )
    except Exception as e:
        logger.error(f"Error in error handler: {e}")


# ============================================================
# ======================= MAIN ===============================
# ============================================================

def main():
    """Start the bot"""
    try:
        logger.info("🤖 Initializing bot...")
        
        # Build application
        app = ApplicationBuilder().token(TELEGRAM_TOKEN).build()

        # Create conversation handler with states
        conv_handler = ConversationHandler(
            entry_points=[
                CommandHandler("start", start),
                CallbackQueryHandler(button_handler)
            ],
            states={
                SELECTING_MODE: [
                    CallbackQueryHandler(button_handler)
                ],
                SELECTING_TOKEN: [
                    CallbackQueryHandler(button_handler),
                    MessageHandler(filters.TEXT & ~filters.COMMAND, handle_manual_token)
                ],
                SELECTING_STRATEGY: [
                    CallbackQueryHandler(button_handler)
                ]
            },
            fallbacks=[
                CommandHandler("start", start),
                CommandHandler("help", help_command)
            ],
            allow_reentry=True,
            conversation_timeout=600  # 10 minutes timeout
        )

        # Add handlers
        app.add_handler(conv_handler)
        app.add_handler(CommandHandler("help", help_command))
        
        # Add error handler
        app.add_error_handler(error_handler)

        logger.info("✅ Bot started successfully!")
        logger.info("🚀 Bot is running... Press Ctrl+C to stop.")
        
        # Start polling
        app.run_polling(
            allowed_updates=Update.ALL_TYPES,
            drop_pending_updates=True  # Ignore old pending updates
        )

    except Exception as e:
        logger.error(f"❌ Failed to start bot: {e}")
        raise


if __name__ == "__main__":
    main()


# ==================== END OF PART 3 ====================
# ==================== END OF main.py ====================

"""
CHANGELOG - UPDATED VERSION:

✅ IMPROVEMENTS:
1. Better imports with utility functions from utils.py
2. Enhanced PnL calculation using utility functions
3. Better formatted currency and percentage displays
4. Improved error messages with format_error_message()
5. Symbol validation before processing
6. User action logging throughout the bot
7. Better auto trading monitor with formatted numbers
8. Enhanced help command with more details
9. Conversation timeout (10 minutes)
10. Global error handler
11. MARKET order type for instant execution
12. Better formatted success/error messages
13. Comprehensive logging for debugging

🔧 TECHNICAL:
- Using format_currency() for all price displays
- Using format_percentage() for all % displays
- Using calculate_pnl_percentage() for PnL calculation
- Using calculate_risk_reward() for R/R calculation
- Using validate_price_levels_util() for validation
- Using normalize_symbol() for token input
- Using log_user_action() for user tracking

⚠️ REQUIREMENTS:
- All files must be in place: config.py, utils.py, ai.py, 
  binance_futures.py, shared_state.py
- Binance API keys must be configured in config.py
- Telegram bot token must be set in config.py
- ALLOWED_USER_IDS must be configured

📚 USAGE:
1. Ensure all dependencies are installed
2. Configure config.py with your API keys
3. Run: python main.py
4. Open Telegram and send /start to your bot
"""
