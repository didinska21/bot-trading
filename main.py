# main.py - PART 1/3: IMPORTS, HELPERS, AND FUTURES EXECUTION (FIXED)

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
from utils import only_allowed, format_result_for_telegram
from ai import analyze_with_gpt
from shared_state import SharedState

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

binance_client = Client(
    api_key=BINANCE_API_KEY,
    api_secret=BINANCE_API_SECRET
)

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

# =====================================================================
# ======================= HELPER FUNCTIONS ============================
# =====================================================================

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
        logging.error(f"Failed to edit message: {e}")
        try:
            await query.message.reply_text(text, parse_mode=parse_mode, reply_markup=reply_markup)
        except:
            pass


def parse_price_from_ai(ai_result: str, price_type: str, side: str, current_price: float) -> float:
    """
    IMPROVED: Parse price from AI response with better logic for SHORT positions
    
    Args:
        ai_result: AI response text
        price_type: 'entry', 'tp', or 'sl'
        side: 'BUY' (LONG) or 'SELL' (SHORT)
        current_price: Current market price as fallback
    
    Returns:
        Parsed price as float
    """
    import re
    
    try:
        if price_type == 'entry':
            # Parse entry range
            entry_match = re.search(
                r'Entry\s*(?:Range|Price)?.*?\$?\s*([\d.,]+)\s*(?:-|to|~)?\s*\$?\s*([\d.,]+)?',
                ai_result,
                re.IGNORECASE
            )
            if entry_match:
                entry_low = float(entry_match.group(1).replace(',', ''))
                entry_high = float(entry_match.group(2).replace(',', '')) if entry_match.group(2) else entry_low
                return (entry_low + entry_high) / 2
            else:
                # Fallback: use current price with small buffer
                return current_price * (0.998 if side == 'BUY' else 1.002)
        
        elif price_type == 'tp':
            # Parse Take Profit - CRITICAL FIX for SHORT
            tp_matches = re.findall(
                r'TP\s*\d*.*?\$?\s*([\d.,]+)',
                ai_result,
                re.IGNORECASE
            )
            
            if tp_matches and len(tp_matches) >= 1:
                # Parse all TPs
                tp_values = [float(tp.replace(',', '')) for tp in tp_matches[:3]]
                
                # CRITICAL: Choose TP based on position side
                if side.upper() in ['BUY', 'LONG']:
                    # For LONG: Use highest TP (most aggressive profit target)
                    selected_tp = max(tp_values)
                    logging.info(f"LONG position: Selected TP = ${selected_tp:.4f} from {tp_values}")
                else:  # SHORT or SELL
                    # For SHORT: Use lowest TP (most aggressive profit target downward)
                    selected_tp = min(tp_values)
                    logging.info(f"SHORT position: Selected TP = ${selected_tp:.4f} from {tp_values}")
                
                return selected_tp
            else:
                # Fallback: 3% profit target based on side
                if side.upper() in ['BUY', 'LONG']:
                    return current_price * 1.03  # 3% above for LONG
                else:
                    return current_price * 0.97  # 3% below for SHORT
        
        elif price_type == 'sl':
            # Parse Stop Loss
            sl_match = re.search(
                r'(?:Stop\s*Loss|SL).*?\$?\s*([\d.,]+)',
                ai_result,
                re.IGNORECASE
            )
            if sl_match:
                return float(sl_match.group(1).replace(',', ''))
            else:
                # Fallback: 2% stop loss based on side
                if side.upper() in ['BUY', 'LONG']:
                    return current_price * 0.98  # 2% below for LONG
                else:
                    return current_price * 1.02  # 2% above for SHORT
        
        return current_price
        
    except Exception as e:
        logging.error(f"Error parsing {price_type}: {e}")
        return current_price


# =====================================================================
# ======================= FUTURES EXECUTION ============================
# =====================================================================

@only_allowed
async def handle_execute_confirm(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Execute futures order - IMPROVED VERSION with better parsing"""
    query = update.callback_query
    await query.answer()

    analysis = context.user_data.get("analysis")
    if not analysis:
        await query.answer("❌ Data tidak ditemukan! Silakan analisis ulang.", show_alert=True)
        return await start(update, context)

    await safe_edit_message(
        query,
        "⏳ <b>Memasang posisi...</b>\n\nMohon tunggu...",
    )

    try:
        from binance_futures import BinanceFuturesTrader

        futures_trader = BinanceFuturesTrader()
        ai_result = analysis["ai_result"]
        current_price = analysis["current_price"]

        # Validate AI response
        if not ai_result or len(ai_result.strip()) < 20:
            raise Exception("AI response tidak valid atau terlalu pendek")

        # Detect LONG / SHORT
        ai_upper = ai_result.upper()
        
        # More robust side detection
        if "🟢 LONG" in ai_result or "**LONG**" in ai_result or ("POSISI: LONG" in ai_upper):
            side = "BUY"
            logging.info("✅ Detected LONG position from AI")
        elif "🔴 SHORT" in ai_result or "**SHORT**" in ai_result or ("POSISI: SHORT" in ai_upper):
            side = "SELL"
            logging.info("✅ Detected SHORT position from AI")
        elif "LONG" in ai_upper and "SHORT" not in ai_upper:
            side = "BUY"
            logging.info("✅ Detected LONG position (fallback)")
        elif "SHORT" in ai_upper and "LONG" not in ai_upper:
            side = "SELL"
            logging.info("✅ Detected SHORT position (fallback)")
        else:
            # Check for WAIT signal
            if "WAIT" in ai_upper or "HOLD" in ai_upper:
                await safe_edit_message(
                    query,
                    "❌ <b>AI Menyarankan WAIT</b>\n\nTidak ada sinyal trading yang jelas.\nSilakan coba analisis ulang.",
                    reply_markup=InlineKeyboardMarkup([
                        [InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{analysis['timeframe']}")],
                        [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
                    ])
                )
                return SELECTING_MODE
            
            raise Exception("Tidak dapat mendeteksi LONG atau SHORT dari AI response")

        # Parse prices using improved parser
        entry_price = parse_price_from_ai(ai_result, 'entry', side, current_price)
        tp_price = parse_price_from_ai(ai_result, 'tp', side, current_price)
        sl_price = parse_price_from_ai(ai_result, 'sl', side, current_price)

        logging.info(f"📊 Parsed prices: Entry=${entry_price:.4f}, TP=${tp_price:.4f}, SL=${sl_price:.4f}")

        # Validate price levels before proceeding
        if side == "BUY":
            if tp_price <= entry_price:
                logging.error(f"❌ Invalid TP for LONG: TP={tp_price:.4f} <= Entry={entry_price:.4f}")
                raise Exception(f"Invalid price levels: TP must be above entry for LONG")
            if sl_price >= entry_price:
                logging.error(f"❌ Invalid SL for LONG: SL={sl_price:.4f} >= Entry={entry_price:.4f}")
                raise Exception(f"Invalid price levels: SL must be below entry for LONG")
        else:  # SELL/SHORT
            if tp_price >= entry_price:
                logging.error(f"❌ Invalid TP for SHORT: TP={tp_price:.4f} >= Entry={entry_price:.4f}")
                raise Exception(f"Invalid price levels: TP must be below entry for SHORT")
            if sl_price <= entry_price:
                logging.error(f"❌ Invalid SL for SHORT: SL={sl_price:.4f} <= Entry={entry_price:.4f}")
                raise Exception(f"Invalid price levels: SL must be above entry for SHORT")

        logging.info("✅ Price level validation passed")

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
        logging.info(f"🚀 Placing order: {side} {quantity} {analysis['symbol']}")
        result = futures_trader.place_futures_order(
            symbol=analysis["symbol"],
            side=side,
            quantity=quantity,
            entry_price=entry_price,
            tp_price=tp_price,
            sl_price=sl_price,
            leverage=10
        )

        if not result.get("success"):
            raise Exception(result.get("error", "Unknown error from Binance"))

        # Calculate PnL
        if side == "BUY":
            profit_pct = (tp_price - entry_price) / entry_price * 100
            loss_pct = (entry_price - sl_price) / entry_price * 100
        else:  # SELL/SHORT
            profit_pct = (entry_price - tp_price) / entry_price * 100
            loss_pct = (sl_price - entry_price) / entry_price * 100

        rr = profit_pct / loss_pct if loss_pct > 0 else 0

        # Calculate potential profit/loss with leverage
        leverage = 10
        max_profit = position_size * (profit_pct / 100) * leverage
        max_loss = position_size * (loss_pct / 100) * leverage

        success_msg = f"""
✅ <b>POSISI BERHASIL DIPASANG!</b>

📊 <b>Detail Order:</b>
Symbol: {analysis['symbol']}
Side: <b>{"🟢 LONG" if side == "BUY" else "🔴 SHORT"}</b>
Leverage: 10x

💰 <b>Prices:</b>
Entry: ${entry_price:.4f}
TP: ${tp_price:.4f} (<b>+{profit_pct:.2f}%</b>)
SL: ${sl_price:.4f} (<b>-{loss_pct:.2f}%</b>)

📦 <b>Position:</b>
Quantity: {quantity}
Size: ${position_size:.2f}

📈 <b>Risk/Reward:</b>
Ratio: 1:{rr:.2f}
Max Profit: ${max_profit:.2f}
Max Loss: ${max_loss:.2f}

🎯 <b>Order IDs:</b>
Entry: {result['entry_order']['orderId']}
TP: {result['tp_order']['orderId']}
SL: {result['sl_order']['orderId']}

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

    except Exception as e:
        logging.error(f"Execute futures error: {e}")
        await safe_edit_message(
            query,
            f"""
❌ <b>GAGAL MEMASANG POSISI!</b>

<b>Error:</b> {str(e)}

<b>Kemungkinan penyebab:</b>
• Balance tidak cukup
• Symbol tidak tersedia untuk futures
• Price level tidak valid (cek log untuk detail)
• Koneksi ke Binance bermasalah
• AI response parsing error

<b>Debug Info:</b>
Symbol: {analysis.get('symbol', 'N/A')}
Mode: {analysis.get('mode', 'N/A')}

Silakan cek balance dan coba lagi.
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data=f"strategy_{analysis.get('timeframe', '1h')}")],
                [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


@only_allowed
async def handle_execute_cancel(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer("❌ Dibatalkan", show_alert=True)
    return await start(update, context)


@only_allowed
async def handle_execute_futures(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()

    analysis = context.user_data.get("analysis")
    if not analysis:
        await query.answer("❌ Data analisis tidak ditemukan! Silakan analisis ulang.", show_alert=True)
        return await start(update, context)

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
Trading futures memiliki risiko tinggi. Pastikan Anda memahami risikonya.

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


# =====================================================================
# NOTE: Part 2 akan berisi: start(), mode selection, auto trading menu
# Part 3 akan berisi: token selection, strategy/analysis, error handler
# =====================================================================
# main.py - PART 2/3: MAIN MENU, MODE SELECTION, AUTO TRADING MONITOR

# =====================================================================
# ========================= MAIN MENU FLOW =============================
# =====================================================================

@only_allowed
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Main menu - entry point"""
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

Selamat datang! Bot ini memberikan sinyal trading crypto dengan analisis AI menggunakan Groq (LLaMA 3.3).

📊 <b>Pilih Mode Trading:</b>

💼 <b>SPOT TRADING</b>
Analisis untuk trading spot (tanpa leverage)
• Aman untuk pemula
• Buy & hold strategy
• Risk lebih rendah

📈 <b>FUTURES TRADING</b>
Analisis + eksekusi otomatis futures dengan leverage 10x
• Auto execution order
• TP & SL otomatis
• ⚠️ High risk, high reward

🤖 <b>AUTO TRADING MONITOR</b>
Monitor performa auto trading bot
• Real-time positions
• Trading statistics
• Performance tracking

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


# =====================================================================
# ========================= MODE SELECTION =============================
# =====================================================================

@only_allowed
async def handle_mode_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle mode selection (spot/futures/auto)"""
    query = update.callback_query
    await query.answer()

    mode = query.data.split("_")[1]

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

{"⚠️ <b>Note:</b> Mode futures akan auto-execute order dengan leverage 10x" if mode == "futures" else ""}
""",
        reply_markup=InlineKeyboardMarkup(buttons)
    )

    return SELECTING_TOKEN


# =====================================================================
# ========================= AUTO TRADING MENU ==========================
# =====================================================================

@only_allowed
async def show_auto_trading_menu(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show auto trading monitor dashboard"""
    query = update.callback_query

    try:
        state = shared_state.get_state()
        balance = state.get("balance", {})
        stats = state.get("stats", {})
        daily_stats = state.get("daily_stats", {})
        is_running = state.get("is_running", False)

        status_emoji = "🟢" if is_running else "🔴"
        status_text = "RUNNING" if is_running else "STOPPED"

        # Calculate net P&L
        total_profit = stats.get("total_profit", 0)
        total_loss = stats.get("total_loss", 0)
        net_pnl = total_profit - total_loss

        # Format win rate
        win_rate = stats.get("win_rate", 0)

        text = f"""
🤖 <b>AUTO TRADING MONITOR</b>

<b>Status:</b> {status_emoji} {status_text}
<b>Last Update:</b> {state.get("last_update", "N/A")}

━━━━━━━━━━━━━━━━━━━━
💰 <b>BALANCE</b>
━━━━━━━━━━━━━━━━━━━━
Total: ${balance.get("total", 0):.2f}
Available: ${balance.get("available", 0):.2f}
Unrealized PnL: ${balance.get("unrealized_pnl", 0):.2f}

━━━━━━━━━━━━━━━━━━━━
📊 <b>ALL-TIME STATISTICS</b>
━━━━━━━━━━━━━━━━━━━━
Total Trades: {stats.get("total_trades", 0)}
Winning: {stats.get("winning_trades", 0)} 🟢
Losing: {stats.get("losing_trades", 0)} 🔴
Win Rate: {win_rate:.1f}%

Total Profit: ${total_profit:.2f}
Total Loss: ${total_loss:.2f}
<b>Net P&L: ${net_pnl:.2f}</b>

Avg Profit: ${stats.get("avg_profit", 0):.2f}
Avg Loss: ${stats.get("avg_loss", 0):.2f}
Max Profit: ${stats.get("max_profit", 0):.2f}
Max Loss: ${stats.get("max_loss", 0):.2f}

━━━━━━━━━━━━━━━━━━━━
📅 <b>TODAY ({daily_stats.get("date", "N/A")})</b>
━━━━━━━━━━━━━━━━━━━━
Trades: {daily_stats.get("trades", 0)}
Profit: ${daily_stats.get("profit", 0):.2f}
Loss: ${daily_stats.get("loss", 0):.2f}
Net: ${daily_stats.get("profit", 0) - daily_stats.get("loss", 0):.2f}
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
        logging.error(f"Auto trading menu error: {e}")
        await safe_edit_message(
            query,
            "❌ <b>Gagal memuat data auto trading</b>\n\nSilakan coba lagi atau hubungi admin.",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


# =====================================================================
# ========================= AUTO MONITOR ACTIONS =======================
# =====================================================================

@only_allowed
async def handle_monitor_actions(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle auto trading monitor actions"""
    query = update.callback_query
    await query.answer()

    action = query.data.split("_")[1]
    
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
                    side_emoji = "🟢" if pos['side'] == "LONG" else "🔴"
                    
                    text += f"""
<b>{pos['symbol']}</b> {side_emoji} {pos['side']}
Entry: ${pos['entry_price']:.4f}
Mark: ${pos['mark_price']:.4f}
Qty: {pos['quantity']}
Leverage: {pos['leverage']}x
PnL: {emoji} ${pnl:.2f}
━━━━━━━━━━━━━━━
"""

        elif action == "log":
            trades = state.get("trade_log", [])
            text = "📜 <b>Trade Log (Last 10)</b>\n\n"

            if not trades:
                text += "✅ Belum ada trade yang tercatat."
            else:
                for trade in trades[-10:]:
                    side_emoji = "🟢" if trade.get('side', '').upper() in ['BUY', 'LONG'] else "🔴"
                    text += f"""
{side_emoji} <b>{trade.get('symbol', 'N/A')}</b> {trade.get('side', 'N/A')}
Entry: ${trade.get('entry_price', 0):.4f}
TP: ${trade.get('tp_price', 0):.4f}
SL: ${trade.get('sl_price', 0):.4f}
Size: ${trade.get('position_size', 0):.2f}
Time: {trade.get('timestamp', 'N/A')}
━━━━━━━━━━━━━━━
"""

        elif action == "performance":
            stats = state.get("stats", {})
            total = stats.get("total_trades", 0)
            wins = stats.get("winning_trades", 0)
            losses = stats.get("losing_trades", 0)
            winrate = stats.get("win_rate", 0)
            
            total_profit = stats.get("total_profit", 0)
            total_loss = stats.get("total_loss", 0)
            net_profit = total_profit - total_loss

            # Calculate profit factor
            profit_factor = total_profit / total_loss if total_loss > 0 else 0
            
            text = f"""
📈 <b>PERFORMANCE SUMMARY</b>

━━━━━━━━━━━━━━━━━━━━
<b>📊 Trade Statistics</b>
━━━━━━━━━━━━━━━━━━━━
Total Trades: {total}
Winning Trades: {wins} 🟢
Losing Trades: {losses} 🔴
Win Rate: <b>{winrate:.1f}%</b>

━━━━━━━━━━━━━━━━━━━━
<b>💰 Financial Performance</b>
━━━━━━━━━━━━━━━━━━━━
Total Profit: ${total_profit:.2f}
Total Loss: ${total_loss:.2f}
<b>Net P&L: ${net_profit:.2f}</b>

Profit Factor: {profit_factor:.2f}x

━━━━━━━━━━━━━━━━━━━━
<b>📉 Average per Trade</b>
━━━━━━━━━━━━━━━━━━━━
Avg Win: ${stats.get('avg_profit', 0):.2f}
Avg Loss: ${stats.get('avg_loss', 0):.2f}
Max Win: ${stats.get('max_profit', 0):.2f}
Max Loss: ${stats.get('max_loss', 0):.2f}

━━━━━━━━━━━━━━━━━━━━
<b>🎯 Performance Rating</b>
━━━━━━━━━━━━━━━━━━━━
"""
            # Add performance rating
            if winrate >= 60 and net_profit > 0:
                text += "⭐⭐⭐⭐⭐ Excellent!"
            elif winrate >= 50 and net_profit > 0:
                text += "⭐⭐⭐⭐ Good"
            elif winrate >= 40:
                text += "⭐⭐⭐ Average"
            else:
                text += "⭐⭐ Needs Improvement"

        elif action == "errors":
            errors = state.get("errors", [])
            text = "⚠️ <b>Error Log (Last 10)</b>\n\n"

            if not errors:
                text += "✅ Tidak ada error yang tercatat."
            else:
                for err in errors[-10:]:
                    text += f"""
⏰ {err.get('timestamp', 'N/A')}
❌ {err.get('message', 'Unknown error')}
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
        logging.error(f"Monitor action error: {e}")
        await safe_edit_message(
            query,
            f"""
❌ <b>Gagal memuat data</b>

Error: {str(e)}

Silakan coba lagi.
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]
            ])
        )

    return SELECTING_MODE


# =====================================================================
# NOTE: Part 3 akan berisi: token selection, strategy analysis, 
#       help command, button router, error handler, dan main()
# =====================================================================
# main.py - PART 3/3: TOKEN SELECTION, STRATEGY, HELP, ROUTER, MAIN (FINAL)

# =====================================================================
# ========================= TOKEN SELECTION ============================
# =====================================================================

@only_allowed
async def handle_token_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle token selection from inline buttons"""
    query = update.callback_query
    await query.answer()

    token = query.data.split("_")[1]
    
    # Validate symbol
    if not validate_symbol(token):
        await query.answer(f"❌ Symbol {token} tidak ditemukan di Binance!", show_alert=True)
        return SELECTING_TOKEN
    
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

📊 <b>Pilih timeframe untuk analisis:</b>

⚡ <b>Scalping (15m)</b> - Quick trades, high frequency
• Cocok untuk: Day trader aktif
• Holding time: Minutes to hours

📊 <b>Day Trade (1h)</b> - Intraday trading
• Cocok untuk: Active traders
• Holding time: Hours to 1 day

🎯 <b>Multi TF (4h)</b> - Medium term analysis
• Cocok untuk: Swing traders
• Holding time: 1-7 days

🔄 <b>Swing (1d)</b> - Multi-day positions
• Cocok untuk: Position traders
• Holding time: Days to weeks

📈 <b>Long-Term (1w)</b> - Position trading
• Cocok untuk: Long-term investors
• Holding time: Weeks to months

{"⚠️ <b>Note:</b> Futures mode akan auto-execute setelah analisis" if mode == "futures" else ""}
""",
        reply_markup=InlineKeyboardMarkup(keyboard)
    )

    return SELECTING_STRATEGY


@only_allowed
async def handle_manual_token(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle manual token input from user"""
    token = update.message.text.upper().strip()
    
    # Add USDT if not present
    if not token.endswith("USDT"):
        token += "USDT"
    
    # Validate symbol
    if not validate_symbol(token):
        await update.message.reply_text(
            f"❌ <b>Symbol {token} tidak ditemukan!</b>\n\n"
            "Pastikan nama token benar.\n"
            "Contoh: BTCUSDT, ETHUSDT, BNBUSDT",
            parse_mode="HTML"
        )
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
                InlineKeyboardButton("⚡ 15m", callback_data="strategy_15m"),
                InlineKeyboardButton("📊 1h", callback_data="strategy_1h")
            ],
            [
                InlineKeyboardButton("🎯 4h", callback_data="strategy_4h"),
                InlineKeyboardButton("🔄 1d", callback_data="strategy_1d")
            ],
            [
                InlineKeyboardButton("📈 1w", callback_data="strategy_1w")
            ],
            [
                InlineKeyboardButton("🔙 Kembali", callback_data=f"mode_{mode}")
            ]
        ])
    )

    return SELECTING_STRATEGY


# =====================================================================
# ========================= STRATEGY / ANALYSIS ========================
# =====================================================================

@only_allowed
async def handle_strategy_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle strategy/timeframe selection and perform AI analysis"""
    query = update.callback_query
    await query.answer()

    timeframe = query.data.split("_")[1]
    symbol = context.user_data.get("symbol")
    mode = context.user_data.get("mode", "spot")

    if not symbol:
        await query.answer("❌ Symbol tidak ditemukan!", show_alert=True)
        return await start(update, context)

    # Show loading message
    await safe_edit_message(
        query,
        f"""
⏳ <b>Menganalisis {symbol}</b>

Mode: {mode.upper()}
Timeframe: {timeframe}

📊 Mengambil data dari Binance...
🤖 Memproses dengan AI (Groq LLaMA 3.3)...

<i>Mohon tunggu 10-30 detik...</i>
"""
    )

    try:
        # Get klines data
        logging.info(f"Fetching klines for {symbol} {timeframe}")
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

        logging.info(f"Got {len(ohlc)} candles, analyzing with AI...")

        # Get AI analysis
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

        # Format for Telegram
        formatted = format_result_for_telegram(gpt_result)
        current_price = ohlc[-1]["close"]

        # Save analysis to context
        context.user_data["analysis"] = {
            "symbol": symbol,
            "timeframe": timeframe,
            "mode": mode,
            "ai_result": gpt_result,
            "current_price": current_price,
            "ohlc": ohlc[-1]
        }

        # Build keyboard based on mode
        keyboard = [
            [
                InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{timeframe}"),
                InlineKeyboardButton("⏱️ Timeframe Lain", callback_data=f"token_{symbol}")
            ]
        ]

        # Add execute button for futures mode
        if mode == "futures":
            keyboard.insert(0, [
                InlineKeyboardButton("🚀 EXECUTE FUTURES ORDER", callback_data="execute_multi_tf")
            ])

        keyboard.append([
            InlineKeyboardButton("🔙 Token Lain", callback_data=f"mode_{mode}"),
            InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")
        ])

        # Truncate if too long (Telegram limit 4096 chars)
        from utils import truncate_text
        
        message_text = f"""
<b>📊 ANALISIS {symbol}</b>
━━━━━━━━━━━━━━━━━━━━

<b>Mode:</b> {mode.upper()}
<b>Timeframe:</b> {timeframe}
<b>Current Price:</b> ${current_price:,.4f}

━━━━━━━━━━━━━━━━━━━━
{formatted}
━━━━━━━━━━━━━━━━━━━━

⚠️ <i>Disclaimer: Analisis ini hanya referensi. DYOR before trading!</i>
"""
        
        message_text = truncate_text(message_text, 4000)

        await safe_edit_message(
            query,
            message_text,
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

        logging.info(f"✅ Analysis completed for {symbol}")

    except BinanceAPIException as e:
        logging.error(f"Binance API error: {e}")
        await safe_edit_message(
            query,
            f"""
❌ <b>Error dari Binance API</b>

<b>Symbol:</b> {symbol}
<b>Error:</b> {str(e)}

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

    except Exception as e:
        logging.error(f"Analysis error: {e}")
        await safe_edit_message(
            query,
            f"""
❌ <b>Gagal Melakukan Analisis</b>

<b>Error:</b> {str(e)}

<b>Kemungkinan penyebab:</b>
• AI service timeout atau bermasalah
• Data tidak lengkap dari Binance
• Error parsing data
• Network issues

Silakan coba lagi dalam beberapa saat.
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data=f"strategy_{timeframe}")],
                [InlineKeyboardButton("🔙 Kembali", callback_data=f"token_{symbol}")],
                [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


# =====================================================================
# ========================= HELP =======================================
# =====================================================================

@only_allowed
async def help_command(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show help and usage information"""
    text = """
📚 <b>PANDUAN PENGGUNAAN BOT</b>

━━━━━━━━━━━━━━━━━━━━
<b>🎯 MODE TRADING</b>
━━━━━━━━━━━━━━━━━━━━

<b>💼 SPOT TRADING</b>
• Trading tanpa leverage
• Risiko lebih rendah
• Hanya mendapat sinyal analisis
• Cocok untuk: Pemula & investor jangka panjang

<b>📈 FUTURES TRADING</b>
• Trading dengan leverage 10x
• Auto execution order
• Auto TP & SL
• Risiko tinggi, profit tinggi
• Cocok untuk: Trader berpengalaman

<b>🤖 AUTO TRADING MONITOR</b>
• Monitor performa bot auto trading
• Lihat open positions real-time
• Track profit/loss statistics
• View error logs

━━━━━━━━━━━━━━━━━━━━
<b>📊 TIMEFRAME</b>
━━━━━━━━━━━━━━━━━━━━

<b>15m</b> - Scalping (quick trades, minutes-hours)
<b>1h</b> - Day trading (hours-1 day)
<b>4h</b> - Multi timeframe (1-7 days)
<b>1d</b> - Swing trading (days-weeks)
<b>1w</b> - Position trading (weeks-months)

━━━━━━━━━━━━━━━━━━━━
<b>⚙️ CARA PENGGUNAAN</b>
━━━━━━━━━━━━━━━━━━━━

1️⃣ Pilih mode (Spot/Futures/Monitor)
2️⃣ Pilih token dari daftar atau ketik manual
3️⃣ Pilih timeframe yang sesuai
4️⃣ Tunggu analisis AI (10-30 detik)
5️⃣ Untuk Futures: Klik "Execute" untuk auto-trade

━━━━━━━━━━━━━━━━━━━━
<b>⚠️ DISCLAIMER</b>
━━━━━━━━━━━━━━━━━━━━

• Trading crypto memiliki risiko tinggi
• Bot ini hanya memberikan analisis AI
• Keputusan akhir ada di tangan Anda
• Jangan invest lebih dari yang Anda mampu kehilangan
• <b>DYOR (Do Your Own Research)</b>

━━━━━━━━━━━━━━━━━━━━
<b>🔧 TECHNICAL INFO</b>
━━━━━━━━━━━━━━━━━━━━

• AI Model: Groq LLaMA 3.3 70B
• Exchange: Binance Spot & Futures
• Leverage: 10x (Futures only)
• Position Size: 15% of balance
• TP/SL: Auto-set by AI

━━━━━━━━━━━━━━━━━━━━
<b>📞 SUPPORT</b>
━━━━━━━━━━━━━━━━━━━━

Jika ada masalah atau pertanyaan:
• Coba restart bot: /start
• Check balance di Binance
• Hubungi admin jika error persisten

<b>💡 Tips:</b>
• Start dengan capital kecil
• Test di Spot sebelum Futures
• Monitor positions secara berkala
• Set stop-loss selalu!
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


# =====================================================================
# ========================= BUTTON ROUTER ==============================
# =====================================================================

async def button_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Central button handler - routes to appropriate function"""
    query = update.callback_query
    
    try:
        await query.answer()
        callback_data = query.data

        logging.info(f"Button pressed: {callback_data}")

        # Route to appropriate handler
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
            logging.warning(f"Unknown callback data: {callback_data}")
            await query.answer("❌ Invalid action", show_alert=True)
            return SELECTING_MODE

    except Exception as e:
        logging.error(f"Button handler error: {e}")
        import traceback
        logging.error(traceback.format_exc())
        
        try:
            await query.answer("❌ Terjadi kesalahan. Silakan coba lagi.", show_alert=True)
        except:
            pass
        
        return await start(update, context)


# =====================================================================
# ========================= ERROR HANDLER ==============================
# =====================================================================

async def error_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Global error handler for the bot"""
    logging.error(f"Update {update} caused error {context.error}")
    
    import traceback
    logging.error(traceback.format_exc())
    
    try:
        if update and update.effective_message:
            await update.effective_message.reply_text(
                "❌ <b>Terjadi kesalahan!</b>\n\n"
                "Bot akan restart. Silakan /start kembali.\n\n"
                "<i>Jika error terus muncul, hubungi admin.</i>",
                parse_mode="HTML"
            )
    except Exception as e:
        logging.error(f"Error in error handler: {e}")


# =====================================================================
# ========================= MAIN =======================================
# =====================================================================

def main():
    """Start the bot"""
    try:
        logging.info("🤖 Starting bot...")
        
        # Build application
        app = ApplicationBuilder().token(TELEGRAM_TOKEN).build()

        # Create conversation handler
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

        logging.info("✅ Bot started successfully!")
        logging.info("="*60)
        logging.info("Bot is running... Press Ctrl+C to stop.")
        logging.info("="*60)
        
        # Start polling
        app.run_polling(
            allowed_updates=Update.ALL_TYPES,
            drop_pending_updates=True  # Ignore old updates
        )

    except Exception as e:
        logging.error(f"❌ Failed to start bot: {e}")
        import traceback
        logging.error(traceback.format_exc())
        raise


if __name__ == "__main__":
    main()
