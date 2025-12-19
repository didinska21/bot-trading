# main.py - CLEAN VERSION (FULL, PART 1/2)

import logging
import asyncio
from telegram import Update, InlineKeyboardButton, InlineKeyboardMarkup
from telegram.ext import (
    ApplicationBuilder, CommandHandler, CallbackQueryHandler,
    MessageHandler, ConversationHandler, ContextTypes, filters
)
from binance.client import Client

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

# === BINANCE TIMEFRAME MAP (FIX, TANPA UBAH UX) ===
TIMEFRAME_MAP = {
    "15m": Client.KLINE_INTERVAL_15MINUTE,
    "1h": Client.KLINE_INTERVAL_1HOUR,
    "4h": Client.KLINE_INTERVAL_4HOUR,
    "1d": Client.KLINE_INTERVAL_1DAY,
    "1w": Client.KLINE_INTERVAL_1WEEK,
}

# =====================================================================
# ======================= FUTURES EXECUTION ============================
# =====================================================================

@only_allowed
async def handle_execute_confirm(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()

    analysis = context.user_data.get("analysis")
    if not analysis:
        await query.answer("❌ Data tidak ditemukan!", show_alert=True)
        return await start(update, context)

    await query.edit_message_text(
        "⏳ <b>Memasang posisi...</b>\n\nMohon tunggu...",
        parse_mode="HTML"
    )

    try:
        from binance_futures import BinanceFuturesTrader
        import re

        futures_trader = BinanceFuturesTrader()

        ai_result = analysis["ai_result"]
        current_price = analysis["current_price"]

        # SAFETY MINIMAL (tidak ubah logika)
        if not ai_result or len(ai_result.strip()) < 20:
            raise Exception("AI response kosong atau tidak valid")

        # Detect LONG / SHORT
        if "LONG" in ai_result.upper():
            side = "BUY"
        elif "SHORT" in ai_result.upper():
            side = "SELL"
        else:
            await query.edit_message_text(
                "❌ <b>AI menyarankan WAIT</b>\n\nTidak ada posisi yang dipasang.",
                parse_mode="HTML"
            )
            return SELECTING_MODE

        # Entry price
        entry_match = re.search(
            r'Entry Range.*?\$?([\d.,]+)\s*-\s*\$?([\d.,]+)',
            ai_result,
            re.IGNORECASE
        )
        if entry_match:
            entry_low = float(entry_match.group(1).replace(',', ''))
            entry_high = float(entry_match.group(2).replace(',', ''))
            entry_price = (entry_low + entry_high) / 2
        else:
            entry_price = current_price * (0.998 if side == 'BUY' else 1.002)

        # Take Profit
        tp_matches = re.findall(r'TP[123].*?\$?([\d.,]+)', ai_result, re.IGNORECASE)
        if tp_matches and len(tp_matches) >= 2:
            tp_price = float(tp_matches[1].replace(',', ''))
        else:
            tp_price = entry_price * (1.03 if side == 'BUY' else 0.97)

        # Stop Loss
        sl_match = re.search(r'Stop Loss.*?\$?([\d.,]+)', ai_result, re.IGNORECASE)
        if sl_match:
            sl_price = float(sl_match.group(1).replace(',', ''))
        else:
            sl_price = entry_price * (0.98 if side == 'BUY' else 1.02)

        balance = futures_trader.get_futures_balance()
        if not balance:
            raise Exception("Gagal mendapatkan balance")

        position_size = balance["availableBalance"] * 0.15

        quantity = futures_trader.calculate_quantity(
            analysis["symbol"],
            entry_price,
            position_size
        )
        if not quantity:
            raise Exception("Gagal menghitung quantity")

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
            raise Exception(result.get("error", "Unknown error"))

        # PnL calculation (FIX tanpa ubah logic)
        if side == "BUY":
            profit_pct = (tp_price - entry_price) / entry_price * 100
            loss_pct = (entry_price - sl_price) / entry_price * 100
        else:
            profit_pct = (entry_price - tp_price) / entry_price * 100
            loss_pct = (sl_price - entry_price) / entry_price * 100

        rr = profit_pct / loss_pct if loss_pct > 0 else 0

        success_msg = f"""
✅ <b>POSISI BERHASIL DIPASANG!</b>

📊 <b>Detail Order:</b>
Symbol: {analysis['symbol']}
Side: {"LONG" if side == "BUY" else "SHORT"}
Leverage: 10x

💰 <b>Prices:</b>
Entry: ${entry_price:.4f}
TP: ${tp_price:.4f} (+{profit_pct:.2f}%)
SL: ${sl_price:.4f} (-{loss_pct:.2f}%)

📦 <b>Position:</b>
Quantity: {quantity}
Size: ${position_size:.2f}

📈 <b>Risk/Reward:</b>
1:{rr:.2f}
Max Profit: ${position_size * (profit_pct/100):.2f}
Max Loss: ${position_size * (loss_pct/100):.2f}

🎯 <b>Order IDs:</b>
Entry: {result['entry_order']['orderId']}
TP: {result['tp_order']['orderId']}
SL: {result['sl_order']['orderId']}
"""

        keyboard = [
            [InlineKeyboardButton("📊 Lihat di Binance", url=f"https://www.binance.com/en/futures/{analysis['symbol']}")],
            [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
        ]

        await query.edit_message_text(
            success_msg,
            parse_mode="HTML",
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

    except Exception as e:
        await query.edit_message_text(
            f"""
❌ <b>GAGAL MEMASANG POSISI!</b>

Error: {str(e)}
""",
            parse_mode="HTML",
            reply_markup=InlineKeyboardMarkup([
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
        await query.answer("❌ Data analisis tidak ditemukan!", show_alert=True)
        return await start(update, context)

    await query.edit_message_text(
        f"""
📋 <b>KONFIRMASI FUTURES ORDER</b>

Symbol: {analysis['symbol']}
Timeframe: {analysis['timeframe']}
Leverage: 10x
Position size: 15% balance

<b>Lanjutkan?</b>
""",
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup([
            [
                InlineKeyboardButton("✅ YA, PASANG", callback_data="execute_confirm"),
                InlineKeyboardButton("❌ BATAL", callback_data="execute_cancel")
            ]
        ])
    )

    return SELECTING_MODE

# ===================== PART 1 STOP DI SINI =====================

# =====================================================================
# ========================= MAIN MENU FLOW =============================
# =====================================================================

@only_allowed
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    keyboard = [
        [
            InlineKeyboardButton("💼 SPOT TRADING", callback_data="mode_spot"),
            InlineKeyboardButton("📈 FUTURES TRADING", callback_data="mode_futures")
        ],
        [
            InlineKeyboardButton("🤖 AUTO TRADING FUTURES", callback_data="mode_auto")
        ],
        [
            InlineKeyboardButton("ℹ️ Help & Info", callback_data="help")
        ]
    ]

    reply_markup = InlineKeyboardMarkup(keyboard)

    welcome_text = """
🤖 <b>CRYPTO SIGNAL BOT - AI POWERED</b>

Selamat datang! Bot ini memberikan sinyal trading crypto dengan analisis AI.

📊 <b>Pilih Mode Trading:</b>
- <b>SPOT</b> - Trading spot
- <b>FUTURES</b> - Trading futures manual
- <b>AUTO TRADING</b> - Monitoring auto trading

Silakan pilih mode:
"""

    if update.message:
        await update.message.reply_text(
            welcome_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
    else:
        await update.callback_query.message.edit_text(
            welcome_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )

    return SELECTING_MODE


# =====================================================================
# ========================= MODE SELECTION =============================
# =====================================================================

@only_allowed
async def handle_mode_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()

    mode = query.data.split("_")[1]

    if mode == "auto":
        return await show_auto_trading_menu(update, context)

    context.user_data["mode"] = mode

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
    mode_text = "SPOT" if mode == "spot" else "FUTURES"

    await query.edit_message_text(
        f"""
{mode_emoji} <b>MODE: {mode_text}</b>

📌 Pilih token yang ingin dianalisis
<i>Atau ketik manual (contoh: BTCUSDT)</i>
""",
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup(buttons)
    )

    return SELECTING_TOKEN


# =====================================================================
# ========================= AUTO TRADING MENU ==========================
# =====================================================================

@only_allowed
async def show_auto_trading_menu(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query

    state = shared_state.get_state()
    balance = state.get("balance", {})
    stats = state.get("stats", {})
    is_running = state.get("is_running", False)

    status_emoji = "🟢" if is_running else "🔴"
    status_text = "RUNNING" if is_running else "STOPPED"

    text = f"""
🤖 <b>AUTO TRADING MONITOR</b>

Status: {status_emoji} {status_text}

💰 <b>Balance</b>
Total: ${balance.get("total", 0):.2f}
Available: ${balance.get("available", 0):.2f}
PnL: ${balance.get("unrealized_pnl", 0):.2f}

📊 <b>Stats</b>
Total Trades: {stats.get("total_trades", 0)}
Win: {stats.get("winning_trades", 0)}
Loss: {stats.get("losing_trades", 0)}
Profit: ${stats.get("total_profit", 0):.2f}
Loss: ${stats.get("total_loss", 0):.2f}

⏰ Last Update: {state.get("last_update", "N/A")}
"""

    keyboard = [
        [
            InlineKeyboardButton("📊 Positions", callback_data="monitor_positions"),
            InlineKeyboardButton("📜 Trade Log", callback_data="monitor_log")
        ],
        [
            InlineKeyboardButton("📈 Performance", callback_data="monitor_performance"),
            InlineKeyboardButton("⚠️ Errors", callback_data="monitor_errors")
        ],
        [
            InlineKeyboardButton("🔄 Refresh", callback_data="mode_auto"),
            InlineKeyboardButton("🔙 Back", callback_data="back_to_start")
        ]
    ]

    await query.edit_message_text(
        text,
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup(keyboard)
    )

    return SELECTING_MODE


# =====================================================================
# ========================= AUTO MONITOR ACTIONS =======================
# =====================================================================

@only_allowed
async def handle_monitor_actions(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()

    action = query.data.split("_")[1]
    state = shared_state.get_state()

    if action == "positions":
        positions = state.get("open_positions", [])

        if not positions:
            text = "📊 <b>Open Positions</b>\n\nTidak ada posisi terbuka."
        else:
            text = "📊 <b>Open Positions</b>\n\n"
            for pos in positions:
                pnl = pos.get("unrealized_pnl", 0)
                emoji = "🟢" if pnl > 0 else "🔴"
                text += f"""
<b>{pos['symbol']}</b>
Side: {pos['side']}
Entry: ${pos['entry_price']:.4f}
Mark: ${pos['mark_price']:.4f}
PnL: {emoji} ${pnl:.2f}
Leverage: {pos['leverage']}x
━━━━━━━━━━━━━━━
"""

    elif action == "log":
        trades = state.get("trade_log", [])
        text = "📜 <b>Trade Log</b>\n\n"

        if not trades:
            text += "Belum ada trade."
        else:
            for trade in trades[-10:]:
                text += f"""
<b>{trade['symbol']}</b> {trade['side']}
Entry: ${trade['entry_price']:.4f}
TP: ${trade['tp_price']:.4f}
SL: ${trade['sl_price']:.4f}
Size: ${trade['position_size']:.2f}
Time: {trade['timestamp']}
━━━━━━━━━━━━━━━
"""

    elif action == "performance":
        stats = state.get("stats", {})
        total = stats.get("total_trades", 0)
        wins = stats.get("winning_trades", 0)
        losses = stats.get("losing_trades", 0)
        winrate = (wins / total * 100) if total else 0

        text = f"""
📈 <b>PERFORMANCE</b>

Total Trades: {total}
Wins: {wins}
Losses: {losses}
Winrate: {winrate:.1f}%

Profit: ${stats.get("total_profit", 0):.2f}
Loss: ${stats.get("total_loss", 0):.2f}
"""

    elif action == "errors":
        errors = state.get("errors", [])
        text = "⚠️ <b>Error Log</b>\n\n"

        if not errors:
            text += "Tidak ada error."
        else:
            for err in errors[-10:]:
                text += f"""
{err['timestamp']}
{err['message']}
━━━━━━━━━━━━━━━
"""

    else:
        text = "❌ Unknown action"

    await query.edit_message_text(
        text,
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup([
            [InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]
        ])
    )

    return SELECTING_MODE


# =====================================================================
# ========================= TOKEN SELECTION ============================
# =====================================================================

@only_allowed
async def handle_token_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()

    token = query.data.split("_")[1]
    context.user_data["symbol"] = token

    mode = context.user_data.get("mode", "spot")
    emoji = "💼" if mode == "spot" else "📈"

    keyboard = [
        [
            InlineKeyboardButton("⚡ Scalping (15m)", callback_data="strategy_15m"),
            InlineKeyboardButton("📊 Day Trade (1h)", callback_data="strategy_1h")
        ],
        [
            InlineKeyboardButton("🔄 Swing (1d)", callback_data="strategy_1d"),
            InlineKeyboardButton("📈 Long-Term (1w)", callback_data="strategy_1w")
        ],
        [
            InlineKeyboardButton("🎯 Multi TF (4h)", callback_data="strategy_4h")
        ],
        [
            InlineKeyboardButton("🔙 Kembali", callback_data=f"mode_{mode}")
        ]
    ]

    await query.edit_message_text(
        f"{emoji} <b>{token}</b>\n\nPilih timeframe:",
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup(keyboard)
    )

    return SELECTING_STRATEGY


@only_allowed
async def handle_manual_token(update: Update, context: ContextTypes.DEFAULT_TYPE):
    token = update.message.text.upper().strip()
    if not token.endswith("USDT"):
        token += "USDT"

    context.user_data["symbol"] = token
    mode = context.user_data.get("mode", "spot")
    emoji = "💼" if mode == "spot" else "📈"

    await update.message.reply_text(
        f"{emoji} <b>{token}</b>\n\nPilih timeframe:",
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup([
            [
                InlineKeyboardButton("⚡ Scalping (15m)", callback_data="strategy_15m"),
                InlineKeyboardButton("📊 Day Trade (1h)", callback_data="strategy_1h")
            ],
            [
                InlineKeyboardButton("🔄 Swing (1d)", callback_data="strategy_1d"),
                InlineKeyboardButton("📈 Long-Term (1w)", callback_data="strategy_1w")
            ],
            [
                InlineKeyboardButton("🎯 Multi TF (4h)", callback_data="strategy_4h")
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
    query = update.callback_query
    await query.answer()

    timeframe = query.data.split("_")[1]
    symbol = context.user_data.get("symbol")
    mode = context.user_data.get("mode", "spot")

    await query.edit_message_text(
        f"""
⏳ <b>Menganalisis {symbol}</b>

Mode: {mode.upper()}
Timeframe: {timeframe}
""",
        parse_mode="HTML"
    )

    klines = binance_client.get_klines(
        symbol=symbol,
        interval=TIMEFRAME_MAP[timeframe],
        limit=100
    )

    ohlc = [{
        "open": float(k[1]),
        "high": float(k[2]),
        "low": float(k[3]),
        "close": float(k[4]),
        "volume": float(k[5])
    } for k in klines]

    loop = asyncio.get_running_loop()
    gpt_result = await loop.run_in_executor(
        None,
        analyze_with_gpt,
        symbol,
        timeframe,
        ohlc[-1],
        mode
    )

    formatted = format_result_for_telegram(gpt_result)

    # === SIMPAN ANALYSIS (FIX KRITIS) ===
    context.user_data["analysis"] = {
        "symbol": symbol,
        "timeframe": timeframe,
        "mode": mode,
        "ai_result": gpt_result,
        "current_price": ohlc[-1]["close"]
    }

    keyboard = [
        [
            InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{timeframe}"),
            InlineKeyboardButton("🔙 Token Lain", callback_data=f"mode_{mode}")
        ],
        [
            InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")
        ]
    ]

    await query.edit_message_text(
        f"""
<b>ANALISIS {symbol}</b>
Timeframe: {timeframe}
Price: ${ohlc[-1]['close']:,.2f}

━━━━━━━━━━━━━━━
{formatted}
""",
        parse_mode="HTML",
        reply_markup=InlineKeyboardMarkup(keyboard)
    )

    return SELECTING_MODE


# =====================================================================
# ========================= HELP ======================================
# =====================================================================

@only_allowed
async def help_command(update: Update, context: ContextTypes.DEFAULT_TYPE):
    text = """
📚 <b>PANDUAN BOT</b>

- SPOT: tanpa leverage
- FUTURES: manual execution
- AUTO: monitoring auto trading

⚠️ Trading crypto berisiko tinggi.
"""

    if update.callback_query:
        await update.callback_query.edit_message_text(
            text,
            parse_mode="HTML",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="back_to_start")]
            ])
        )
    else:
        await update.message.reply_text(text, parse_mode="HTML")


# =====================================================================
# ========================= BUTTON ROUTER ==============================
# =====================================================================

async def button_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()

    if query.data == "back_to_start":
        return await start(update, context)
    elif query.data == "help":
        return await help_command(update, context)
    elif query.data.startswith("monitor_"):
        return await handle_monitor_actions(update, context)
    elif query.data.startswith("mode_"):
        return await handle_mode_selection(update, context)
    elif query.data.startswith("token_"):
        return await handle_token_selection(update, context)
    elif query.data.startswith("strategy_"):
        return await handle_strategy_selection(update, context)
    elif query.data == "execute_confirm":
        return await handle_execute_confirm(update, context)
    elif query.data == "execute_cancel":
        return await handle_execute_cancel(update, context)
    elif query.data == "execute_multi_tf":
        return await handle_execute_futures(update, context)


# =====================================================================
# ========================= MAIN ======================================
# =====================================================================

def main():
    app = ApplicationBuilder().token(TELEGRAM_TOKEN).build()

    conv_handler = ConversationHandler(
        entry_points=[
            CommandHandler("start", start),
            CallbackQueryHandler(button_handler)
        ],
        states={
            SELECTING_MODE: [CallbackQueryHandler(button_handler)],
            SELECTING_TOKEN: [
                CallbackQueryHandler(button_handler),
                MessageHandler(filters.TEXT & ~filters.COMMAND, handle_manual_token)
            ],
            SELECTING_STRATEGY: [CallbackQueryHandler(button_handler)]
        },
        fallbacks=[CommandHandler("start", start)],
        allow_reentry=True
    )

    app.add_handler(conv_handler)
    app.add_handler(CommandHandler("help", help_command))

    logging.info("🤖 Bot started successfully")
    app.run_polling(allowed_updates=Update.ALL_TYPES)


if __name__ == "__main__":
    main()
