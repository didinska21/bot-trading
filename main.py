# main.py - COMPLETE VERSION - PART 1/3

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
from utils import only_allowed, format_result_for_telegram, truncate_text
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
SELECTING_MODE, SELECTING_TOKEN, SELECTING_STRATEGY, INPUTTING_SIZE, INPUTTING_LEVERAGE = range(5)

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
    """Parse price from AI response with smart logic for LONG/SHORT"""
    import re
    
    try:
        if price_type == 'entry':
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
                return current_price * (0.998 if side == 'BUY' else 1.002)
        
        elif price_type == 'tp':
            tp_matches = re.findall(r'TP\s*\d*.*?\$?\s*([\d.,]+)', ai_result, re.IGNORECASE)
            
            if tp_matches and len(tp_matches) >= 1:
                tp_values = [float(tp.replace(',', '')) for tp in tp_matches[:3]]
                
                if side.upper() in ['BUY', 'LONG']:
                    selected_tp = max(tp_values)
                    logging.info(f"LONG: Selected TP = ${selected_tp:.4f} from {tp_values}")
                else:
                    selected_tp = min(tp_values)
                    logging.info(f"SHORT: Selected TP = ${selected_tp:.4f} from {tp_values}")
                
                return selected_tp
            else:
                if side.upper() in ['BUY', 'LONG']:
                    return current_price * 1.03
                else:
                    return current_price * 0.97
        
        elif price_type == 'sl':
            sl_match = re.search(r'(?:Stop\s*Loss|SL).*?\$?\s*([\d.,]+)', ai_result, re.IGNORECASE)
            if sl_match:
                return float(sl_match.group(1).replace(',', ''))
            else:
                if side.upper() in ['BUY', 'LONG']:
                    return current_price * 0.98
                else:
                    return current_price * 1.02
        
        return current_price
        
    except Exception as e:
        logging.error(f"Error parsing {price_type}: {e}")
        return current_price


# =====================================================================
# ========================= MAIN MENU =================================
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

Selamat datang! Bot ini memberikan sinyal trading crypto dengan analisis AI.

📊 <b>Pilih Mode Trading:</b>

💼 <b>SPOT TRADING</b>
Analisis untuk trading spot (tanpa leverage)

📈 <b>FUTURES TRADING</b>
Analisis + eksekusi otomatis futures
• Manual input position size & leverage
• Auto TP & SL
• ⚠️ High risk, high reward

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


# =====================================================================
# ========================= MODE SELECTION ============================
# =====================================================================

@only_allowed
async def handle_mode_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle mode selection"""
    query = update.callback_query
    await query.answer()

    mode = query.data.split("_")[1]

    if mode == "auto":
        return await show_auto_trading_menu(update, context)

    context.user_data["mode"] = mode

    # Create token buttons
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

📌 Pilih token yang ingin dianalisis

💡 <i>Atau ketik manual nama token (contoh: BTCUSDT)</i>
""",
        reply_markup=InlineKeyboardMarkup(buttons)
    )

    return SELECTING_TOKEN


# =====================================================================
# =================== AUTO TRADING MONITOR ============================
# =====================================================================

@only_allowed
async def show_auto_trading_menu(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show auto trading monitor"""
    query = update.callback_query

    try:
        state = shared_state.get_state()
        balance = state.get("balance", {})
        stats = state.get("stats", {})
        is_running = state.get("is_running", False)

        status_emoji = "🟢" if is_running else "🔴"
        status_text = "RUNNING" if is_running else "STOPPED"

        text = f"""
🤖 <b>AUTO TRADING MONITOR</b>

<b>Status:</b> {status_emoji} {status_text}

💰 <b>Balance</b>
Total: ${balance.get("total", 0):.2f}
Available: ${balance.get("available", 0):.2f}

📊 <b>Statistics</b>
Total Trades: {stats.get("total_trades", 0)}
Winning: {stats.get("winning_trades", 0)}
Losing: {stats.get("losing_trades", 0)}

⏰ <b>Last Update:</b> {state.get("last_update", "N/A")}
"""

        keyboard = [
            [
                InlineKeyboardButton("📊 Positions", callback_data="monitor_positions"),
                InlineKeyboardButton("📜 Logs", callback_data="monitor_log")
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
            "❌ <b>Gagal memuat data</b>",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


@only_allowed
async def handle_monitor_actions(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle monitor actions"""
    query = update.callback_query
    await query.answer()

    action = query.data.split("_")[1]
    
    try:
        state = shared_state.get_state()

        if action == "positions":
            positions = state.get("open_positions", [])
            text = "📊 <b>Open Positions</b>\n\n"
            
            if not positions:
                text += "✅ Tidak ada posisi terbuka"
            else:
                for pos in positions:
                    text += f"<b>{pos['symbol']}</b> {pos['side']}\n"
                    text += f"PnL: ${pos.get('unrealized_pnl', 0):.2f}\n━━━\n"

        elif action == "log":
            trades = state.get("trade_log", [])
            text = "📜 <b>Trade Log (Last 10)</b>\n\n"
            
            if not trades:
                text += "✅ Belum ada trade"
            else:
                for trade in trades[-10:]:
                    text += f"{trade.get('symbol', 'N/A')} {trade.get('side', 'N/A')}\n"
                    text += f"Time: {trade.get('timestamp', 'N/A')}\n━━━\n"

        else:
            text = "❌ Unknown action"

        await safe_edit_message(
            query,
            text,
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]
            ])
        )

    except Exception as e:
        logging.error(f"Monitor action error: {e}")
        text = "❌ Error"
        await safe_edit_message(query, text)

    return SELECTING_MODE


# Continue to Part 2...
# main.py - COMPLETE VERSION - PART 2/3

# =====================================================================
# ===================== TOKEN SELECTION ===============================
# =====================================================================

@only_allowed
async def handle_token_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle token selection"""
    query = update.callback_query
    await query.answer()

    token = query.data.split("_")[1]
    
    if not validate_symbol(token):
        await query.answer(f"❌ Symbol {token} tidak valid!", show_alert=True)
        return SELECTING_TOKEN
    
    context.user_data["symbol"] = token
    mode = context.user_data.get("mode", "spot")
    emoji = "💼" if mode == "spot" else "📈"

    keyboard = [
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
            InlineKeyboardButton("🔙 Token Lain", callback_data=f"mode_{mode}")
        ]
    ]

    await safe_edit_message(
        query,
        f"{emoji} <b>{token}</b>\n\n📊 Pilih timeframe:",
        reply_markup=InlineKeyboardMarkup(keyboard)
    )

    return SELECTING_STRATEGY


@only_allowed
async def handle_manual_token(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle manual token input"""
    token = update.message.text.upper().strip()
    
    if not token.endswith("USDT"):
        token += "USDT"
    
    if not validate_symbol(token):
        await update.message.reply_text(
            f"❌ <b>Symbol {token} tidak ditemukan!</b>\n\nContoh: BTCUSDT, ETHUSDT",
            parse_mode="HTML"
        )
        return SELECTING_TOKEN

    context.user_data["symbol"] = token
    mode = context.user_data.get("mode", "spot")

    await update.message.reply_text(
        f"<b>{token}</b>\n\n📊 Pilih timeframe:",
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
            [InlineKeyboardButton("📈 1w", callback_data="strategy_1w")],
            [InlineKeyboardButton("🔙 Kembali", callback_data=f"mode_{mode}")]
        ])
    )

    return SELECTING_STRATEGY


# =====================================================================
# ===================== STRATEGY / ANALYSIS ===========================
# =====================================================================

@only_allowed
async def handle_strategy_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle strategy/timeframe and perform analysis"""
    query = update.callback_query
    await query.answer()

    timeframe = query.data.split("_")[1]
    symbol = context.user_data.get("symbol")
    mode = context.user_data.get("mode", "spot")

    if not symbol:
        await query.answer("❌ Symbol tidak ditemukan!", show_alert=True)
        return await start(update, context)

    await safe_edit_message(
        query,
        f"⏳ <b>Menganalisis {symbol}</b>\n\nMode: {mode.upper()}\nTimeframe: {timeframe}\n\n<i>Tunggu 10-30 detik...</i>"
    )

    try:
        # Get klines
        klines = binance_client.get_klines(
            symbol=symbol,
            interval=TIMEFRAME_MAP[timeframe],
            limit=100
        )

        if not klines:
            raise Exception("Gagal mendapatkan data dari Binance")

        # Parse OHLC
        ohlc = [{
            "open": float(k[1]),
            "high": float(k[2]),
            "low": float(k[3]),
            "close": float(k[4]),
            "volume": float(k[5])
        } for k in klines]

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
            raise Exception("AI analysis gagal")

        formatted = format_result_for_telegram(gpt_result)
        current_price = ohlc[-1]["close"]

        # Save to context
        context.user_data["analysis"] = {
            "symbol": symbol,
            "timeframe": timeframe,
            "mode": mode,
            "ai_result": gpt_result,
            "current_price": current_price
        }

        # Build keyboard
        keyboard = [
            [
                InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{timeframe}"),
                InlineKeyboardButton("⏱️ Timeframe Lain", callback_data=f"token_{symbol}")
            ]
        ]

        # Add execute button for futures
        if mode == "futures":
            keyboard.insert(0, [
                InlineKeyboardButton("🚀 EXECUTE FUTURES", callback_data="execute_multi_tf")
            ])

        keyboard.append([
            InlineKeyboardButton("🔙 Token Lain", callback_data=f"mode_{mode}"),
            InlineKeyboardButton("🏠 Menu", callback_data="back_to_start")
        ])

        message_text = f"""<b>📊 ANALISIS {symbol}</b>
━━━━━━━━━━━━━━━━━━━━

<b>Mode:</b> {mode.upper()}
<b>Timeframe:</b> {timeframe}
<b>Price:</b> ${current_price:,.4f}

━━━━━━━━━━━━━━━━━━━━
{formatted}
━━━━━━━━━━━━━━━━━━━━

⚠️ <i>DYOR before trading!</i>
"""
        
        message_text = truncate_text(message_text, 4000)

        await safe_edit_message(
            query,
            message_text,
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

    except Exception as e:
        logging.error(f"Analysis error: {e}")
        await safe_edit_message(
            query,
            f"❌ <b>Gagal analisis</b>\n\nError: {str(e)}",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data=f"strategy_{timeframe}")],
                [InlineKeyboardButton("🏠 Menu", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


# =====================================================================
# ================= FUTURES EXECUTION (MANUAL INPUT) ==================
# =====================================================================

@only_allowed
async def handle_execute_futures(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Ask for position size"""
    query = update.callback_query
    await query.answer()

    analysis = context.user_data.get("analysis")
    if not analysis:
        await query.answer("❌ Data tidak ditemukan!", show_alert=True)
        return await start(update, context)

    try:
        from binance_futures import BinanceFuturesTrader
        futures_trader = BinanceFuturesTrader()
        
        balance = futures_trader.get_futures_balance()
        if not balance:
            raise Exception("Gagal mendapatkan balance")
        
        available = balance.get("availableBalance", 0)
        
        symbol_info = futures_trader.get_symbol_info(analysis["symbol"])
        min_notional = symbol_info.get("min_notional", 5.0) if symbol_info else 5.0
        max_leverage = symbol_info.get("max_leverage", 125) if symbol_info else 125
        
        context.user_data["available_balance"] = available
        context.user_data["min_notional"] = min_notional
        context.user_data["max_leverage"] = max_leverage

        await safe_edit_message(
            query,
            f"""
💰 <b>SETUP FUTURES - STEP 1/2</b>

<b>Balance:</b> ${available:.2f}
<b>Symbol:</b> {analysis['symbol']}

<b>Requirements:</b>
Min: ${min_notional:.2f}
Max: ${available:.2f}

━━━━━━━━━━━━━━━━━━━━

💵 <b>Berapa position size?</b>

Contoh: ketik <code>5</code> untuk $5

<i>Ketik angka di chat...</i>
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("❌ Batal", callback_data="execute_cancel")]
            ])
        )

    except Exception as e:
        logging.error(f"Error: {e}")
        await safe_edit_message(
            query,
            f"❌ Error: {str(e)}",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Kembali", callback_data="back_to_start")]
            ])
        )
        return SELECTING_MODE

    return INPUTTING_SIZE


@only_allowed
async def handle_size_input(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle position size input"""
    try:
        size_text = update.message.text.strip()
        position_size = float(size_text)
        
        available = context.user_data.get("available_balance", 0)
        min_notional = context.user_data.get("min_notional", 5.0)
        
        if position_size < min_notional:
            await update.message.reply_text(
                f"❌ <b>Terlalu kecil!</b>\n\nMin: ${min_notional:.2f}\nInput: ${position_size:.2f}",
                parse_mode="HTML"
            )
            return INPUTTING_SIZE
        
        if position_size > available:
            await update.message.reply_text(
                f"❌ <b>Melebihi balance!</b>\n\nAvailable: ${available:.2f}\nInput: ${position_size:.2f}",
                parse_mode="HTML"
            )
            return INPUTTING_SIZE
        
        context.user_data["position_size"] = position_size
        
        max_leverage = context.user_data.get("max_leverage", 125)
        pct = (position_size / available) * 100
        
        await update.message.reply_text(
            f"""
✅ <b>Size: ${position_size:.2f}</b> ({pct:.1f}%)

━━━━━━━━━━━━━━━━━━━━

⚡ <b>STEP 2/2 - Pilih Leverage</b>

Range: 1x - {max_leverage}x

Contoh: ketik <code>20</code> untuk 20x

⚠️ <b>Risk Level:</b>
• 1x-10x: Low Risk
• 10x-25x: Medium Risk
• 25x-50x: High Risk
• 50x+: Very High Risk

<i>Ketik angka leverage...</i>
""",
            parse_mode="HTML"
        )
        
        return INPUTTING_LEVERAGE
        
    except ValueError:
        await update.message.reply_text(
            "❌ <b>Format salah!</b>\n\nContoh: <code>5</code> atau <code>10.5</code>",
            parse_mode="HTML"
        )
        return INPUTTING_SIZE


@only_allowed
async def handle_leverage_input(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle leverage input"""
    try:
        leverage_text = update.message.text.strip()
        leverage = int(float(leverage_text))
        
        max_leverage = context.user_data.get("max_leverage", 125)
        position_size = context.user_data.get("position_size", 0)
        analysis = context.user_data.get("analysis")
        
        if leverage < 1:
            await update.message.reply_text(
                "❌ <b>Minimal 1x!</b>",
                parse_mode="HTML"
            )
            return INPUTTING_LEVERAGE
        
        if leverage > max_leverage:
            await update.message.reply_text(
                f"❌ <b>Maksimal {max_leverage}x!</b>\n\nInput: {leverage}x",
                parse_mode="HTML"
            )
            return INPUTTING_LEVERAGE
        
        context.user_data["leverage"] = leverage
        
        if leverage <= 10:
            risk_level = "🟢 LOW RISK"
        elif leverage <= 25:
            risk_level = "🟡 MEDIUM RISK"
        elif leverage <= 50:
            risk_level = "🟠 HIGH RISK"
        else:
            risk_level = "🔴 VERY HIGH RISK"
        
        await update.message.reply_text(
            f"""
📋 <b>KONFIRMASI ORDER</b>

━━━━━━━━━━━━━━━━━━━━

<b>Symbol:</b> {analysis['symbol']}
<b>Size:</b> ${position_size:.2f}
<b>Leverage:</b> {leverage}x
<b>Risk:</b> {risk_level}

━━━━━━━━━━━━━━━━━━━━

<b>Lanjutkan?</b>
""",
            parse_mode="HTML",
            reply_markup=InlineKeyboardMarkup([
                [
                    InlineKeyboardButton("✅ YA, EXECUTE!", callback_data="execute_confirm_manual"),
                    InlineKeyboardButton("❌ BATAL", callback_data="execute_cancel")
                ]
            ])
        )
        
        return SELECTING_MODE
        
    except ValueError:
        await update.message.reply_text(
            "❌ <b>Format salah!</b>\n\nContoh: <code>10</code> atau <code>20</code>",
            parse_mode="HTML"
        )
        return INPUTTING_LEVERAGE


# Continue to Part 3...

