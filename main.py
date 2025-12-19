# main.py - UPGRADED VERSION WITH SPOT, FUTURES & AUTO TRADING
import logging
from telegram import Update, InlineKeyboardButton, InlineKeyboardMarkup
from telegram.ext import (
    ApplicationBuilder, CommandHandler, CallbackQueryHandler, 
    MessageHandler, ConversationHandler, ContextTypes, filters
)
from binance.client import Client
from config import TELEGRAM_TOKEN, BINANCE_API_KEY, BINANCE_API_SECRET, TOP_TOKENS
from utils import only_allowed, format_result_for_telegram
from ai import analyze_with_gpt
from auto_trading import AutoTradingBot
import asyncio


# Tambahkan di bagian atas main.py (setelah import yang sudah ada)
from shared_state import SharedState

# Tambahkan setelah inisialisasi binance_client
shared_state = SharedState()

# Tambahkan fungsi-fungsi monitoring ini:

@only_allowed
async def show_auto_trading_menu(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show Auto Trading monitoring menu"""
    query = update.callback_query
    
    # Get current status from shared state
    state = shared_state.get_state()
    balance = state.get('balance', {'total': 0, 'available': 0, 'unrealized_pnl': 0})
    is_running = state.get('is_running', False)
    stats = state.get('stats', {})
    
    status_emoji = "🟢" if is_running else "🔴"
    status_text = "RUNNING" if is_running else "STOPPED"
    
    menu_text = f"""
🤖 <b>AUTO TRADING MONITOR</b>

<b>Status: {status_emoji} {status_text}</b>

💰 <b>Balance:</b>
- Total: ${balance.get('total', 0):.2f}
- Available: ${balance.get('available', 0):.2f}
- PnL: ${balance.get('unrealized_pnl', 0):.2f}

📊 <b>Stats:</b>
- Total Trades: {stats.get('total_trades', 0)}
- Win: {stats.get('winning_trades', 0)} | Loss: {stats.get('losing_trades', 0)}
- Profit: ${stats.get('total_profit', 0):.2f}
- Loss: ${stats.get('total_loss', 0):.2f}

⏰ Last Update: {state.get('last_update', 'N/A')}

<i>💡 Auto trading berjalan di terminal VPS</i>
"""
    
    keyboard = [
        [
            InlineKeyboardButton("📊 Positions", callback_data="monitor_positions"),
            InlineKeyboardButton("📜 Trade Log", callback_data="monitor_log")
        ],
        [
            InlineKeyboardButton("🔄 Refresh", callback_data="mode_auto"),
            InlineKeyboardButton("📈 Performance", callback_data="monitor_performance")
        ],
        [
            InlineKeyboardButton("⚠️ Errors", callback_data="monitor_errors"),
            InlineKeyboardButton("🔙 Back", callback_data="back_to_start")
        ]
    ]
    
    reply_markup = InlineKeyboardMarkup(keyboard)
    
    await query.edit_message_text(
        menu_text,
        parse_mode="HTML",
        reply_markup=reply_markup
    )
    
    return SELECTING_MODE

@only_allowed
async def handle_monitor_actions(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle monitoring actions"""
    query = update.callback_query
    await query.answer()
    
    action = query.data.split("_")[1]
    state = shared_state.get_state()
    
    if action == "positions":
        positions = state.get('open_positions', [])
        
        if not positions:
            text = "📊 <b>Open Positions</b>\n\nNo open positions."
        else:
            text = "📊 <b>Open Positions</b>\n\n"
            for pos in positions:
                pnl_emoji = "🟢" if pos.get('unrealized_pnl', 0) > 0 else "🔴"
                text += f"""
<b>{pos['symbol']}</b>
Side: {pos['side']}
Entry: ${pos['entry_price']:.4f}
Mark: ${pos['mark_price']:.4f}
PnL: {pnl_emoji} ${pos['unrealized_pnl']:.2f}
Leverage: {pos['leverage']}x
━━━━━━━━━━━━━━━
"""
        
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(text, parse_mode="HTML", reply_markup=reply_markup)
        return SELECTING_MODE
    
    elif action == "log":
        trades = state.get('trade_log', [])
        
        if not trades:
            text = "📜 <b>Trade Log</b>\n\nNo trades yet."
        else:
            text = "📜 <b>Trade Log (Last 10)</b>\n\n"
            for trade in list(reversed(trades))[-10:]:
                status_emoji = "🟢" if trade.get('status') == 'CLOSED' else "🔵"
                text += f"""
{status_emoji} <b>{trade['symbol']}</b> - {trade['side']}
Entry: ${trade['entry_price']:.4f}
TP: ${trade['tp_price']:.4f}
SL: ${trade['sl_price']:.4f}
Size: ${trade['position_size']:.2f}
Confidence: {trade['confidence']}%
Time: {trade['timestamp']}
━━━━━━━━━━━━━━━
"""
        
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(text, parse_mode="HTML", reply_markup=reply_markup)
        return SELECTING_MODE
    
    elif action == "performance":
        stats = state.get('stats', {})
        balance = state.get('balance', {})
        
        total_trades = stats.get('total_trades', 0)
        wins = stats.get('winning_trades', 0)
        losses = stats.get('losing_trades', 0)
        win_rate = (wins / total_trades * 100) if total_trades > 0 else 0
        
        total_profit = stats.get('total_profit', 0)
        total_loss = stats.get('total_loss', 0)
        net_profit = total_profit - total_loss
        
        text = f"""
📈 <b>PERFORMANCE REPORT</b>

💰 <b>Balance:</b>
Total: ${balance.get('total', 0):.2f}
Available: ${balance.get('available', 0):.2f}

📊 <b>Trading Stats:</b>
Total Trades: {total_trades}
Wins: {wins} ({win_rate:.1f}%)
Losses: {losses}

💵 <b>P&L:</b>
Profit: ${total_profit:.2f}
Loss: ${total_loss:.2f}
Net: ${net_profit:.2f}

⏰ Started: {state.get('started_at', 'N/A')}
"""
        
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(text, parse_mode="HTML", reply_markup=reply_markup)
        return SELECTING_MODE
    
    elif action == "errors":
        errors = state.get('errors', [])
        
        if not errors:
            text = "⚠️ <b>Error Log</b>\n\nNo errors."
        else:
            text = "⚠️ <b>Error Log (Last 10)</b>\n\n"
            for error in list(reversed(errors))[-10:]:
                text += f"""
🔴 {error['timestamp']}
{error['message']}
━━━━━━━━━━━━━━━
"""
        
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(text, parse_mode="HTML", reply_markup=reply_markup)
        return SELECTING_MODE

# Update button_handler, tambahkan:
async def button_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    query = update.callback_query
    await query.answer()
    
    if query.data == "back_to_start":
        return await start(update, context)
    elif query.data == "help":
        return await help_command(update, context)
    elif query.data == "mode_auto":
        return await show_auto_trading_menu(update, context)
    elif query.data.startswith("monitor_"):
        return await handle_monitor_actions(update, context)
    elif query.data.startswith("mode_"):
        return await handle_mode_selection(update, context)
    elif query.data.startswith("token_"):
        return await handle_token_selection(update, context)
    elif query.data.startswith("strategy_"):
        return await handle_strategy_selection(update, context)

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

binance_client = Client(api_key=BINANCE_API_KEY, api_secret=BINANCE_API_SECRET)

# Global auto trading bot instance
auto_bot = AutoTradingBot()

# Conversation states
SELECTING_MODE, SELECTING_TOKEN, SELECTING_STRATEGY = range(3)

@only_allowed
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk command /start dengan menu SPOT, FUTURES & AUTO TRADING"""
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

Selamat datang! Bot ini memberikan sinyal trading crypto dengan analisis AI yang akurat menggunakan Groq Llama 3.3 70B.

<b>📊 Pilih Mode Trading:</b>
• <b>SPOT</b> - Trading spot dengan risk rendah
• <b>FUTURES</b> - Trading futures manual dengan leverage
• <b>AUTO TRADING</b> - Auto trading futures dengan AI

Silakan pilih mode di bawah ini:
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

@only_allowed
async def handle_mode_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk memilih mode SPOT, FUTURES, atau AUTO"""
    query = update.callback_query
    await query.answer()
    
    mode = query.data.split("_")[1]  # spot, futures, atau auto
    
    # Handle Auto Trading mode
    if mode == "auto":
        return await show_auto_trading_menu(update, context)
    
    context.user_data["mode"] = mode
    
    # Buat keyboard untuk pilihan token (5 kolom)
    buttons = []
    for i in range(0, len(TOP_TOKENS[:50]), 5):
        row = [
            InlineKeyboardButton(token.replace("USDT", ""), callback_data=f"token_{token}")
            for token in TOP_TOKENS[i:i+5]
        ]
        buttons.append(row)
    
    # Tombol kembali
    buttons.append([InlineKeyboardButton("🔙 Kembali", callback_data="back_to_start")])
    
    reply_markup = InlineKeyboardMarkup(buttons)
    
    mode_emoji = "💼" if mode == "spot" else "📈"
    mode_text = "SPOT" if mode == "spot" else "FUTURES"
    
    await query.edit_message_text(
        f"{mode_emoji} <b>MODE: {mode_text} TRADING</b>\n\n"
        f"📌 Pilih token yang ingin dianalisis:\n"
        f"<i>Atau ketik manual seperti: BTCUSDT</i>",
        parse_mode="HTML",
        reply_markup=reply_markup
    )
    
    return SELECTING_TOKEN

@only_allowed
async def show_auto_trading_menu(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show Auto Trading menu"""
    query = update.callback_query
    
    # Get current status
    status = auto_bot.get_status()
    balance = status['balance']
    is_running = status['is_running']
    
    status_emoji = "🟢" if is_running else "🔴"
    status_text = "RUNNING" if is_running else "STOPPED"
    
    menu_text = f"""
🤖 <b>AUTO TRADING FUTURES</b>

<b>Status: {status_emoji} {status_text}</b>

💰 <b>Balance Info:</b>
• Total: ${balance['total_balance']:.2f} USDT
• Available: ${balance['available_balance']:.2f} USDT
• Unrealized PnL: ${balance['unrealized_pnl']:.2f}

⚙️ <b>Config:</b>
• Leverage: {status['config']['max_leverage']}x
• Position Size: {status['config']['position_size_pct']}%
• Max Loss: {status['config']['max_loss_pct']}%
• Min Confidence: {status['config']['min_confidence']}%
• Timeframe: {status['config']['timeframe']}

📊 <b>Stats:</b>
• Total Trades: {status['total_trades']}
• Open Positions: {len(status['open_positions']) if status['open_positions'] else 0}
"""
    
    # Build keyboard based on status
    if is_running:
        keyboard = [
            [InlineKeyboardButton("🔴 STOP Auto Trading", callback_data="auto_stop")],
            [InlineKeyboardButton("📊 View Positions", callback_data="auto_positions")],
            [InlineKeyboardButton("📜 Trade Log", callback_data="auto_log")],
            [InlineKeyboardButton("🚨 Emergency Close All", callback_data="auto_emergency")],
            [InlineKeyboardButton("🔙 Back to Menu", callback_data="back_to_start")]
        ]
    else:
        keyboard = [
            [InlineKeyboardButton("🟢 START Auto Trading", callback_data="auto_start")],
            [InlineKeyboardButton("⚙️ Settings", callback_data="auto_settings")],
            [InlineKeyboardButton("📜 Trade Log", callback_data="auto_log")],
            [InlineKeyboardButton("🔙 Back to Menu", callback_data="back_to_start")]
        ]
    
    reply_markup = InlineKeyboardMarkup(keyboard)
    
    await query.edit_message_text(
        menu_text,
        parse_mode="HTML",
        reply_markup=reply_markup
    )
    
    return SELECTING_MODE

@only_allowed
async def handle_auto_trading_actions(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle auto trading actions"""
    query = update.callback_query
    await query.answer()
    
    action = query.data.split("_")[1]
    
    if action == "start":
        auto_bot.start()
        
        # Start the trading loop in background
        asyncio.create_task(auto_bot.scan_and_trade())
        
        await query.answer("🟢 Auto Trading Started!", show_alert=True)
        return await show_auto_trading_menu(update, context)
    
    elif action == "stop":
        auto_bot.stop()
        await query.answer("🔴 Auto Trading Stopped!", show_alert=True)
        return await show_auto_trading_menu(update, context)
    
    elif action == "positions":
        status = auto_bot.get_status()
        positions = status['open_positions']
        
        if not positions:
            positions_text = "📊 <b>Open Positions</b>\n\nNo open positions."
        else:
            positions_text = "📊 <b>Open Positions</b>\n\n"
            for pos in positions:
                pnl_emoji = "🟢" if pos['unrealized_pnl'] > 0 else "🔴"
                positions_text += f"""
<b>{pos['symbol']}</b>
Side: {pos['side']}
Entry: ${pos['entry_price']:.4f}
Mark: ${pos['mark_price']:.4f}
PnL: {pnl_emoji} ${pos['unrealized_pnl']:.2f}
Leverage: {pos['leverage']}x
━━━━━━━━━━━━━━━
"""
        
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(
            positions_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
        return SELECTING_MODE
    
    elif action == "log":
        trades = auto_bot.get_trade_log(limit=10)
        
        if not trades:
            log_text = "📜 <b>Trade Log</b>\n\nNo trades yet."
        else:
            log_text = "📜 <b>Trade Log (Last 10)</b>\n\n"
            for trade in reversed(trades):
                status_emoji = "🟢" if trade['status'] == 'CLOSED' else "🔵"
                log_text += f"""
{status_emoji} <b>{trade['symbol']}</b> - {trade['side']}
Entry: ${trade['entry_price']:.4f}
Size: ${trade['position_size']:.2f}
Confidence: {trade['confidence']}%
Time: {trade['timestamp']}
━━━━━━━━━━━━━━━
"""
        
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(
            log_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
        return SELECTING_MODE
    
    elif action == "emergency":
        keyboard = [
            [
                InlineKeyboardButton("✅ YES, Close All", callback_data="auto_emergency_confirm"),
                InlineKeyboardButton("❌ Cancel", callback_data="mode_auto")
            ]
        ]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(
            "🚨 <b>EMERGENCY CLOSE ALL</b>\n\n"
            "⚠️ This will close ALL open positions immediately!\n\n"
            "Are you sure?",
            parse_mode="HTML",
            reply_markup=reply_markup
        )
        return SELECTING_MODE
    
    elif action == "emergency_confirm":
        auto_bot.emergency_close_all()
        await query.answer("🚨 All positions closed!", show_alert=True)
        return await show_auto_trading_menu(update, context)
    
    elif action == "settings":
        settings_text = """
⚙️ <b>AUTO TRADING SETTINGS</b>

Coming soon! For now, edit settings in auto_trading.py config.

Current Settings:
• Max Leverage: 20x
• Position Size: 15% of balance
• Max Loss: 17.5% of balance
• Min Confidence: 85%
• Timeframe: 1h
• Scan Interval: 5 minutes
"""
        keyboard = [[InlineKeyboardButton("🔙 Back", callback_data="mode_auto")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(
            settings_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
        return SELECTING_MODE

@only_allowed
async def handle_token_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk memilih token"""
    query = update.callback_query
    await query.answer()
    
    token = query.data.split("_")[1]
    context.user_data["symbol"] = token
    
    mode = context.user_data.get("mode", "spot")
    mode_emoji = "💼" if mode == "spot" else "📈"
    
    # Keyboard strategi trading
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
        [InlineKeyboardButton("🔙 Kembali", callback_data=f"mode_{mode}")]
    ]
    
    reply_markup = InlineKeyboardMarkup(keyboard)
    
    await query.edit_message_text(
        f"{mode_emoji} <b>{token}</b>\n\n"
        f"⏰ Pilih timeframe untuk analisis:",
        parse_mode="HTML",
        reply_markup=reply_markup
    )
    
    return SELECTING_STRATEGY

@only_allowed
async def handle_manual_token(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk token yang diketik manual"""
    token = update.message.text.upper().strip()
    
    if not token.endswith("USDT"):
        token += "USDT"
    
    context.user_data["symbol"] = token
    mode = context.user_data.get("mode", "spot")
    mode_emoji = "💼" if mode == "spot" else "📈"
    
    # Keyboard strategi trading
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
        [InlineKeyboardButton("🔙 Kembali", callback_data=f"mode_{mode}")]
    ]
    
    reply_markup = InlineKeyboardMarkup(keyboard)
    
    await update.message.reply_text(
        f"{mode_emoji} <b>{token}</b>\n\n"
        f"⏰ Pilih timeframe untuk analisis:",
        parse_mode="HTML",
        reply_markup=reply_markup
    )
    
    return SELECTING_STRATEGY

@only_allowed
async def handle_strategy_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk analisis berdasarkan strategi"""
    query = update.callback_query
    await query.answer()
    
    timeframe = query.data.split("_")[1]
    symbol = context.user_data.get("symbol")
    mode = context.user_data.get("mode", "spot")
    
    mode_emoji = "💼" if mode == "spot" else "📈"
    mode_text = "SPOT" if mode == "spot" else "FUTURES"
    
    await query.edit_message_text(
        f"{mode_emoji} <b>Menganalisis {symbol}</b>\n\n"
        f"Mode: {mode_text}\n"
        f"Timeframe: {timeframe}\n\n"
        f"⏳ Sedang mengambil data market...",
        parse_mode="HTML"
    )
    
    try:
        # Ambil data dari Binance
        klines = binance_client.get_klines(
            symbol=symbol, 
            interval=timeframe, 
            limit=100
        )
        
        if not klines:
            raise Exception("Data tidak tersedia untuk symbol ini")
        
        # Format OHLCV
        ohlc = [{
            "time": k[0],
            "open": float(k[1]),
            "high": float(k[2]),
            "low": float(k[3]),
            "close": float(k[4]),
            "volume": float(k[5])
        } for k in klines]
        
        # Update status
        await query.edit_message_text(
            f"{mode_emoji} <b>Menganalisis {symbol}</b>\n\n"
            f"Mode: {mode_text}\n"
            f"Timeframe: {timeframe}\n\n"
            f"🤖 AI sedang menganalisis data...\n"
            f"⏱️ Mohon tunggu 10-20 detik...",
            parse_mode="HTML"
        )
        
        # Analisis dengan AI
        gpt_result = analyze_with_gpt(symbol, timeframe, ohlc[-1], mode)
        formatted_result = format_result_for_telegram(gpt_result)
        
        # Header hasil
        header = f"""
{mode_emoji} <b>ANALISIS {mode_text} - {symbol}</b>
⏰ Timeframe: {timeframe}
💰 Price: ${ohlc[-1]['close']:,.2f}

━━━━━━━━━━━━━━━━━━━━
"""
        
        # Kirim hasil
        result_message = header + formatted_result
        
        # Buat keyboard untuk analisis ulang
        keyboard = [
            [
                InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{timeframe}"),
                InlineKeyboardButton("🔙 Token Lain", callback_data=f"mode_{mode}")
            ],
            [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
        ]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(
            result_message,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
        
    except Exception as e:
        error_msg = f"❌ <b>Terjadi Kesalahan</b>\n\n{str(e)}"
        
        keyboard = [
            [InlineKeyboardButton("🔙 Coba Lagi", callback_data=f"mode_{mode}")],
            [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
        ]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await query.edit_message_text(
            error_msg,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
    
    return SELECTING_MODE

@only_allowed
async def help_command(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk help"""
    query = update.callback_query
    if query:
        await query.answer()
    
    help_text = """
📚 <b>PANDUAN PENGGUNAAN BOT</b>

<b>🎯 MODE TRADING:</b>
• <b>SPOT</b> - Trading tanpa leverage, risk lebih rendah
• <b>FUTURES</b> - Trading futures manual dengan leverage
• <b>AUTO TRADING</b> - Bot otomatis scan & execute trade

<b>⏰ TIMEFRAME:</b>
• <b>Scalping (15m)</b> - Trading cepat, hold 15-60 menit
• <b>Day Trade (1h)</b> - Trading harian, hold beberapa jam
• <b>Swing (1d)</b> - Trading jangka menengah, hold beberapa hari
• <b>Long-Term (1w)</b> - Investment jangka panjang
• <b>Multi TF (4h)</b> - Analisis multi timeframe

<b>🤖 AUTO TRADING:</b>
• Bot scan market setiap 5 menit
• Hanya execute signal dengan confidence ≥85%
• Auto TP/SL via Binance
• Max 1 position open
• Position size: 15% balance
• Max loss: ~17.5% balance per trade

<b>⚠️ DISCLAIMER:</b>
Bot ini hanya memberikan referensi. Trading crypto berisiko tinggi! Gunakan dana yang siap hilang.

<b>💡 TIPS:</b>
• Start dengan capital kecil
• Monitor bot secara berkala
• Gunakan emergency stop jika perlu
"""
    
    keyboard = [[InlineKeyboardButton("🔙 Kembali", callback_data="back_to_start")]]
    reply_markup = InlineKeyboardMarkup(keyboard)
    
    if query:
        await query.edit_message_text(
            help_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
    else:
        await update.message.reply_text(
            help_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )

async def button_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk semua callback button"""
    query = update.callback_query
    await query.answer()
    
    if query.data == "back_to_start":
        return await start(update, context)
    elif query.data == "help":
        return await help_command(update, context)
    elif query.data.startswith("mode_"):
        return await handle_mode_selection(update, context)
    elif query.data.startswith("token_"):
        return await handle_token_selection(update, context)
    elif query.data.startswith("strategy_"):
        return await handle_strategy_selection(update, context)
    elif query.data.startswith("auto_"):
        return await handle_auto_trading_actions(update, context)

def main():
    """Main function untuk menjalankan bot"""
    app = ApplicationBuilder().token(TELEGRAM_TOKEN).build()
    
    # Conversation handler
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
            CallbackQueryHandler(button_handler)
        ],
        allow_reentry=True
    )
    
    # Add handlers
    app.add_handler(conv_handler)
    app.add_handler(CommandHandler("help", help_command))
    
    # Start bot
    logging.info("🤖 Bot started successfully!")
    logging.info("✅ Waiting for commands...")
    app.run_polling(allowed_updates=Update.ALL_TYPES)

if __name__ == "__main__":
    main()
