# main.py - UPGRADED VERSION WITH SPOT & FUTURES
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

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

binance_client = Client(api_key=BINANCE_API_KEY, api_secret=BINANCE_API_SECRET)

# Conversation states
SELECTING_MODE, SELECTING_TOKEN, SELECTING_STRATEGY = range(3)

@only_allowed
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk command /start dengan menu SPOT & FUTURES"""
    keyboard = [
        [
            InlineKeyboardButton("💼 SPOT TRADING", callback_data="mode_spot"),
            InlineKeyboardButton("📈 FUTURES TRADING", callback_data="mode_futures")
        ],
        [InlineKeyboardButton("ℹ️ Help & Info", callback_data="help")]
    ]
    reply_markup = InlineKeyboardMarkup(keyboard)
    
    welcome_text = """
🤖 <b>CRYPTO SIGNAL BOT - AI POWERED</b>

Selamat datang! Bot ini memberikan sinyal trading crypto dengan analisis AI yang akurat menggunakan Groq Llama 3.3 70B.

<b>📊 Pilih Mode Trading:</b>
• <b>SPOT</b> - Trading spot dengan risk rendah
• <b>FUTURES</b> - Trading futures dengan leverage

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
    """Handler untuk memilih mode SPOT atau FUTURES"""
    query = update.callback_query
    await query.answer()
    
    mode = query.data.split("_")[1]  # spot atau futures
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
• <b>FUTURES</b> - Trading dengan leverage, risk tinggi tapi profit besar

<b>⏰ TIMEFRAME:</b>
• <b>Scalping (15m)</b> - Trading cepat, hold 15-60 menit
• <b>Day Trade (1h)</b> - Trading harian, hold beberapa jam
• <b>Swing (1d)</b> - Trading jangka menengah, hold beberapa hari
• <b>Long-Term (1w)</b> - Investment jangka panjang
• <b>Multi TF (4h)</b> - Analisis multi timeframe

<b>📊 INDIKATOR YANG DIGUNAKAN:</b>
• RSI - Relative Strength Index
• MACD - Moving Average Convergence Divergence
• EMA - Exponential Moving Average
• Volume & OBV Analysis
• Support/Resistance Levels

<b>⚠️ DISCLAIMER:</b>
Bot ini hanya memberikan referensi analisis. Keputusan trading sepenuhnya tanggung jawab Anda. Selalu gunakan risk management!

<b>💡 TIPS:</b>
• Gunakan stop loss
• Jangan FOMO atau panic sell
• Diversifikasi portfolio
• Risk max 2-5% per trade
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

@only_allowed
async def stats_command(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handler untuk command /stats"""
    try:
        # Ambil market overview
        tickers = binance_client.get_ticker()
        btc = next((t for t in tickers if t['symbol'] == 'BTCUSDT'), None)
        eth = next((t for t in tickers if t['symbol'] == 'ETHUSDT'), None)
        
        stats_text = f"""
📊 <b>MARKET OVERVIEW</b>

<b>Bitcoin (BTC)</b>
💰 Price: ${float(btc['lastPrice']):,.2f}
📈 24h Change: {float(btc['priceChangePercent']):.2f}%
📊 24h Volume: ${float(btc['volume']):,.0f}

<b>Ethereum (ETH)</b>
💰 Price: ${float(eth['lastPrice']):,.2f}
📈 24h Change: {float(eth['priceChangePercent']):.2f}%
📊 24h Volume: ${float(eth['volume']):,.0f}

⏰ Last Update: {update.message.date.strftime('%Y-%m-%d %H:%M:%S')}
"""
        
        keyboard = [[InlineKeyboardButton("🔄 Refresh", callback_data="refresh_stats")]]
        reply_markup = InlineKeyboardMarkup(keyboard)
        
        await update.message.reply_text(
            stats_text,
            parse_mode="HTML",
            reply_markup=reply_markup
        )
    except Exception as e:
        await update.message.reply_text(f"❌ Error: {str(e)}")

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
    app.add_handler(CommandHandler("stats", stats_command))
    
    # Start bot
    logging.info("🤖 Bot started successfully!")
    logging.info("✅ Waiting for commands...")
    app.run_polling(allowed_updates=Update.ALL_TYPES)

if __name__ == "__main__":
    main()
