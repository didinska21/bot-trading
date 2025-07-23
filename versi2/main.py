import logging
from telegram import Update, ReplyKeyboardMarkup
from telegram.ext import (
    ApplicationBuilder, CommandHandler, MessageHandler,
    ConversationHandler, ContextTypes, filters
)
from binance.client import Client
from config import TELEGRAM_TOKEN, BINANCE_API_KEY, BINANCE_API_SECRET, TOP_TOKENS
from utils import only_allowed, format_result_for_telegram
from ai import analyze_with_gpt

# Konfigurasi logging
logging.basicConfig(level=logging.INFO)

# Inisialisasi Binance client
binance_client = Client(api_key=BINANCE_API_KEY, api_secret=BINANCE_API_SECRET)

# Status untuk ConversationHandler
SELECTING_TIMEFRAME = range(1)

@only_allowed
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    await update.message.reply_text("👋 Selamat datang di bot analisa crypto!")
    buttons = [[token] for token in TOP_TOKENS[:100]]
    markup = ReplyKeyboardMarkup(buttons, resize_keyboard=True, one_time_keyboard=True)
    await update.message.reply_text("📌 Pilih token dari daftar atau ketik manual (contoh: BTCUSDT):", reply_markup=markup)

@only_allowed
async def handle_symbol(update: Update, context: ContextTypes.DEFAULT_TYPE):
    symbol = update.message.text.upper().strip()
    context.user_data["symbol"] = symbol
    reply_keyboard = [
        ["Scalping", "Day Trading"],
        ["Swing Trading", "Long-Term"],
        ["Multi Timeframe"]
    ]
    await update.message.reply_text(
        f"📈 Pilih strategi trading untuk {symbol}:",
        reply_markup=ReplyKeyboardMarkup(reply_keyboard, resize_keyboard=True, one_time_keyboard=True)
    )
    return SELECTING_TIMEFRAME

@only_allowed
async def handle_timeframe_selection(update: Update, context: ContextTypes.DEFAULT_TYPE):
    timeframe_map = {
        "Scalping": "15m",
        "Day Trading": "1h",
        "Swing Trading": "1d",
        "Long-Term": "1w",
        "Multi Timeframe": "4h"
    }
    category = update.message.text
    symbol = context.user_data.get("symbol")
    interval = timeframe_map.get(category, "1h")
    
    await update.message.reply_text(f"📥 Mengambil data {symbol} pada timeframe {interval}...")

    try:
        klines = binance_client.get_klines(symbol=symbol, interval=interval, limit=50)
        if not klines:
            raise Exception("Data tidak tersedia atau simbol salah.")
        
        ohlc = [{
            "time": k[0],
            "open": float(k[1]),
            "high": float(k[2]),
            "low": float(k[3]),
            "close": float(k[4]),
            "volume": float(k[5])
        } for k in klines]

        await update.message.reply_text("🔎 Menganalisis data pasar secara real-time...")
        
        gpt_result = analyze_with_gpt(symbol, interval, ohlc[-1])
        formatted_result = format_result_for_telegram(gpt_result)
        await update.message.reply_text(formatted_result, parse_mode="HTML")
        
    except Exception as e:
        logging.exception("❌ Error saat analisa:")
        await update.message.reply_text(f"❌ Gagal mengambil data atau analisa.\n{e}")
    
    return ConversationHandler.END

def main():
    app = ApplicationBuilder().token(TELEGRAM_TOKEN).build()
    
    conv_handler = ConversationHandler(
        entry_points=[MessageHandler(filters.TEXT & ~filters.COMMAND, handle_symbol)],
        states={SELECTING_TIMEFRAME: [MessageHandler(filters.TEXT & ~filters.COMMAND, handle_timeframe_selection)]},
        fallbacks=[]
    )
    
    app.add_handler(CommandHandler("start", start))
    app.add_handler(conv_handler)
    
    logging.info("✅ Bot Telegram aktif! Menunggu perintah...")
    app.run_polling()

if __name__ == "__main__":
    main()
