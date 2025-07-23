import re
from telegram import Update
from telegram.ext import ContextTypes
from config import ALLOWED_USER_IDS
import html

def format_number(val):
    """
    Format angka agar lebih mudah dibaca berdasarkan rentangnya.
    """
    try:
        val = float(val)
        if val < 0.01:
            return f"{val:.8f}".rstrip("0").rstrip(".")
        elif val < 1:
            return f"{val:.6f}".rstrip("0").rstrip(".")
        elif val < 100:
            return f"{val:.4f}".rstrip("0").rstrip(".")
        else:
            return f"{val:.2f}".rstrip("0").rstrip(".")
    except:
        return str(val)

def only_allowed(func):
    """
    Dekorator untuk membatasi akses hanya ke user ID yang diizinkan.
    """
    async def wrapper(update: Update, context: ContextTypes.DEFAULT_TYPE):
        if update.effective_user.id not in ALLOWED_USER_IDS:
            await update.message.reply_text("⛔ Maaf, kamu tidak memiliki akses ke bot ini.")
            return
        return await func(update, context)
    return wrapper

def format_result_for_telegram(text: str) -> str:
    """
    Format hasil analisa agar rapi di Telegram, termasuk:
    - Mengubah angka scientific ke desimal
    - Replace keyword menjadi bold/emoji
    - Escape HTML characters
    """
    def sci_to_decimal(match):
        try:
            return format_number(float(match.group()))
        except:
            return match.group()

    # Convert angka scientific notation (misal 1.2e-05) ke format desimal
    text = re.sub(r"\b\d+\.\d+e[+-]?\d+\b", sci_to_decimal, text, flags=re.IGNORECASE)

    # Escape HTML dulu biar aman, lalu kita tambah <b>/<u> yang valid
    text = html.escape(text)

    replacements = {
        "**FUTURES**": "<b>📊 REKOMENDASI SETUP FUTURES</b>",
        "**SPOT**": "<b>💼 REKOMENDASI SETUP SPOT</b>",
        "**Catatan**": "<b>🧠 CATATAN TAMBAHAN</b>",
        "**Risk Reward Ratio**": "<b>📈 RISK REWARD RATIO</b>",
        "**Confidence Level**": "<b>🔐 CONFIDENCE LEVEL</b>",
        "**Sinyal Aksi**": "<b>🔔 SINYAL AKSI</b>",
        "**Analisis tren pasar**": "<b><u>📊 ANALISIS TREN PASAR SAAT INI</u></b>"
    }

    for key, val in replacements.items():
        escaped_key = html.escape(key)
        text = text.replace(escaped_key, val)

    return text
