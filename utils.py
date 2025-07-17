from telegram import Update
from telegram.ext import ContextTypes
from config import ALLOWED_USER_IDS

def format_number(val):
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
    async def wrapper(update: Update, context: ContextTypes.DEFAULT_TYPE):
        if update.effective_user.id not in ALLOWED_USER_IDS:
            await update.message.reply_text("⛔ Maaf, kamu tidak memiliki akses ke bot ini.")
            return
        return await func(update, context)
    return wrapper

import re
def format_result_for_telegram(text: str) -> str:
    def sci_to_decimal(match):
        try:
            return format_number(float(match.group()))
        except:
            return match.group()

    text = re.sub(r"\b\d+\.\d+e[+-]?\d+\b", sci_to_decimal, text, flags=re.IGNORECASE)

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
        text = text.replace(key, val)

    return text
