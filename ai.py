# ai.py - GROQ API VERSION (FIXED, NO LOGIC CHANGE)

from groq import Groq
from config import GROQ_API_KEY
from utils import format_number
import logging

logger = logging.getLogger(__name__)

# Initialize Groq client
client = Groq(api_key=GROQ_API_KEY)


def analyze_with_gpt(symbol: str, timeframe: str, last_candle: dict, mode: str = "spot") -> str:
    """
    Analisis crypto dengan AI Groq berdasarkan mode (spot/futures)
    """

    # ================= SYSTEM PROMPT =================

    if mode == "futures":
        system_prompt = """Kamu adalah analis crypto profesional expert dalam FUTURES TRADING dengan akurasi tinggi.
Kamu ahli dalam analisis teknikal mendalam seperti Willy Woo (on-chain) dan CryptoJelleNL (trading aktif).
Fokus pada setup FUTURES dengan leverage, multiple TP, dan risk management ketat.
Berikan analisis yang akurat, data-driven, dan profesional."""
    else:
        system_prompt = """Kamu adalah analis crypto profesional expert dalam SPOT TRADING dengan akurasi tinggi.
Kamu ahli dalam analisis teknikal untuk buy & hold strategy, swing trading spot, dan investment jangka menengah-panjang.
Fokus pada entry yang aman dengan risk/reward yang baik untuk spot trading.
Berikan analisis yang akurat, data-driven, dan profesional."""

    # ================= USER PROMPT =================

    if mode == "futures":
        user_prompt = f"""
🎯 ANALISIS FUTURES TRADING

Simbol: {symbol}
Timeframe: {timeframe}
Mode: FUTURES TRADING

📊 Data Harga Terakhir:
- Open: {format_number(last_candle['open'])}
- High: {format_number(last_candle['high'])}
- Low: {format_number(last_candle['low'])}
- Close: {format_number(last_candle['close'])}
- Volume: {format_number(last_candle['volume'])}

Berikan analisis LENGKAP dengan format berikut (gunakan format Markdown dengan ** untuk bold):

**Analisis tren pasar**
(Analisis struktur harga, volume, momentum, RSI, MACD, EMA crossover, OBV, support/resistance kunci)

**FUTURES**
🎯 Posisi: [LONG / SHORT / WAIT]
🟢 Entry Range: $X.XX - $X.XX
⚡ Leverage: Xx (rekomendasi 3x-10x untuk risk moderate)
🎯 Target:
   • TP1: $X.XX (30% profit take)
   • TP2: $X.XX (40% profit take)
   • TP3: $X.XX (30% profit take)
🔴 Stop Loss: $X.XX (maksimal -2% hingga -5%)
⚖️ Risk/Reward: 1:X

**Risk Reward Ratio**
Berikan perhitungan R/R ratio yang detail dan estimasi profit/loss dalam %.

**Catatan**
- Support/resistance levels penting
- Validasi konfluensi indikator (minimal 3 indikator align)
- Anomali volume atau divergence
- Potensi fakeout atau bull/bear trap
- Market sentiment dan context

**Sinyal Aksi**
[STRONG BUY / BUY / HOLD / SELL / STRONG SELL]

**Confidence Level**
X% (berikan alasan kenapa confidence level ini)

⚠️ PENTING:
- Jika Confidence Level < 85%, JANGAN berikan setup futures.
- Ganti dengan penjelasan kenapa market sedang tidak ideal.
"""

    else:
        user_prompt = f"""
🎯 ANALISIS SPOT TRADING

Simbol: {symbol}
Timeframe: {timeframe}
Mode: SPOT TRADING

📊 Data Harga Terakhir:
- Open: {format_number(last_candle['open'])}
- High: {format_number(last_candle['high'])}
- Low: {format_number(last_candle['low'])}
- Close: {format_number(last_candle['close'])}
- Volume: {format_number(last_candle['volume'])}

Berikan analisis LENGKAP dengan format berikut (gunakan format Markdown dengan ** untuk bold):

**Analisis tren pasar**

**SPOT**
🟢 Entry: $X.XX - $X.XX (atau WAIT)
🎯 Target (TP): $X.XX
🔴 Stop Loss (SL): $X.XX
📊 Timeframe Hold: [Short / Medium / Long]
💰 Position Size: Maksimal X% dari portfolio

**Risk Reward Ratio**

**Sinyal Aksi**
[ACCUMULATE / BUY / HOLD / REDUCE / SELL]

**Confidence Level**
X%
"""

    # ================= GROQ API CALL =================

    try:
        completion = client.chat.completions.create(
            model="llama-3.3-70b-versatile",
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            temperature=0.7,
            max_completion_tokens=3000,
            top_p=0.9,
            stream=False,
        )

        if not completion.choices:
            raise Exception("Empty response from Groq API")

        return completion.choices[0].message.content

    except Exception as e:
        logger.error(f"Groq API error: {e}")
        return (
            "❌ Error dari Groq API.\n\n"
            "Market sedang tidak bisa dianalisis saat ini. "
            "Silakan coba beberapa saat lagi."
        )


# ============================================================
# ========================= STREAMING ========================
# ============================================================

def analyze_with_gpt_stream(symbol: str, timeframe: str, last_candle: dict, mode: str = "spot"):
    """
    Versi streaming (optional)
    """

    if mode == "futures":
        system_prompt = "Kamu adalah analis crypto profesional expert FUTURES."
        user_prompt = "[Prompt futures yang sama seperti di atas]"
    else:
        system_prompt = "Kamu adalah analis crypto profesional expert SPOT."
        user_prompt = "[Prompt spot yang sama seperti di atas]"

    try:
        completion = client.chat.completions.create(
            model="llama-3.3-70b-versatile",
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            temperature=0.7,
            max_completion_tokens=3000,
            top_p=0.9,
            stream=True,
        )

        for chunk in completion:
            if chunk.choices and chunk.choices[0].delta.content:
                yield chunk.choices[0].delta.content

    except Exception as e:
        yield f"❌ Error: {str(e)}"
