# ai.py - IMPROVED VERSION with Better Error Handling & Retry Logic

from groq import Groq
from config import GROQ_API_KEY, GROQ_MODEL, GROQ_TEMPERATURE, GROQ_MAX_TOKENS
from utils import format_number
import logging
import time

logger = logging.getLogger(__name__)

# Initialize Groq client with error handling
try:
    client = Groq(api_key=GROQ_API_KEY)
    logger.info("✅ Groq client initialized")
except Exception as e:
    logger.error(f"❌ Failed to initialize Groq client: {e}")
    client = None


def analyze_with_gpt(symbol: str, timeframe: str, last_candle: dict, mode: str = "spot", retry_count: int = 3) -> str:
    """
    Analisis crypto dengan AI Groq berdasarkan mode (spot/futures)
    
    Args:
        symbol: Trading pair (e.g., BTCUSDT)
        timeframe: Timeframe (15m, 1h, 4h, 1d, 1w)
        last_candle: Dictionary with OHLCV data
        mode: "spot" or "futures"
        retry_count: Number of retries on failure
    
    Returns:
        Analysis result string
    """
    
    if not client:
        return "❌ Groq client tidak tersedia. Silakan periksa GROQ_API_KEY."

    # ================= SYSTEM PROMPT =================

    if mode == "futures":
        system_prompt = """Kamu adalah analis crypto profesional expert dalam FUTURES TRADING dengan akurasi tinggi.
Kamu ahli dalam analisis teknikal mendalam seperti Willy Woo (on-chain) dan CryptoJelleNL (trading aktif).
Fokus pada setup FUTURES dengan leverage, multiple TP, dan risk management ketat.

PENTING:
- Berikan analisis yang akurat dan data-driven
- Gunakan format yang KONSISTEN dan mudah di-parse
- Jika confidence level < 85%, JANGAN berikan setup trading, sarankan WAIT
- Pertimbangkan volume, momentum, dan market structure
- Berikan reasoning yang jelas untuk setiap keputusan"""
    else:
        system_prompt = """Kamu adalah analis crypto profesional expert dalam SPOT TRADING dengan akurasi tinggi.
Kamu ahli dalam analisis teknikal untuk buy & hold strategy, swing trading spot, dan investment jangka menengah-panjang.
Fokus pada entry yang aman dengan risk/reward yang baik untuk spot trading.

PENTING:
- Berikan analisis yang akurat dan data-driven
- Gunakan format yang KONSISTEN dan mudah di-parse
- Pertimbangkan timeframe hold dan volatilitas
- Fokus pada support/resistance yang kuat
- Berikan reasoning yang jelas untuk setiap rekomendasi"""

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

⚠️ KRITERIA PENTING:
- Jika Confidence Level < 85%, JANGAN berikan setup futures
- Ganti dengan penjelasan kenapa market sedang tidak ideal
- Hanya rekomendasikan LONG/SHORT jika setup benar-benar kuat
- Gunakan angka yang REALISTIS dan bisa di-parse (gunakan format: $12345.67)
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
(Analisis tren, support/resistance, volume, indikator teknikal)

**SPOT**
🟢 Entry: $X.XX - $X.XX (atau WAIT)
🎯 Target (TP): $X.XX
🔴 Stop Loss (SL): $X.XX
📊 Timeframe Hold: [Short / Medium / Long]
💰 Position Size: Maksimal X% dari portfolio

**Risk Reward Ratio**
Perhitungan R/R ratio dan estimasi profit/loss dalam %

**Sinyal Aksi**
[ACCUMULATE / BUY / HOLD / REDUCE / SELL]

**Confidence Level**
X% (berikan reasoning)

⚠️ KRITERIA PENTING:
- Gunakan angka yang REALISTIS dan bisa di-parse (format: $12345.67)
- Pertimbangkan timeframe hold yang sesuai
- Berikan reasoning yang jelas
"""

    # ================= GROQ API CALL WITH RETRY =================

    for attempt in range(retry_count):
        try:
            logger.info(f"Calling Groq API for {symbol} (attempt {attempt + 1}/{retry_count})")
            
            start_time = time.time()
            
            completion = client.chat.completions.create(
                model=GROQ_MODEL,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt},
                ],
                temperature=GROQ_TEMPERATURE,
                max_completion_tokens=GROQ_MAX_TOKENS,
                top_p=0.9,
                stream=False,
            )

            elapsed_time = time.time() - start_time
            logger.info(f"✅ Groq API response received in {elapsed_time:.2f}s")

            if not completion.choices:
                raise Exception("Empty response from Groq API")

            result = completion.choices[0].message.content
            
            # Validate result
            if not result or len(result.strip()) < 50:
                raise Exception("Response too short or empty")
            
            # Check if AI is suggesting WAIT due to low confidence
            result_upper = result.upper()
            if "CONFIDENCE LEVEL" in result_upper:
                # Try to extract confidence level
                import re
                confidence_match = re.search(r'(\d+)%', result)
                if confidence_match:
                    confidence = int(confidence_match.group(1))
                    logger.info(f"AI Confidence Level: {confidence}%")
                    
                    if confidence < 70:
                        logger.warning(f"Low confidence level: {confidence}%")

            logger.info(f"Analysis completed for {symbol}")
            return result

        except Exception as e:
            logger.error(f"Groq API error (attempt {attempt + 1}/{retry_count}): {e}")
            
            if attempt < retry_count - 1:
                # Wait before retry (exponential backoff)
                wait_time = 2 ** attempt
                logger.info(f"Retrying in {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                # Final attempt failed
                logger.error(f"All retry attempts failed for {symbol}")
                return _generate_error_response(symbol, mode, str(e))

    return _generate_error_response(symbol, mode, "Max retries exceeded")


def _generate_error_response(symbol: str, mode: str, error: str) -> str:
    """Generate user-friendly error response"""
    return f"""
❌ <b>Gagal Mendapatkan Analisis</b>

Symbol: {symbol}
Mode: {mode.upper()}

<b>Error:</b> {error}

<b>Kemungkinan penyebab:</b>
• Groq API sedang mengalami masalah
• Koneksi internet tidak stabil
• Rate limit exceeded
• API key tidak valid

<b>Solusi:</b>
• Tunggu beberapa saat dan coba lagi
• Periksa koneksi internet
• Jika masalah berlanjut, hubungi admin

💡 <i>Tip: Coba analisis dengan timeframe atau token lain terlebih dahulu.</i>
"""


# ============================================================
# ========================= STREAMING ========================
# ============================================================

def analyze_with_gpt_stream(symbol: str, timeframe: str, last_candle: dict, mode: str = "spot"):
    """
    Versi streaming untuk real-time analysis
    Generator function yang yield chunks of text
    """
    
    if not client:
        yield "❌ Groq client tidak tersedia."
        return

    # Use same prompts as non-streaming version
    if mode == "futures":
        system_prompt = """Kamu adalah analis crypto profesional expert dalam FUTURES TRADING dengan akurasi tinggi.
Berikan analisis yang akurat, data-driven, dan profesional dengan format yang konsisten."""
        
        user_prompt = f"""
Analisis FUTURES untuk {symbol} pada timeframe {timeframe}.

Data: Open={format_number(last_candle['open'])}, High={format_number(last_candle['high'])}, 
Low={format_number(last_candle['low'])}, Close={format_number(last_candle['close'])}, 
Volume={format_number(last_candle['volume'])}

Berikan analisis lengkap dengan format yang sama seperti biasa.
"""
    else:
        system_prompt = """Kamu adalah analis crypto profesional expert dalam SPOT TRADING.
Berikan analisis yang akurat dengan format yang konsisten."""
        
        user_prompt = f"""
Analisis SPOT untuk {symbol} pada timeframe {timeframe}.

Data: Open={format_number(last_candle['open'])}, High={format_number(last_candle['high'])}, 
Low={format_number(last_candle['low'])}, Close={format_number(last_candle['close'])}, 
Volume={format_number(last_candle['volume'])}

Berikan analisis lengkap dengan format yang sama seperti biasa.
"""

    try:
        logger.info(f"Starting streaming analysis for {symbol}")
        
        completion = client.chat.completions.create(
            model=GROQ_MODEL,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            temperature=GROQ_TEMPERATURE,
            max_completion_tokens=GROQ_MAX_TOKENS,
            top_p=0.9,
            stream=True,
        )

        for chunk in completion:
            if chunk.choices and chunk.choices[0].delta.content:
                yield chunk.choices[0].delta.content

        logger.info(f"Streaming completed for {symbol}")

    except Exception as e:
        logger.error(f"Streaming error: {e}")
        yield f"\n\n❌ Error: {str(e)}"


# ============================================================
# ========================= VALIDATION =======================
# ============================================================

def validate_analysis_result(result: str, mode: str = "spot") -> tuple[bool, str]:
    """
    Validate AI analysis result
    
    Returns:
        (is_valid, error_message)
    """
    if not result or len(result.strip()) < 50:
        return False, "Response too short"
    
    result_upper = result.upper()
    
    # Check for required sections
    required_sections = ["ANALISIS", "CONFIDENCE LEVEL"]
    
    if mode == "futures":
        required_sections.extend(["FUTURES", "ENTRY", "STOP LOSS"])
    else:
        required_sections.extend(["SPOT", "ENTRY"])
    
    missing_sections = [sec for sec in required_sections if sec not in result_upper]
    
    if missing_sections:
        return False, f"Missing sections: {', '.join(missing_sections)}"
    
    return True, "Valid"


# ============================================================
# ========================= TESTING ==========================
# ============================================================

if __name__ == "__main__":
    """Test AI analysis"""
    
    print("="*60)
    print("TESTING AI.PY")
    print("="*60)
    
    # Sample OHLCV data
    sample_candle = {
        "open": 50000.0,
        "high": 51000.0,
        "low": 49500.0,
        "close": 50800.0,
        "volume": 1234567.89
    }
    
    print("\n1. Testing SPOT Analysis:")
    print("-" * 60)
    result = analyze_with_gpt("BTCUSDT", "1h", sample_candle, "spot")
    print(result[:200] + "..." if len(result) > 200 else result)
    
    print("\n2. Testing FUTURES Analysis:")
    print("-" * 60)
    result = analyze_with_gpt("ETHUSDT", "4h", sample_candle, "futures")
    print(result[:200] + "..." if len(result) > 200 else result)
    
    print("\n3. Testing Validation:")
    print("-" * 60)
    valid, msg = validate_analysis_result(result, "futures")
    print(f"Valid: {valid}, Message: {msg}")
    
    print("\n" + "="*60)
    print("Tests completed!")
    print("="*60)
