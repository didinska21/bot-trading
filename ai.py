# ai.py - GROQ API VERSION
from groq import Groq
from config import GROQ_API_KEY
from utils import format_number

# Initialize Groq client
client = Groq(api_key=GROQ_API_KEY)

def analyze_with_gpt(symbol: str, timeframe: str, last_candle: dict, mode: str = "spot") -> str:
    """
    Analisis crypto dengan AI Groq berdasarkan mode (spot/futures)
    
    Args:
        symbol: Symbol crypto (contoh: BTCUSDT)
        timeframe: Timeframe analisis (15m, 1h, 1d, dll)
        last_candle: Data candle terakhir
        mode: Mode trading ('spot' atau 'futures')
    """
    
    # System prompt berbeda untuk spot dan futures
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
    
    # Prompt berbeda untuk spot dan futures
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
- Ganti dengan penjelasan kenapa market sedang tidak ideal (consolidation, low volume, unclear trend, mixed signals, dll)
- Sarankan untuk WAIT dan monitor dulu

Gunakan bahasa Indonesia profesional, data-driven, dan berikan reasoning yang jelas untuk setiap rekomendasi.
"""
    else:  # mode == "spot"
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
(Analisis struktur harga, volume, momentum, RSI, MACD, EMA crossover, OBV, support/resistance kunci)

**SPOT**
🟢 Entry: $X.XX - $X.XX (atau WAIT jika belum ideal)
🎯 Target (TP): $X.XX 
🔴 Stop Loss (SL): $X.XX (maksimal -3% hingga -7% untuk spot)
📊 Timeframe Hold: [Short-term / Medium-term / Long-term]
💰 Position Size: Maksimal X% dari portfolio
📘 Catatan: 
- Strategi DCA (Dollar Cost Averaging) jika applicable
- Area akumulasi yang ideal
- Holding strategy

**Risk Reward Ratio**
Berikan perhitungan R/R ratio untuk spot trading dan estimasi profit target dalam %.

**Catatan**
- Support/resistance levels penting untuk entry
- Validasi trend dengan multiple timeframe
- Volume analysis dan money flow
- Market structure (higher high, higher low, etc)
- Fundamental catalyst jika ada (news, updates, adoption)

**Sinyal Aksi**
[ACCUMULATE / BUY / HOLD / REDUCE / SELL]

**Confidence Level**
X% (berikan alasan kenapa confidence level ini)

⚠️ PENTING: 
- Jika Confidence Level < 75%, JANGAN berikan setup spot yang agresif.
- Sarankan untuk DCA atau WAIT untuk entry yang lebih baik
- Jelaskan kondisi market saat ini

Gunakan bahasa Indonesia profesional, fokus pada buy & hold strategy yang aman untuk spot trading.
"""

    try:
        # Call Groq API dengan streaming
        completion = client.chat.completions.create(
            model="llama-3.3-70b-versatile",
            messages=[
                {
                    "role": "system",
                    "content": system_prompt
                },
                {
                    "role": "user",
                    "content": user_prompt
                }
            ],
            temperature=0.7,
            max_completion_tokens=3000,
            top_p=0.9,
            stream=False,  # Disable streaming untuk response langsung
            stop=None
        )
        
        # Get the complete response
        result = completion.choices[0].message.content
        return result
        
    except Exception as e:
        return f"❌ Error dari Groq API: {str(e)}\n\nSilakan coba lagi atau hubungi admin."


def analyze_with_gpt_stream(symbol: str, timeframe: str, last_candle: dict, mode: str = "spot"):
    """
    Versi streaming untuk real-time response (optional)
    Generator function yang yield response per chunk
    """
    
    # System prompt
    if mode == "futures":
        system_prompt = """Kamu adalah analis crypto profesional expert dalam FUTURES TRADING dengan akurasi tinggi.
Kamu ahli dalam analisis teknikal mendalam seperti Willy Woo (on-chain) dan CryptoJelleNL (trading aktif).
Fokus pada setup FUTURES dengan leverage, multiple TP, dan risk management ketat."""
    else:
        system_prompt = """Kamu adalah analis crypto profesional expert dalam SPOT TRADING dengan akurasi tinggi.
Kamu ahli dalam analisis teknikal untuk buy & hold strategy, swing trading spot, dan investment jangka menengah-panjang.
Fokus pada entry yang aman dengan risk/reward yang baik untuk spot trading."""
    
    # User prompt (sama seperti di atas)
    if mode == "futures":
        user_prompt = f"""[Prompt futures yang sama seperti di atas]"""
    else:
        user_prompt = f"""[Prompt spot yang sama seperti di atas]"""
    
    try:
        # Call Groq API dengan streaming enabled
        completion = client.chat.completions.create(
            model="llama-3.3-70b-versatile",
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ],
            temperature=0.7,
            max_completion_tokens=3000,
            top_p=0.9,
            stream=True,
            stop=None
        )
        
        # Yield chunks untuk streaming
        for chunk in completion:
            if chunk.choices[0].delta.content:
                yield chunk.choices[0].delta.content
                
    except Exception as e:
        yield f"❌ Error: {str(e)}"
