import requests
from config import HYPERBOLIC_API_KEY
from utils import format_number

def analyze_with_gpt(symbol: str, timeframe: str, last_candle: dict) -> str:
    prompt = f"""
Simbol: {symbol}
Timeframe: {timeframe}
Harga terakhir:
- Open: {format_number(last_candle['open'])}
- High: {format_number(last_candle['high'])}
- Low: {format_number(last_candle['low'])}
- Close: {format_number(last_candle['close'])}
- Volume: {format_number(last_candle['volume'])}

🎯 Kamu adalah analis crypto profesional 1000% akurat. seperti seorang analisis komprehensif Teknikal, Fundamental, On-Chain Willy Woo dan trader aktif sejago CryptoJelleNL Berikan:

1. Analisis tren pasar (struktur harga, volume, RSI, MACD, EMA, OBV)
2. FUTURES:
🟢 Entry : (LONG / SHORT / WAIT)
🟢 Entry Range
🟢 Leverage
🟢 TP1, TP2, TP3
🔴 Stop Loss
⚖️ Risk/Reward

3. SPOT:
🟢 Entry
🟢 TP
🔴 SL
📘 Catatan

4. Risk Reward Ratio
5. Catatan (support/resistance, validasi indikator, anomali volume, fakeout)
6. Sinyal Aksi
7. Confidence Level (jika di bawah 85% setup future & spot tidak usah di kirim, ganti saja dengan notif bahwa pasar lagi sedang jelek atau gimana bebas)

Tampilkan hasil dengan bahasa Indonesia profesional dan akurat.
"""
    response = requests.post(
        "https://api.hyperbolic.xyz/v1/chat/completions",
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Bearer {HYPERBOLIC_API_KEY}"
        },
        json={
            "model": "meta-llama/Meta-Llama-3-70B-Instruct",
            "messages": [
                {"role": "system", "content": "Kamu adalah analis crypto profesional 1000% akurat..."},
                {"role": "user", "content": prompt}
            ],
            "max_tokens": 2000
        }
    ).json()

    return response["choices"][0]["message"]["content"]
