# config.py - FIXED (NO LOGIC CHANGE)

import os
import logging
from dotenv import load_dotenv

load_dotenv()

logger = logging.getLogger(__name__)

# ============================================================
# ========================= TELEGRAM =========================
# ============================================================

TELEGRAM_TOKEN = os.getenv("TELEGRAM_TOKEN")
if not TELEGRAM_TOKEN:
    logger.warning("TELEGRAM_TOKEN is not set")

# ============================================================
# ========================= GROQ AI ==========================
# ============================================================

GROQ_API_KEY = os.getenv("GROQ_API_KEY")
if not GROQ_API_KEY:
    logger.warning("GROQ_API_KEY is not set")

# ============================================================
# ========================= BINANCE ==========================
# ============================================================

BINANCE_API_KEY = os.getenv("BINANCE_API_KEY")
BINANCE_API_SECRET = os.getenv("BINANCE_API_SECRET")

if not BINANCE_API_KEY or not BINANCE_API_SECRET:
    logger.warning("BINANCE_API_KEY or BINANCE_API_SECRET is not set")

# ============================================================
# ========================= ACCESS ===========================
# ============================================================

_raw_ids = os.getenv("ALLOWED_USER_IDS", "")
ALLOWED_USER_IDS = set()

if _raw_ids.strip():
    try:
        ALLOWED_USER_IDS = {
            int(uid.strip())
            for uid in _raw_ids.split(",")
            if uid.strip().isdigit()
        }
    except Exception:
        logger.warning("Failed parsing ALLOWED_USER_IDS")

# ============================================================
# ========================= TOKENS ===========================
# ============================================================

TOP_TOKENS = [
    "BTCUSDT", "ETHUSDT", "BNBUSDT", "SOLUSDT", "XRPUSDT", "DOGEUSDT",
    "ADAUSDT", "AVAXUSDT", "TRXUSDT", "SHIBUSDT",
    "LINKUSDT", "MATICUSDT", "DOTUSDT", "OPUSDT", "ARBUSDT",
    "WIFUSDT", "PEPEUSDT", "FLOKIUSDT", "TIAUSDT", "INJUSDT",
    "SEIUSDT", "NEARUSDT", "SUIUSDT", "RUNEUSDT", "FTMUSDT",
    "UNIUSDT", "LTCUSDT", "ETCUSDT", "RNDRUSDT", "1000SATSUSDT",
    "DYDXUSDT", "PYTHUSDT", "LDOUSDT", "GMXUSDT", "BCHUSDT",
    "ATOMUSDT", "ARKMUSDT", "JTOUSDT", "JUPUSDT", "ORDIUSDT",
    "BLURUSDT", "STXUSDT", "CYBERUSDT", "GRTUSDT", "ENSUSDT",
    "CAKEUSDT", "MASKUSDT", "NTRNUSDT", "ZETAUSDT", "YGGUSDT",
    "WLDUSDT", "TUSDT", "SKLUSDT", "AGIXUSDT", "KASUSDT",
    "NKNUSDT", "MAVUSDT", "DEXEUSDT", "LINAUSDT", "SSVUSDT",
    "HOOKUSDT", "CTSIUSDT", "CELRUSDT", "CFXUSDT", "BANDUSDT",
    "STMXUSDT", "REEFUSDT", "MTLUSDT", "DODOUSDT", "BELUSDT",
    "VETUSDT", "ACHUSDT", "SANDUSDT", "ALICEUSDT", "PHBUSDT",
    "TOMOUSDT", "PROMUSDT", "XVSUSDT", "ZILUSDT", "CVCUSDT",
    "TRBUSDT", "ICXUSDT", "PERPUSDT", "BICOUSDT", "KAVAUSDT",
    "ILVUSDT", "ARDRUSDT", "SFPUSDT", "CKBUSDT", "STRAXUSDT",
    "DENTUSDT", "HIGHUSDT", "BADGERUSDT", "RIFUSDT", "MOVRUSDT",
    "OCEANUSDT", "OMGUSDT", "ANTUSDT", "SUPERUSDT", "BAKEUSDT",
]

# ============================================================
# ========================= GROQ MODEL =======================
# ============================================================

GROQ_MODEL = "llama-3.3-70b-versatile"
GROQ_TEMPERATURE = 0.7
GROQ_MAX_TOKENS = 3000
