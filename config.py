# config.py - IMPROVED VERSION with Better Validation

import os
import logging
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Setup logging
logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)
logger = logging.getLogger(__name__)

# ============================================================
# ========================= TELEGRAM =========================
# ============================================================

TELEGRAM_TOKEN = os.getenv("TELEGRAM_TOKEN")
if not TELEGRAM_TOKEN:
    logger.error("❌ TELEGRAM_TOKEN is not set! Bot cannot start.")
else:
    logger.info("✅ TELEGRAM_TOKEN loaded")

# ============================================================
# ========================= GROQ AI ==========================
# ============================================================

GROQ_API_KEY = os.getenv("GROQ_API_KEY")
if not GROQ_API_KEY:
    logger.error("❌ GROQ_API_KEY is not set! AI analysis will fail.")
else:
    logger.info("✅ GROQ_API_KEY loaded")

# ============================================================
# ========================= BINANCE ==========================
# ============================================================

BINANCE_API_KEY = os.getenv("BINANCE_API_KEY")
BINANCE_API_SECRET = os.getenv("BINANCE_API_SECRET")

if not BINANCE_API_KEY or not BINANCE_API_SECRET:
    logger.error("❌ BINANCE credentials not set! Trading will not work.")
else:
    logger.info("✅ BINANCE credentials loaded")

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
        logger.info(f"✅ Allowed users: {len(ALLOWED_USER_IDS)} user(s)")
    except Exception as e:
        logger.error(f"❌ Failed parsing ALLOWED_USER_IDS: {e}")
else:
    logger.warning("⚠️ No ALLOWED_USER_IDS set - bot will reject all users!")

# ============================================================
# ========================= DEBUG MODE =======================
# ============================================================

DEBUG = os.getenv("DEBUG", "False").lower() == "true"
if DEBUG:
    logger.setLevel(logging.DEBUG)
    logger.warning("🐛 DEBUG MODE ENABLED")

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

logger.info(f"✅ Loaded {len(TOP_TOKENS)} trading pairs")

# ============================================================
# ========================= GROQ MODEL =======================
# ============================================================

GROQ_MODEL = "llama-3.3-70b-versatile"
GROQ_TEMPERATURE = 0.7
GROQ_MAX_TOKENS = 3000

logger.info(f"✅ Groq model: {GROQ_MODEL}")

# ============================================================
# ========================= TRADING CONFIG ===================
# ============================================================

# Futures Trading Configuration
FUTURES_CONFIG = {
    "default_leverage": 10,
    "max_leverage": 20,
    "position_size_pct": 15,  # % of available balance
    "max_loss_pct": 17.5,  # Max loss before stopping
    "min_confidence": 85,  # Minimum AI confidence level
}

# Risk Management
RISK_CONFIG = {
    "max_daily_trades": 10,
    "max_concurrent_positions": 3,
    "stop_loss_buffer": 1.02,  # 2% buffer for SL
    "take_profit_buffer": 0.98,  # 2% buffer for TP
}

logger.info("✅ Trading configuration loaded")

# ============================================================
# ========================= VALIDATION =======================
# ============================================================

def validate_config():
    """Validate all required configurations"""
    errors = []
    
    if not TELEGRAM_TOKEN:
        errors.append("TELEGRAM_TOKEN is missing")
    
    if not GROQ_API_KEY:
        errors.append("GROQ_API_KEY is missing")
    
    if not BINANCE_API_KEY or not BINANCE_API_SECRET:
        errors.append("BINANCE credentials are missing")
    
    if not ALLOWED_USER_IDS:
        errors.append("ALLOWED_USER_IDS is empty - no one can use the bot")
    
    if errors:
        logger.error("❌ Configuration validation failed:")
        for error in errors:
            logger.error(f"   - {error}")
        return False
    
    logger.info("✅ All configurations validated successfully")
    return True

# Run validation on import
if __name__ == "__main__":
    print("\n" + "="*60)
    print("CONFIG VALIDATION")
    print("="*60)
    validate_config()
    print("="*60 + "\n")
