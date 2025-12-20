# main.py - PART 1/3: IMPORTS & CONVERSATION STATES (UPDATE)

import logging
import asyncio
from telegram import Update, InlineKeyboardButton, InlineKeyboardMarkup
from telegram.ext import (
    ApplicationBuilder, CommandHandler, CallbackQueryHandler,
    MessageHandler, ConversationHandler, ContextTypes, filters
)
from binance.client import Client
from binance.exceptions import BinanceAPIException

from config import TELEGRAM_TOKEN, BINANCE_API_KEY, BINANCE_API_SECRET, TOP_TOKENS
from utils import only_allowed, format_result_for_telegram
from ai import analyze_with_gpt
from shared_state import SharedState

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

binance_client = Client(
    api_key=BINANCE_API_KEY,
    api_secret=BINANCE_API_SECRET
)

shared_state = SharedState()

# ⭐ UPDATE: Conversation states - TAMBAH 2 STATE BARU
SELECTING_MODE, SELECTING_TOKEN, SELECTING_STRATEGY, INPUTTING_SIZE, INPUTTING_LEVERAGE = range(5)

# Binance timeframe mapping
TIMEFRAME_MAP = {
    "15m": Client.KLINE_INTERVAL_15MINUTE,
    "1h": Client.KLINE_INTERVAL_1HOUR,
    "4h": Client.KLINE_INTERVAL_4HOUR,
    "1d": Client.KLINE_INTERVAL_1DAY,
    "1w": Client.KLINE_INTERVAL_1WEEK,
}

# =====================================================================
# ======================= HELPER FUNCTIONS ============================
# =====================================================================

def validate_symbol(symbol: str) -> bool:
    """Validate if symbol exists on Binance"""
    try:
        binance_client.get_symbol_ticker(symbol=symbol)
        return True
    except BinanceAPIException:
        return False


async def safe_edit_message(query, text, parse_mode="HTML", reply_markup=None):
    """Safely edit message with error handling"""
    try:
        await query.edit_message_text(
            text,
            parse_mode=parse_mode,
            reply_markup=reply_markup
        )
    except Exception as e:
        logging.error(f"Failed to edit message: {e}")
        try:
            await query.message.reply_text(text, parse_mode=parse_mode, reply_markup=reply_markup)
        except:
            pass


def parse_price_from_ai(ai_result: str, price_type: str, side: str, current_price: float) -> float:
    """Parse price from AI response - SAMA SEPERTI SEBELUMNYA"""
    import re
    
    try:
        if price_type == 'entry':
            entry_match = re.search(
                r'Entry\s*(?:Range|Price)?.*?\$?\s*([\d.,]+)\s*(?:-|to|~)?\s*\$?\s*([\d.,]+)?',
                ai_result,
                re.IGNORECASE
            )
            if entry_match:
                entry_low = float(entry_match.group(1).replace(',', ''))
                entry_high = float(entry_match.group(2).replace(',', '')) if entry_match.group(2) else entry_low
                return (entry_low + entry_high) / 2
            else:
                return current_price * (0.998 if side == 'BUY' else 1.002)
        
        elif price_type == 'tp':
            tp_matches = re.findall(r'TP\s*\d*.*?\$?\s*([\d.,]+)', ai_result, re.IGNORECASE)
            
            if tp_matches and len(tp_matches) >= 1:
                tp_values = [float(tp.replace(',', '')) for tp in tp_matches[:3]]
                
                if side.upper() in ['BUY', 'LONG']:
                    selected_tp = max(tp_values)
                else:
                    selected_tp = min(tp_values)
                
                return selected_tp
            else:
                if side.upper() in ['BUY', 'LONG']:
                    return current_price * 1.03
                else:
                    return current_price * 0.97
        
        elif price_type == 'sl':
            sl_match = re.search(r'(?:Stop\s*Loss|SL).*?\$?\s*([\d.,]+)', ai_result, re.IGNORECASE)
            if sl_match:
                return float(sl_match.group(1).replace(',', ''))
            else:
                if side.upper() in ['BUY', 'LONG']:
                    return current_price * 0.98
                else:
                    return current_price * 1.02
        
        return current_price
        
    except Exception as e:
        logging.error(f"Error parsing {price_type}: {e}")
        return current_price


# =====================================================================
# NOTE: PART 2 akan berisi input handlers (size & leverage)
# PART 3 akan berisi execute confirm dan conversation handler
# =====================================================================
# main.py - PART 2/3: MANUAL INPUT HANDLERS (SIZE & LEVERAGE)

# =====================================================================
# =================== HANDLE EXECUTE FUTURES (UPDATED) ================
# =====================================================================

@only_allowed
async def handle_execute_futures(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Show balance and ask for position size - UPDATED VERSION"""
    query = update.callback_query
    await query.answer()

    analysis = context.user_data.get("analysis")
    if not analysis:
        await query.answer("❌ Data analisis tidak ditemukan!", show_alert=True)
        return await start(update, context)

    try:
        from binance_futures import BinanceFuturesTrader
        futures_trader = BinanceFuturesTrader()
        
        # Get balance
        balance = futures_trader.get_futures_balance()
        if not balance:
            raise Exception("Gagal mendapatkan balance")
        
        available = balance.get("availableBalance", 0)
        
        # Get symbol info for minimum
        symbol_info = futures_trader.get_symbol_info(analysis["symbol"])
        min_notional = symbol_info.get("min_notional", 5.0) if symbol_info else 5.0
        max_leverage = symbol_info.get("max_leverage", 125) if symbol_info else 125
        
        # Save to context
        context.user_data["available_balance"] = available
        context.user_data["min_notional"] = min_notional
        context.user_data["max_leverage"] = max_leverage

        await safe_edit_message(
            query,
            f"""
💰 <b>SETUP FUTURES ORDER - STEP 1/2</b>

📊 <b>Balance Info:</b>
Available: <b>${available:.2f}</b>
Symbol: {analysis['symbol']}

⚙️ <b>Requirements:</b>
Minimum: ${min_notional:.2f}
Maximum: ${available:.2f}

━━━━━━━━━━━━━━━━━━━━

💵 <b>Berapa position size yang ingin digunakan?</b>

💡 <b>Contoh:</b>
• Ketik: <code>1</code> untuk $1
• Ketik: <code>2.5</code> untuk $2.50
• Ketik: <code>5</code> untuk $5
• Ketik: <code>{available:.2f}</code> untuk ALL-IN

⚠️ Position size harus: ${min_notional:.2f} - ${available:.2f}

<i>Ketik angka position size di chat...</i>
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("❌ Batal", callback_data="execute_cancel")]
            ])
        )

    except Exception as e:
        logging.error(f"Error getting balance: {e}")
        await safe_edit_message(
            query,
            f"❌ <b>Error:</b> {str(e)}\n\nSilakan coba lagi.",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔙 Kembali", callback_data="back_to_start")]
            ])
        )
        return SELECTING_MODE

    return INPUTTING_SIZE


# =====================================================================
# ======================= HANDLE SIZE INPUT ===========================
# =====================================================================

@only_allowed
async def handle_size_input(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle position size input"""
    try:
        size_text = update.message.text.strip()
        position_size = float(size_text)
        
        available = context.user_data.get("available_balance", 0)
        min_notional = context.user_data.get("min_notional", 5.0)
        analysis = context.user_data.get("analysis")
        
        # Validate minimum
        if position_size < min_notional:
            await update.message.reply_text(
                f"❌ <b>Position size terlalu kecil!</b>\n\n"
                f"Minimum: <b>${min_notional:.2f}</b>\n"
                f"Anda input: ${position_size:.2f}\n\n"
                f"Silakan input lagi dengan nilai ≥ ${min_notional:.2f}",
                parse_mode="HTML"
            )
            return INPUTTING_SIZE
        
        # Validate maximum
        if position_size > available:
            await update.message.reply_text(
                f"❌ <b>Position size melebihi balance!</b>\n\n"
                f"Available: <b>${available:.2f}</b>\n"
                f"Anda input: ${position_size:.2f}\n\n"
                f"Silakan input lagi dengan nilai ≤ ${available:.2f}",
                parse_mode="HTML"
            )
            return INPUTTING_SIZE
        
        # Save position size
        context.user_data["position_size"] = position_size
        
        # Calculate percentage
        pct = (position_size / available) * 100
        
        # Get max leverage
        max_leverage = context.user_data.get("max_leverage", 125)
        
        # Show warning if high percentage
        warning = ""
        if pct > 50:
            warning = f"\n⚠️ <b>HIGH RISK:</b> Menggunakan {pct:.1f}% dari balance!"
        
        # Ask for leverage
        await update.message.reply_text(
            f"""
✅ <b>Position size: ${position_size:.2f}</b> ({pct:.1f}% dari balance){warning}

━━━━━━━━━━━━━━━━━━━━

⚡ <b>STEP 2/2 - Pilih Leverage</b>

📊 <b>Info Leverage untuk {analysis['symbol']}:</b>
Minimum: 1x
Maximum: {max_leverage}x

💡 <b>Contoh:</b>
• Ketik: <code>10</code> untuk 10x
• Ketik: <code>20</code> untuk 20x
• Ketik: <code>50</code> untuk 50x

⚠️ <b>Perhatian Leverage:</b>

<b>Low Risk (1x-10x):</b>
• Liquidation jauh dari entry
• Cocok untuk pemula
• Profit/loss lebih kecil

<b>Medium Risk (10x-25x):</b>
• Balanced risk/reward
• Liquidation ~4-10% dari entry
• Untuk trader berpengalaman

<b>High Risk (25x-50x):</b>
• Liquidation ~2-4% dari entry
• Profit/loss sangat besar
• Hanya untuk expert

<b>Very High Risk (50x+):</b>
• Liquidation < 2% dari entry
• Bisa liquidasi dalam hitungan menit
• Extreme risk!

<i>Ketik angka leverage yang diinginkan...</i>
""",
            parse_mode="HTML"
        )
        
        return INPUTTING_LEVERAGE
        
    except ValueError:
        await update.message.reply_text(
            "❌ <b>Format salah!</b>\n\n"
            "Silakan input angka saja.\n\n"
            "Contoh: <code>1</code> atau <code>2.5</code> atau <code>5</code>",
            parse_mode="HTML"
        )
        return INPUTTING_SIZE


# =====================================================================
# ===================== HANDLE LEVERAGE INPUT =========================
# =====================================================================

@only_allowed
async def handle_leverage_input(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Handle leverage input and show confirmation"""
    try:
        leverage_text = update.message.text.strip()
        leverage = int(float(leverage_text))
        
        max_leverage = context.user_data.get("max_leverage", 125)
        analysis = context.user_data.get("analysis")
        position_size = context.user_data.get("position_size", 0)
        available = context.user_data.get("available_balance", 0)
        
        # Validate minimum
        if leverage < 1:
            await update.message.reply_text(
                "❌ <b>Leverage terlalu kecil!</b>\n\n"
                "Minimum: <b>1x</b>\n\n"
                "Silakan input lagi.",
                parse_mode="HTML"
            )
            return INPUTTING_LEVERAGE
        
        # Validate maximum
        if leverage > max_leverage:
            await update.message.reply_text(
                f"❌ <b>Leverage terlalu besar!</b>\n\n"
                f"Maximum untuk {analysis['symbol']}: <b>{max_leverage}x</b>\n"
                f"Anda input: {leverage}x\n\n"
                f"Silakan input lagi dengan nilai ≤ {max_leverage}x",
                parse_mode="HTML"
            )
            return INPUTTING_LEVERAGE
        
        # Save leverage
        context.user_data["leverage"] = leverage
        
        # Calculate risk level
        if leverage <= 10:
            risk_level = "🟢 LOW RISK"
            risk_emoji = "🟢"
        elif leverage <= 25:
            risk_level = "🟡 MEDIUM RISK"
            risk_emoji = "🟡"
        elif leverage <= 50:
            risk_level = "🟠 HIGH RISK"
            risk_emoji = "🟠"
        else:
            risk_level = "🔴 VERY HIGH RISK"
            risk_emoji = "🔴"
        
        # Calculate liquidation estimate (rough)
        if leverage > 0:
            liq_distance = (100 / leverage)
            liq_text = f"~{liq_distance:.2f}% dari entry price"
        else:
            liq_text = "N/A"
        
        # Show confirmation
        pct = (position_size / available) * 100
        
        await update.message.reply_text(
            f"""
📋 <b>KONFIRMASI FUTURES ORDER</b>

━━━━━━━━━━━━━━━━━━━━
📊 <b>ORDER DETAILS</b>
━━━━━━━━━━━━━━━━━━━━

Symbol: <b>{analysis['symbol']}</b>
Timeframe: {analysis['timeframe']}
Mode: FUTURES

💰 <b>Position:</b>
Size: <b>${position_size:.2f}</b> ({pct:.1f}% balance)
Leverage: <b>{leverage}x</b>
Risk Level: {risk_level}

📍 <b>Liquidation Distance:</b>
{liq_text}

🎯 <b>Trading Setup:</b>
• Entry, TP, SL: Auto by AI
• TP/SL orders: Auto-placed
• Position: Auto-calculated

━━━━━━━━━━━━━━━━━━━━
{risk_emoji} <b>RISK WARNING</b>
━━━━━━━━━━━━━━━━━━━━

Dengan {leverage}x leverage:
• Pergerakan {100/leverage:.2f}% = 100% profit/loss
• Liquidation bisa terjadi cepat
• Monitor position secara aktif!

<b>Apakah Anda yakin ingin melanjutkan?</b>
""",
            parse_mode="HTML",
            reply_markup=InlineKeyboardMarkup([
                [
                    InlineKeyboardButton("✅ YA, EXECUTE!", callback_data="execute_confirm_manual"),
                    InlineKeyboardButton("❌ BATAL", callback_data="execute_cancel")
                ]
            ])
        )
        
        return SELECTING_MODE
        
    except ValueError:
        await update.message.reply_text(
            "❌ <b>Format salah!</b>\n\n"
            "Silakan input angka bulat saja.\n\n"
            "Contoh: <code>10</code> atau <code>20</code> atau <code>50</code>",
            parse_mode="HTML"
        )
        return INPUTTING_LEVERAGE


# =====================================================================
# NOTE: PART 3 akan berisi execute confirm handler dan conversation setup
# =====================================================================
# main.py - PART 3/3: EXECUTE CONFIRM & CONVERSATION HANDLER (FINAL)

# =====================================================================
# =================== EXECUTE CONFIRM (MANUAL INPUT) ==================
# =====================================================================

@only_allowed
async def handle_execute_confirm_manual(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Execute futures order with manual size & leverage"""
    query = update.callback_query
    await query.answer()

    analysis = context.user_data.get("analysis")
    position_size = context.user_data.get("position_size")
    leverage = context.user_data.get("leverage")
    
    if not analysis or not position_size or not leverage:
        await query.answer("❌ Data tidak lengkap! Silakan ulang dari awal.", show_alert=True)
        return await start(update, context)

    await safe_edit_message(
        query,
        "⏳ <b>Memasang posisi...</b>\n\nMohon tunggu...",
    )

    try:
        from binance_futures import BinanceFuturesTrader

        futures_trader = BinanceFuturesTrader()
        ai_result = analysis["ai_result"]
        current_price = analysis["current_price"]

        # Validate AI response
        if not ai_result or len(ai_result.strip()) < 20:
            raise Exception("AI response tidak valid")

        # Detect side (LONG/SHORT)
        ai_upper = ai_result.upper()
        if "🟢 LONG" in ai_result or "**LONG**" in ai_result or ("POSISI: LONG" in ai_upper):
            side = "BUY"
            logging.info("✅ Detected LONG position")
        elif "🔴 SHORT" in ai_result or "**SHORT**" in ai_result or ("POSISI: SHORT" in ai_upper):
            side = "SELL"
            logging.info("✅ Detected SHORT position")
        elif "LONG" in ai_upper and "SHORT" not in ai_upper:
            side = "BUY"
        elif "SHORT" in ai_upper and "LONG" not in ai_upper:
            side = "SELL"
        else:
            if "WAIT" in ai_upper or "HOLD" in ai_upper:
                await safe_edit_message(
                    query,
                    "❌ <b>AI Menyarankan WAIT</b>\n\nTidak ada sinyal trading yang jelas.",
                    reply_markup=InlineKeyboardMarkup([
                        [InlineKeyboardButton("🔄 Analisis Ulang", callback_data=f"strategy_{analysis['timeframe']}")],
                        [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
                    ])
                )
                return SELECTING_MODE
            raise Exception("Tidak dapat mendeteksi LONG atau SHORT")

        # Parse prices
        entry_price = parse_price_from_ai(ai_result, 'entry', side, current_price)
        tp_price = parse_price_from_ai(ai_result, 'tp', side, current_price)
        sl_price = parse_price_from_ai(ai_result, 'sl', side, current_price)

        logging.info(f"📊 Parsed: Entry=${entry_price:.4f}, TP=${tp_price:.4f}, SL=${sl_price:.4f}")

        # Validate price levels
        if side == "BUY":
            if tp_price <= entry_price or sl_price >= entry_price:
                raise Exception(f"Invalid price levels for LONG")
        else:
            if tp_price >= entry_price or sl_price <= entry_price:
                raise Exception(f"Invalid price levels for SHORT")

        logging.info("✅ Price validation passed")

        # Calculate quantity
        quantity = futures_trader.calculate_quantity(
            analysis["symbol"],
            entry_price,
            position_size
        )
        
        if not quantity or quantity <= 0:
            raise Exception(
                f"Gagal menghitung quantity!\n\n"
                f"Position size: ${position_size:.2f}\n"
                f"Entry price: ${entry_price:.4f}\n\n"
                f"Kemungkinan:\n"
                f"• Position size terlalu kecil untuk symbol ini\n"
                f"• Coba naikkan position size atau pilih symbol lain"
            )

        # Place order with user-defined leverage
        logging.info(f"🚀 Placing: {side} {quantity} {analysis['symbol']} @{leverage}x")
        result = futures_trader.place_futures_order(
            symbol=analysis["symbol"],
            side=side,
            quantity=quantity,
            entry_price=entry_price,
            tp_price=tp_price,
            sl_price=sl_price,
            leverage=leverage  # Use manual input leverage
        )

        if not result.get("success"):
            raise Exception(result.get("error", "Unknown error from Binance"))

        # Calculate PnL
        if side == "BUY":
            profit_pct = (tp_price - entry_price) / entry_price * 100
            loss_pct = (entry_price - sl_price) / entry_price * 100
        else:
            profit_pct = (entry_price - tp_price) / entry_price * 100
            loss_pct = (sl_price - entry_price) / entry_price * 100

        rr = profit_pct / loss_pct if loss_pct > 0 else 0
        max_profit = position_size * (profit_pct / 100) * leverage
        max_loss = position_size * (loss_pct / 100) * leverage

        success_msg = f"""
✅ <b>POSISI BERHASIL DIPASANG!</b>

━━━━━━━━━━━━━━━━━━━━
📊 <b>ORDER DETAILS</b>
━━━━━━━━━━━━━━━━━━━━

Symbol: {analysis['symbol']}
Side: <b>{"🟢 LONG" if side == "BUY" else "🔴 SHORT"}</b>
Leverage: <b>{leverage}x</b>

━━━━━━━━━━━━━━━━━━━━
💰 <b>PRICES</b>
━━━━━━━━━━━━━━━━━━━━

Entry: ${entry_price:.4f}
TP: ${tp_price:.4f} (<b>+{profit_pct:.2f}%</b>)
SL: ${sl_price:.4f} (<b>-{loss_pct:.2f}%</b>)

━━━━━━━━━━━━━━━━━━━━
📦 <b>POSITION</b>
━━━━━━━━━━━━━━━━━━━━

Quantity: {quantity}
Position Size: ${position_size:.2f}
Margin Used: ${position_size:.2f}

━━━━━━━━━━━━━━━━━━━━
📈 <b>RISK/REWARD</b>
━━━━━━━━━━━━━━━━━━━━

R/R Ratio: 1:{rr:.2f}
Max Profit: <b>${max_profit:.2f}</b>
Max Loss: <b>-${max_loss:.2f}</b>

━━━━━━━━━━━━━━━━━━━━
🎯 <b>ORDER IDs</b>
━━━━━━━━━━━━━━━━━━━━

Entry: {result['entry_order']['orderId']}
TP: {result['tp_order']['orderId']}
SL: {result['sl_order']['orderId']}

━━━━━━━━━━━━━━━━━━━━
⚠️ <b>REMINDER</b>
━━━━━━━━━━━━━━━━━━━━

• Monitor posisi secara aktif!
• Jangan lupa cek liquidation price
• Siap-siap adjust jika market volatile

<i>Good luck & trade safely! 🚀</i>
"""

        keyboard = [
            [InlineKeyboardButton("📊 Lihat di Binance", url=f"https://www.binance.com/en/futures/{analysis['symbol']}")],
            [InlineKeyboardButton("📈 Trade Lagi", callback_data=f"mode_{analysis['mode']}")],
            [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
        ]

        await safe_edit_message(
            query,
            success_msg,
            reply_markup=InlineKeyboardMarkup(keyboard)
        )

    except Exception as e:
        logging.error(f"Execute error: {e}")
        import traceback
        logging.error(traceback.format_exc())
        
        await safe_edit_message(
            query,
            f"""
❌ <b>GAGAL MEMASANG POSISI!</b>

<b>Error:</b> {str(e)}

<b>Kemungkinan penyebab:</b>
• Balance tidak cukup
• Symbol tidak tersedia untuk futures
• Position size terlalu kecil (coba naikkan)
• Leverage tidak valid (coba kurangi)
• Koneksi ke Binance bermasalah

<b>Debug Info:</b>
Symbol: {analysis.get('symbol', 'N/A')}
Position Size: ${position_size:.2f}
Leverage: {leverage}x

<b>Solusi:</b>
1. Coba naikkan position size
2. Atau kurangi leverage
3. Atau pilih symbol lain

Silakan coba lagi!
""",
            reply_markup=InlineKeyboardMarkup([
                [InlineKeyboardButton("🔄 Coba Lagi", callback_data="execute_multi_tf")],
                [InlineKeyboardButton("🏠 Menu Utama", callback_data="back_to_start")]
            ])
        )

    return SELECTING_MODE


@only_allowed
async def handle_execute_cancel(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Cancel execution"""
    query = update.callback_query
    await query.answer("❌ Dibatalkan", show_alert=True)
    return await start(update, context)


# =====================================================================
# ========================= BUTTON ROUTER =============================
# =====================================================================

async def button_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Central button handler - routes to appropriate function"""
    query = update.callback_query
    
    try:
        await query.answer()
        callback_data = query.data

        logging.info(f"Button pressed: {callback_data}")

        # Route to appropriate handler
        if callback_data == "back_to_start":
            return await start(update, context)
        elif callback_data == "help":
            return await help_command(update, context)
        elif callback_data.startswith("monitor_"):
            return await handle_monitor_actions(update, context)
        elif callback_data.startswith("mode_"):
            return await handle_mode_selection(update, context)
        elif callback_data.startswith("token_"):
            return await handle_token_selection(update, context)
        elif callback_data.startswith("strategy_"):
            return await handle_strategy_selection(update, context)
        elif callback_data == "execute_confirm_manual":  # NEW
            return await handle_execute_confirm_manual(update, context)
        elif callback_data == "execute_cancel":
            return await handle_execute_cancel(update, context)
        elif callback_data == "execute_multi_tf":
            return await handle_execute_futures(update, context)
        else:
            logging.warning(f"Unknown callback: {callback_data}")
            await query.answer("❌ Invalid action", show_alert=True)
            return SELECTING_MODE

    except Exception as e:
        logging.error(f"Button handler error: {e}")
        import traceback
        logging.error(traceback.format_exc())
        
        try:
            await query.answer("❌ Error. Coba lagi.", show_alert=True)
        except:
            pass
        
        return await start(update, context)


# =====================================================================
# ========================= ERROR HANDLER ==============================
# =====================================================================

async def error_handler(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Global error handler"""
    logging.error(f"Update {update} caused error {context.error}")
    import traceback
    logging.error(traceback.format_exc())
    
    try:
        if update and update.effective_message:
            await update.effective_message.reply_text(
                "❌ <b>Terjadi kesalahan!</b>\n\n"
                "Bot akan restart. Silakan /start kembali.",
                parse_mode="HTML"
            )
    except Exception as e:
        logging.error(f"Error in error handler: {e}")


# =====================================================================
# ========================= MAIN =======================================
# =====================================================================

def main():
    """Start the bot"""
    try:
        logging.info("🤖 Starting bot...")
        
        app = ApplicationBuilder().token(TELEGRAM_TOKEN).build()

        # ⭐ UPDATE: Conversation handler dengan state baru
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
                ],
                # ⭐ NEW STATES
                INPUTTING_SIZE: [
                    CallbackQueryHandler(button_handler),
                    MessageHandler(filters.TEXT & ~filters.COMMAND, handle_size_input)
                ],
                INPUTTING_LEVERAGE: [
                    CallbackQueryHandler(button_handler),
                    MessageHandler(filters.TEXT & ~filters.COMMAND, handle_leverage_input)
                ]
            },
            fallbacks=[
                CommandHandler("start", start),
                CommandHandler("help", help_command)
            ],
            allow_reentry=True,
            conversation_timeout=600
        )

        app.add_handler(conv_handler)
        app.add_handler(CommandHandler("help", help_command))
        app.add_error_handler(error_handler)

        logging.info("✅ Bot started successfully!")
        logging.info("="*60)
        logging.info("Bot is running... Press Ctrl+C to stop.")
        logging.info("="*60)
        
        app.run_polling(
            allowed_updates=Update.ALL_TYPES,
            drop_pending_updates=True
        )

    except Exception as e:
        logging.error(f"❌ Failed to start bot: {e}")
        import traceback
        logging.error(traceback.format_exc())
        raise


if __name__ == "__main__":
    main()


# =====================================================================
# NOTE: Jangan lupa tambahkan semua function dari Part 1 & Part 2
# yang belum ada (start, handle_mode_selection, handle_token_selection, 
# handle_strategy_selection, handle_monitor_actions, help_command, dll)
# =====================================================================
