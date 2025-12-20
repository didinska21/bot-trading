# shared_state.py - IMPROVED VERSION with Better Thread Safety & Error Handling

import json
import os
from datetime import datetime
from threading import Lock
import logging

logger = logging.getLogger(__name__)


class SharedState:
    """
    Thread-safe shared state manager for auto trading bot
    Manages persistent state storage with JSON file backend
    """
    
    def __init__(self, state_file="trading_state.json"):
        self.state_file = state_file
        self.lock = Lock()
        self._initialize()
        logger.info(f"✅ SharedState initialized with file: {state_file}")

    # ============================================================
    # ========================= INIT =============================
    # ============================================================

    def _initialize(self):
        """Initialize state file if not exists"""
        if not os.path.exists(self.state_file):
            logger.info("Creating new state file...")
            self._init_state()
        else:
            # Validate existing state
            try:
                state = self._read_state()
                if not isinstance(state, dict) or not state:
                    logger.warning("Invalid state file detected, re-initializing...")
                    self._init_state()
                else:
                    logger.info("Existing state file loaded successfully")
            except Exception as e:
                logger.error(f"Error loading state file: {e}, re-initializing...")
                self._init_state()

    def _default_state(self):
        """Return default state structure"""
        return {
            "is_running": False,
            "started_at": None,
            "last_update": None,
            "balance": {
                "total": 0,
                "available": 0,
                "unrealized_pnl": 0,
            },
            "current_position": None,
            "open_positions": [],
            "trade_log": [],
            "stats": {
                "total_trades": 0,
                "winning_trades": 0,
                "losing_trades": 0,
                "total_profit": 0,
                "total_loss": 0,
                "win_rate": 0,
                "avg_profit": 0,
                "avg_loss": 0,
                "max_profit": 0,
                "max_loss": 0,
            },
            "last_scan": None,
            "last_signal": None,
            "errors": [],
            "config": {
                "max_leverage": 20,
                "position_size_pct": 15,
                "max_loss_pct": 17.5,
                "min_confidence": 85,
                "timeframe": "1h",
                "auto_trade_enabled": False,
            },
            "daily_stats": {
                "date": datetime.now().strftime("%Y-%m-%d"),
                "trades": 0,
                "profit": 0,
                "loss": 0,
            }
        }

    def _init_state(self):
        """Initialize state with default values"""
        try:
            self._write_state(self._default_state())
            logger.info("✅ State initialized with default values")
        except Exception as e:
            logger.error(f"Failed to initialize state: {e}")
            raise

    # ============================================================
    # ========================= IO ===============================
    # ============================================================

    def _read_state(self):
        """Read state from file with thread safety"""
        try:
            with self.lock:
                if not os.path.exists(self.state_file):
                    logger.warning("State file not found, initializing...")
                    self._init_state()
                    return self._default_state()

                with open(self.state_file, "r", encoding="utf-8") as f:
                    data = json.load(f)

                # Safety: validate structure
                if not isinstance(data, dict) or not data:
                    logger.warning("Invalid state structure, resetting...")
                    self._init_state()
                    return self._default_state()

                return data
                
        except json.JSONDecodeError as e:
            logger.error(f"JSON decode error: {e}, re-initializing state")
            self._init_state()
            return self._default_state()
        except Exception as e:
            logger.error(f"Error reading state: {e}")
            return self._default_state()

    def _write_state(self, state):
        """Write state to file with thread safety and atomic write"""
        try:
            with self.lock:
                # Update timestamp
                state["last_update"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

                # Write to temporary file first (atomic write)
                tmp_file = f"{self.state_file}.tmp"
                with open(tmp_file, "w", encoding="utf-8") as f:
                    json.dump(state, f, indent=2, ensure_ascii=False)

                # Replace original file atomically
                os.replace(tmp_file, self.state_file)
                
        except Exception as e:
            logger.error(f"Error writing state: {e}")
            # Try to clean up tmp file
            try:
                if os.path.exists(f"{self.state_file}.tmp"):
                    os.remove(f"{self.state_file}.tmp")
            except:
                pass

    # ============================================================
    # ========================= PUBLIC API =======================
    # ============================================================

    def get_state(self):
        """Get current state"""
        return self._read_state()

    def update_status(self, is_running, started_at=None):
        """Update bot running status"""
        try:
            state = self._read_state()
            state["is_running"] = is_running
            if started_at:
                state["started_at"] = started_at
            self._write_state(state)
            logger.info(f"Status updated: running={is_running}")
        except Exception as e:
            logger.error(f"Error updating status: {e}")

    def update_balance(self, balance_info):
        """Update balance information"""
        try:
            state = self._read_state()

            # Normalize keys (handle different formats)
            state["balance"] = {
                "total": float(balance_info.get("total", balance_info.get("balance", 0))),
                "available": float(balance_info.get(
                    "available", balance_info.get("availableBalance", 0)
                )),
                "unrealized_pnl": float(balance_info.get("unrealized_pnl", 0)),
            }

            self._write_state(state)
            logger.debug("Balance updated")
        except Exception as e:
            logger.error(f"Error updating balance: {e}")

    def update_positions(self, positions):
        """Update open positions list"""
        try:
            state = self._read_state()
            state["open_positions"] = positions
            self._write_state(state)
            logger.debug(f"Positions updated: {len(positions)} open")
        except Exception as e:
            logger.error(f"Error updating positions: {e}")

    def add_trade_log(self, trade):
        """Add trade to log with automatic cleanup"""
        try:
            state = self._read_state()
            
            # Add timestamp if not present
            if "timestamp" not in trade:
                trade["timestamp"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            
            state["trade_log"].append(trade)

            # Limit log size to last 200 trades
            state["trade_log"] = state["trade_log"][-200:]

            # Update total trades counter
            state["stats"]["total_trades"] += 1
            
            self._write_state(state)
            logger.info(f"Trade logged: {trade.get('symbol', 'N/A')}")
        except Exception as e:
            logger.error(f"Error adding trade log: {e}")

    def update_stats(self, profit_loss):
        """Update trading statistics"""
        try:
            state = self._read_state()
            pnl = float(profit_loss)
            
            if pnl > 0:
                state["stats"]["winning_trades"] += 1
                state["stats"]["total_profit"] += pnl
                
                # Update max profit
                if pnl > state["stats"].get("max_profit", 0):
                    state["stats"]["max_profit"] = pnl
                    
            else:
                state["stats"]["losing_trades"] += 1
                state["stats"]["total_loss"] += abs(pnl)
                
                # Update max loss
                if abs(pnl) > state["stats"].get("max_loss", 0):
                    state["stats"]["max_loss"] = abs(pnl)
            
            # Calculate derived metrics
            total = state["stats"]["total_trades"]
            wins = state["stats"]["winning_trades"]
            losses = state["stats"]["losing_trades"]
            
            if total > 0:
                state["stats"]["win_rate"] = (wins / total) * 100
            
            if wins > 0:
                state["stats"]["avg_profit"] = state["stats"]["total_profit"] / wins
            
            if losses > 0:
                state["stats"]["avg_loss"] = state["stats"]["total_loss"] / losses
            
            # Update daily stats
            today = datetime.now().strftime("%Y-%m-%d")
            if state["daily_stats"]["date"] != today:
                # Reset daily stats for new day
                state["daily_stats"] = {
                    "date": today,
                    "trades": 0,
                    "profit": 0,
                    "loss": 0,
                }
            
            state["daily_stats"]["trades"] += 1
            if pnl > 0:
                state["daily_stats"]["profit"] += pnl
            else:
                state["daily_stats"]["loss"] += abs(pnl)
            
            self._write_state(state)
            logger.info(f"Stats updated: PnL={pnl:.2f}")
            
        except Exception as e:
            logger.error(f"Error updating stats: {e}")

    def add_error(self, error_msg):
        """Add error to log with automatic cleanup"""
        try:
            state = self._read_state()
            state["errors"].append({
                "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                "message": str(error_msg),
            })
            
            # Keep only last 50 errors
            state["errors"] = state["errors"][-50:]
            
            self._write_state(state)
            logger.warning(f"Error logged: {error_msg}")
        except Exception as e:
            logger.error(f"Error adding error log: {e}")

    def update_last_scan(self, scan_info):
        """Update last scan information"""
        try:
            state = self._read_state()
            
            if "timestamp" not in scan_info:
                scan_info["timestamp"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            
            state["last_scan"] = scan_info
            self._write_state(state)
            logger.debug("Last scan updated")
        except Exception as e:
            logger.error(f"Error updating last scan: {e}")

    def update_config(self, config_updates):
        """Update configuration"""
        try:
            state = self._read_state()
            state["config"].update(config_updates)
            self._write_state(state)
            logger.info(f"Config updated: {config_updates}")
        except Exception as e:
            logger.error(f"Error updating config: {e}")

    def reset_stats(self):
        """Reset all statistics"""
        try:
            state = self._read_state()
            state["stats"] = self._default_state()["stats"]
            state["trade_log"] = []
            state["errors"] = []
            self._write_state(state)
            logger.info("Stats reset")
        except Exception as e:
            logger.error(f"Error resetting stats: {e}")

    def get_daily_stats(self):
        """Get today's statistics"""
        try:
            state = self._read_state()
            daily = state.get("daily_stats", {})
            
            # Reset if different day
            today = datetime.now().strftime("%Y-%m-%d")
            if daily.get("date") != today:
                return {
                    "date": today,
                    "trades": 0,
                    "profit": 0,
                    "loss": 0,
                }
            
            return daily
        except Exception as e:
            logger.error(f"Error getting daily stats: {e}")
            return {}

    def backup_state(self, backup_file=None):
        """Create backup of current state"""
        try:
            if backup_file is None:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                backup_file = f"{self.state_file}.backup_{timestamp}"
            
            state = self._read_state()
            with open(backup_file, "w", encoding="utf-8") as f:
                json.dump(state, f, indent=2, ensure_ascii=False)
            
            logger.info(f"State backed up to: {backup_file}")
            return backup_file
        except Exception as e:
            logger.error(f"Error backing up state: {e}")
            return None

    def restore_state(self, backup_file):
        """Restore state from backup"""
        try:
            with open(backup_file, "r", encoding="utf-8") as f:
                state = json.load(f)
            
            self._write_state(state)
            logger.info(f"State restored from: {backup_file}")
            return True
        except Exception as e:
            logger.error(f"Error restoring state: {e}")
            return False


# ============================================================
# ========================= TESTING ==========================
# ============================================================

if __name__ == "__main__":
    """Test SharedState functionality"""
    
    print("="*60)
    print("TESTING SHARED_STATE.PY")
    print("="*60)
    
    # Create test instance
    test_file = "test_state.json"
    state_manager = SharedState(test_file)
    
    # Test 1: Get initial state
    print("\n1. Initial State:")
    state = state_manager.get_state()
    print(f"   Running: {state['is_running']}")
    print(f"   Total trades: {state['stats']['total_trades']}")
    
    # Test 2: Update status
    print("\n2. Update Status:")
    state_manager.update_status(True, datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    state = state_manager.get_state()
    print(f"   Running: {state['is_running']}")
    
    # Test 3: Update balance
    print("\n3. Update Balance:")
    state_manager.update_balance({
        "total": 1000,
        "available": 950,
        "unrealized_pnl": 50
    })
    state = state_manager.get_state()
    print(f"   Balance: ${state['balance']['available']:.2f}")
    
    # Test 4: Add trade log
    print("\n4. Add Trade Log:")
    state_manager.add_trade_log({
        "symbol": "BTCUSDT",
        "side": "LONG",
        "entry_price": 50000,
        "exit_price": 51000,
        "pnl": 100
    })
    state = state_manager.get_state()
    print(f"   Total trades: {state['stats']['total_trades']}")
    
    # Test 5: Update stats
    print("\n5. Update Stats:")
    state_manager.update_stats(100)  # Profit
    state_manager.update_stats(-50)  # Loss
    state = state_manager.get_state()
    print(f"   Winning: {state['stats']['winning_trades']}")
    print(f"   Losing: {state['stats']['losing_trades']}")
    print(f"   Win Rate: {state['stats']['win_rate']:.2f}%")
    
    # Test 6: Add error
    print("\n6. Add Error:")
    state_manager.add_error("Test error message")
    state = state_manager.get_state()
    print(f"   Total errors: {len(state['errors'])}")
    
    # Test 7: Backup
    print("\n7. Backup State:")
    backup_file = state_manager.backup_state()
    print(f"   Backup created: {backup_file}")
    
    # Test 8: Get daily stats
    print("\n8. Daily Stats:")
    daily = state_manager.get_daily_stats()
    print(f"   Date: {daily['date']}")
    print(f"   Trades: {daily['trades']}")
    
    # Cleanup
    print("\n9. Cleanup:")
    try:
        os.remove(test_file)
        if backup_file and os.path.exists(backup_file):
            os.remove(backup_file)
        print("   Test files cleaned up")
    except:
        pass
    
    print("\n" + "="*60)
    print("All tests completed!")
    print("="*60)
