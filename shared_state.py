# shared_state.py - Shared State Management (FIXED VERSION)

import json
import os
from datetime import datetime
from threading import Lock


class SharedState:
    def __init__(self, state_file="trading_state.json"):
        self.state_file = state_file
        self.lock = Lock()
        if not os.path.exists(self.state_file):
            self._init_state()

    # ============================================================
    # ========================= INIT =============================
    # ============================================================

    def _default_state(self):
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
            },
        }

    def _init_state(self):
        self._write_state(self._default_state())

    # ============================================================
    # ========================= IO ===============================
    # ============================================================

    def _read_state(self):
        try:
            with self.lock:
                if not os.path.exists(self.state_file):
                    self._init_state()

                with open(self.state_file, "r") as f:
                    data = json.load(f)

                # Safety: jika kosong / corrupt
                if not isinstance(data, dict) or not data:
                    self._init_state()
                    return self._default_state()

                return data
        except Exception:
            # Re-init jika file rusak
            self._init_state()
            return self._default_state()

    def _write_state(self, state):
        try:
            with self.lock:
                state["last_update"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

                tmp_file = f"{self.state_file}.tmp"
                with open(tmp_file, "w") as f:
                    json.dump(state, f, indent=2)

                os.replace(tmp_file, self.state_file)
        except Exception as e:
            print(f"Error writing state: {e}")

    # ============================================================
    # ========================= PUBLIC ===========================
    # ============================================================

    def get_state(self):
        return self._read_state()

    def update_status(self, is_running, started_at=None):
        state = self._read_state()
        state["is_running"] = is_running
        if started_at:
            state["started_at"] = started_at
        self._write_state(state)

    def update_balance(self, balance_info):
        state = self._read_state()

        # Normalize keys (tanpa ubah struktur)
        state["balance"] = {
            "total": balance_info.get("total", balance_info.get("balance", 0)),
            "available": balance_info.get(
                "available", balance_info.get("availableBalance", 0)
            ),
            "unrealized_pnl": balance_info.get("unrealized_pnl", 0),
        }

        self._write_state(state)

    def update_positions(self, positions):
        state = self._read_state()
        state["open_positions"] = positions
        self._write_state(state)

    def add_trade_log(self, trade):
        state = self._read_state()
        state["trade_log"].append(trade)

        # Batasi ukuran log
        state["trade_log"] = state["trade_log"][-200:]

        state["stats"]["total_trades"] += 1
        self._write_state(state)

    def update_stats(self, profit_loss):
        state = self._read_state()
        if profit_loss > 0:
            state["stats"]["winning_trades"] += 1
            state["stats"]["total_profit"] += profit_loss
        else:
            state["stats"]["losing_trades"] += 1
            state["stats"]["total_loss"] += abs(profit_loss)
        self._write_state(state)

    def add_error(self, error_msg):
        state = self._read_state()
        state["errors"].append({
            "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "message": error_msg,
        })
        state["errors"] = state["errors"][-50:]
        self._write_state(state)

    def update_last_scan(self, scan_info):
        state = self._read_state()
        state["last_scan"] = scan_info
        self._write_state(state)
