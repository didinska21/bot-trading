# shared_state.py - Shared State Management
import json
import os
from datetime import datetime
from threading import Lock

class SharedState:
    def __init__(self, state_file='trading_state.json'):
        self.state_file = state_file
        self.lock = Lock()
        if not os.path.exists(state_file):
            self._init_state()
    
    def _init_state(self):
        initial_state = {
            'is_running': False,
            'started_at': None,
            'last_update': None,
            'balance': {'total': 0, 'available': 0, 'unrealized_pnl': 0},
            'current_position': None,
            'open_positions': [],
            'trade_log': [],
            'stats': {
                'total_trades': 0,
                'winning_trades': 0,
                'losing_trades': 0,
                'total_profit': 0,
                'total_loss': 0
            },
            'last_scan': None,
            'last_signal': None,
            'errors': [],
            'config': {
                'max_leverage': 20,
                'position_size_pct': 15,
                'max_loss_pct': 17.5,
                'min_confidence': 85,
                'timeframe': '1h'
            }
        }
        self._write_state(initial_state)
    
    def _read_state(self):
        try:
            with self.lock:
                with open(self.state_file, 'r') as f:
                    return json.load(f)
        except:
            return {}
    
    def _write_state(self, state):
        try:
            with self.lock:
                state['last_update'] = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
                with open(self.state_file, 'w') as f:
                    json.dump(state, f, indent=2)
        except Exception as e:
            print(f"Error writing state: {e}")
    
    def get_state(self):
        return self._read_state()
    
    def update_status(self, is_running, started_at=None):
        state = self._read_state()
        state['is_running'] = is_running
        if started_at:
            state['started_at'] = started_at
        self._write_state(state)
    
    def update_balance(self, balance_info):
        state = self._read_state()
        state['balance'] = balance_info
        self._write_state(state)
    
    def update_positions(self, positions):
        state = self._read_state()
        state['open_positions'] = positions
        self._write_state(state)
    
    def add_trade_log(self, trade):
        state = self._read_state()
        state['trade_log'].append(trade)
        state['stats']['total_trades'] += 1
        self._write_state(state)
    
    def update_stats(self, profit_loss):
        state = self._read_state()
        if profit_loss > 0:
            state['stats']['winning_trades'] += 1
            state['stats']['total_profit'] += profit_loss
        else:
            state['stats']['losing_trades'] += 1
            state['stats']['total_loss'] += abs(profit_loss)
        self._write_state(state)
    
    def add_error(self, error_msg):
        state = self._read_state()
        error = {
            'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            'message': error_msg
        }
        state['errors'].append(error)
        state['errors'] = state['errors'][-50:]  # Keep last 50 errors
        self._write_state(state)
    
    def update_last_scan(self, scan_info):
        state = self._read_state()
        state['last_scan'] = scan_info
        self._write_state(state)
