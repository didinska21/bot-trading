# scanner.py - Market Scanner untuk Top Movers & High Confidence Signals
import logging
from binance.client import Client
from config import BINANCE_API_KEY, BINANCE_API_SECRET
from ai import analyze_with_gpt

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class MarketScanner:
    def __init__(self):
        """Initialize Market Scanner"""
        self.client = Client(api_key=BINANCE_API_KEY, api_secret=BINANCE_API_SECRET)
        self.client.API_URL = 'https://fapi.binance.com/fapi'  # Futures API
        
    def get_top_movers(self, limit=20, min_volume_usdt=1000000):
        """
        Get top gainers and losers in futures market
        
        Args:
            limit: Number of top movers to return
            min_volume_usdt: Minimum 24h volume in USDT
        
        Returns:
            List of top movers with price change %
        """
        try:
            # Get all futures tickers
            tickers = self.client.futures_ticker()
            
            # Filter USDT pairs with minimum volume
            usdt_pairs = []
            for ticker in tickers:
                if ticker['symbol'].endswith('USDT'):
                    volume_usdt = float(ticker['quoteVolume'])
                    if volume_usdt >= min_volume_usdt:
                        usdt_pairs.append({
                            'symbol': ticker['symbol'],
                            'price': float(ticker['lastPrice']),
                            'change_24h': float(ticker['priceChangePercent']),
                            'volume_24h': volume_usdt,
                            'high_24h': float(ticker['highPrice']),
                            'low_24h': float(ticker['lowPrice'])
                        })
            
            # Sort by absolute price change (top movers)
            sorted_pairs = sorted(usdt_pairs, key=lambda x: abs(x['change_24h']), reverse=True)
            
            # Get top gainers and losers
            top_gainers = sorted([p for p in sorted_pairs if p['change_24h'] > 0], 
                                key=lambda x: x['change_24h'], reverse=True)[:limit//2]
            
            top_losers = sorted([p for p in sorted_pairs if p['change_24h'] < 0], 
                               key=lambda x: x['change_24h'])[:limit//2]
            
            result = {
                'gainers': top_gainers,
                'losers': top_losers,
                'all_movers': sorted_pairs[:limit]
            }
            
            logger.info(f"Found {len(top_gainers)} gainers and {len(top_losers)} losers")
            return result
            
        except Exception as e:
            logger.error(f"Error scanning market: {e}")
            return None
    
    def get_klines_data(self, symbol, timeframe='1h', limit=100):
        """Get candlestick data for technical analysis"""
        try:
            klines = self.client.futures_klines(
                symbol=symbol,
                interval=timeframe,
                limit=limit
            )
            
            ohlcv = [{
                'time': k[0],
                'open': float(k[1]),
                'high': float(k[2]),
                'low': float(k[3]),
                'close': float(k[4]),
                'volume': float(k[5])
            } for k in klines]
            
            return ohlcv
            
        except Exception as e:
            logger.error(f"Error getting klines for {symbol}: {e}")
            return None
    
    def calculate_technical_score(self, ohlcv_data):
        """
        Calculate basic technical analysis score
        Returns score 0-100
        """
        if not ohlcv_data or len(ohlcv_data) < 50:
            return 0
        
        try:
            closes = [candle['close'] for candle in ohlcv_data]
            volumes = [candle['volume'] for candle in ohlcv_data]
            
            score = 0
            
            # 1. Trend Score (EMA alignment)
            ema_9 = sum(closes[-9:]) / 9
            ema_21 = sum(closes[-21:]) / 21
            ema_50 = sum(closes[-50:]) / 50
            
            current_price = closes[-1]
            
            # Bullish trend
            if current_price > ema_9 > ema_21 > ema_50:
                score += 30
            elif current_price > ema_9 > ema_21:
                score += 20
            elif current_price > ema_9:
                score += 10
            
            # Bearish trend
            if current_price < ema_9 < ema_21 < ema_50:
                score += 30
            elif current_price < ema_9 < ema_21:
                score += 20
            elif current_price < ema_9:
                score += 10
            
            # 2. Volume Score (increasing volume = stronger signal)
            avg_volume = sum(volumes[-20:]) / 20
            recent_volume = volumes[-1]
            
            if recent_volume > avg_volume * 1.5:
                score += 20
            elif recent_volume > avg_volume:
                score += 10
            
            # 3. Momentum Score (RSI-like simple calculation)
            price_changes = [closes[i] - closes[i-1] for i in range(-14, 0)]
            gains = [c for c in price_changes if c > 0]
            losses = [abs(c) for c in price_changes if c < 0]
            
            avg_gain = sum(gains) / len(gains) if gains else 0
            avg_loss = sum(losses) / len(losses) if losses else 0
            
            if avg_loss == 0:
                rsi = 100
            else:
                rs = avg_gain / avg_loss
                rsi = 100 - (100 / (1 + rs))
            
            # Good RSI range (30-70)
            if 30 <= rsi <= 70:
                score += 20
            elif 25 <= rsi <= 75:
                score += 10
            
            # 4. Volatility Score (high volatility = more opportunity)
            highs = [candle['high'] for candle in ohlcv_data[-20:]]
            lows = [candle['low'] for candle in ohlcv_data[-20:]]
            atr = sum([h - l for h, l in zip(highs, lows)]) / 20
            
            volatility_pct = (atr / current_price) * 100
            if 2 <= volatility_pct <= 5:  # Good volatility range
                score += 20
            elif 1 <= volatility_pct <= 6:
                score += 10
            
            return min(score, 100)
            
        except Exception as e:
            logger.error(f"Error calculating technical score: {e}")
            return 0
    
    def scan_for_signals(self, timeframe='1h', min_confidence=80, top_n=5):
        """
        Scan market for high confidence trading signals
        
        Args:
            timeframe: Timeframe for analysis
            min_confidence: Minimum confidence level required
            top_n: Number of best signals to return
        
        Returns:
            List of high confidence signals with AI analysis
        """
        try:
            logger.info(f"Scanning market for signals (timeframe: {timeframe}, min_confidence: {min_confidence}%)")
            
            # 1. Get top movers
            movers = self.get_top_movers(limit=30)
            if not movers:
                return []
            
            all_candidates = movers['all_movers']
            
            # 2. Analyze each candidate
            signals = []
            for candidate in all_candidates[:20]:  # Limit to 20 to avoid too many API calls
                symbol = candidate['symbol']
                
                logger.info(f"Analyzing {symbol}...")
                
                # Get candlestick data
                ohlcv = self.get_klines_data(symbol, timeframe)
                if not ohlcv:
                    continue
                
                # Calculate technical score
                tech_score = self.calculate_technical_score(ohlcv)
                
                # Only analyze if technical score is decent
                if tech_score < 50:
                    logger.info(f"{symbol} technical score too low: {tech_score}")
                    continue
                
                # Get AI analysis
                try:
                    ai_analysis = analyze_with_gpt(
                        symbol=symbol,
                        timeframe=timeframe,
                        last_candle=ohlcv[-1],
                        mode='futures'
                    )
                    
                    # Extract confidence level from AI response
                    confidence = self._extract_confidence(ai_analysis)
                    
                    if confidence >= min_confidence:
                        signals.append({
                            'symbol': symbol,
                            'confidence': confidence,
                            'tech_score': tech_score,
                            'price_change_24h': candidate['change_24h'],
                            'volume_24h': candidate['volume_24h'],
                            'current_price': candidate['price'],
                            'ai_analysis': ai_analysis,
                            'last_candle': ohlcv[-1]
                        })
                        
                        logger.info(f"✅ {symbol} - Confidence: {confidence}%, Tech Score: {tech_score}")
                    else:
                        logger.info(f"❌ {symbol} - Confidence too low: {confidence}%")
                        
                except Exception as e:
                    logger.error(f"Error analyzing {symbol}: {e}")
                    continue
            
            # 3. Sort by confidence and return top N
            signals.sort(key=lambda x: (x['confidence'], x['tech_score']), reverse=True)
            
            logger.info(f"Found {len(signals)} signals above {min_confidence}% confidence")
            return signals[:top_n]
            
        except Exception as e:
            logger.error(f"Error scanning for signals: {e}")
            return []
    
    def _extract_confidence(self, ai_response):
        """Extract confidence level from AI response text"""
        try:
            # Look for patterns like "Confidence Level: 85%" or "85%"
            import re
            
            # Try to find "Confidence Level" section
            confidence_match = re.search(r'Confidence Level.*?(\d+)%', ai_response, re.IGNORECASE)
            if confidence_match:
                return int(confidence_match.group(1))
            
            # Try to find standalone percentage near end of text
            percentages = re.findall(r'(\d+)%', ai_response[-500:])  # Look in last 500 chars
            if percentages:
                return int(percentages[-1])  # Take the last one
            
            # Default to 0 if not found
            return 0
            
        except Exception as e:
            logger.error(f"Error extracting confidence: {e}")
            return 0
    
    def get_signal_summary(self, signal):
        """Format signal summary for display"""
        return f"""
🎯 <b>{signal['symbol']}</b>

💰 Price: ${signal['current_price']:,.4f}
📈 24h Change: {signal['price_change_24h']:.2f}%
📊 Volume 24h: ${signal['volume_24h']:,.0f}

🔐 Confidence: {signal['confidence']}%
📊 Tech Score: {signal['tech_score']}/100

{signal['ai_analysis'][:500]}...
"""
