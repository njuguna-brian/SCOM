This mini-project explores the use of classification models for price prediction using Safaricom LTD (Ticker: **SCOM**) stock data listed in the Nairobi Securities Exchange. The goal is to analyze the historical price data of SCOM and use classification models to predict future prices. The project will use a variety of classification models such as Generalized Linear Models, Random Forests, and Support Vector Machines, to compare their performance in predicting the stock prices. The results will be evaluated using performance metrics such as accuracy, precision, recall, and F1-score. The study aims to provide insights into the effectiveness of classification models for price prediction and their suitability for the SCOM stock data.

# Features: 

### Moving Average Convergence Divergence (MACD)
The MACD is a trend-following momentum indicator that shows the relationship between two moving averages of a security's price. The MACD is calculated by subtracting the long-period Moving Average (EMA/SMA) from the short-period Moving average. 
$$MACD = MA_{short} - MA_{long}$$
### Relative Strength Index (RSI):
The RSI is a momentum oscillator that measures the speed and change of price movements. It compares the magnitude of recent gains to recent losses to determine overbought and oversold conditions.
$$RSI = 100 - \frac{100}{1 + \frac{\text{average gain}}{\text{average loss}}}$$

### 3.  Average Directional Index (ADX): 
The ADX is a trend strength indicator that measures the strength of a trend regardless of its direction. It is calculated by smoothing the price range over a specified time period and comparing the difference between the highs and lows.

 $$ADX = \text{SMA}\left(\frac{|\text{DI}^+ - \text{DI}^-|}{(\text{+DI} + \text{DI}^-)}, n\right)$$

### 4.  Bollinger Bands:
Bollinger Bands are a volatility indicator that consists of three bands, an upper, lower, and middle band. The middle band is a  moving average, while the upper and lower bands are calculated by adding and subtracting a multiple of the standard deviation of the price from the middle band.


$$\text{Middle Band} = \text{SMA}(price, n)$$
$$\text{Upper Band} = \text{Middle Band} + k * \text{SD}(price, n)$$
$$\text{Lower Band} = \text{Middle Band} - k * \text{SD}(price, n)$$

### 5.  Stochastic Oscillator:
The Stochastic Oscillator is a momentum indicator that compares a security's closing price to its price range over a given time period. The indicator is based on the premise that closing prices should close near the same direction as the prevailing trend.

$$\%K = \frac{\text{(Current Close - Lowest Low)}}{(\text{Highest High - Lowest Low})} * 100$$ $$\%D = \text{SMA}(\%K, n)$$
### 6.  Average True Range (ATR):
The ATR is a volatility indicator that measures the average range of price movement of a security over a given time period.

$$ATR = \text{SMA}(TR, n)$$

### 7. The Commodity Channel Index (CCI):
CCI is a momentum-based indicator that measures the deviation of the current price from its statistical mean. It is used to identify cyclical trends in a security's price.

$$CCI = \frac{(\text{Typical Price} - \text{SMA(Typical Price, n)})}{(0.015 * \text{Mean Deviation})}$$

### Motivation for use:
All of these technical indicators are commonly used in financial analysis to make predictions about future price movements. 

1. The MACD, RSI, and Stochastic Oscillator are all momentum-based indicators that are used to identify overbought and oversold conditions, as well as potential trend reversals. 
2. The ADX and ATR are both trend strength indicators that are used to measure the strength of a trend and its volatility. Bollinger Bands are a volatility indicator that are used to identify potential breakouts and trend reversals.
3. Finally, the CCI is used to identify cyclical trends in a security's price.

---
