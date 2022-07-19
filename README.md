# Beating 1/N - A Machine Learning Approach

My bachelor thesis written at Vienna University of Economics and Business. A Long short-term memory (LSTM) architecture is used to train a recurrent neural network on portfolio weights based on Harry Markowitz’s Modern Portfolio Theory.


## Data

* equity_pcr_daily.csv, CBOE Equity Put/Call Ratio, daily, 2006-11-01 to 2019-10-04:<br/> 
  http://www.cboe.com/data/historical-options-data/volume-put-call-ratios (downloaded 2020-09-03)
* ff_3_factors_daily.csv, Fama/French 3 Factors incl. risk free rate, daily, 1926-07-01 to 2020-06-30:<br/>
  https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html (downloaded 2020-09-03)	
* oef_holdings.csv, Holdings from the iShares S&P 100 ETF:<br/>
  https://www.ishares.com/us/products/239723/ishares-sp-100-etf (downloaded 2020-09-03)
* stock_prices_daily.csv, Closing Prices SP100 Companies, daily, 2006-11-01 to 2019-10-04:<br/>
  fetched from Yahoo Finance with pdfetch (see sp100.R)
* vix_daily.csv, CBOE Volatility Index, daily, 2004-01-02 to 2020-09-02:<br/>
  http://www.cboe.com/products/vix-index-volatility/vix-options-and-futures/vix-index/vix-historical-data (downloaded 2020-09-03)
