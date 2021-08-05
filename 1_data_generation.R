# clean environment ----
rm(list = ls())

# load libraries functions ----
library(xts)
library(TTR)
library(quadprog)
source("custom_functions.R")

# import data ----
stock_data <- read.csv("data/stock_prices_daily.csv")
stock_prices <- xts(stock_data[,-1], order.by = as.Date(stock_data[,"Date"], "%Y-%m-%d"))
stock_prices <- stock_picker(stock_prices,
                             tickers = c("DIS", "AMZN", "TGT", "PG", "WMT", "XOM", "JPM", "MRK", 
                                        "GILD", "HON", "MA", "TXN", "DD", "AMT", "NEE"))
tickers <- colnames(stock_prices)

vix <- read.csv("data/vix_daily.csv", skip = 1)
vix <- xts(vix$VIX.Close, order.by = as.Date(vix$Date, "%m/%d/%Y"))
vix <- as.xts(vix[index(stock_prices)])
names(vix) <- "VIX"

pcr <- read.csv("data/equity_pcr_daily.csv", skip = 2)
pcr <- xts(pcr$P.C.Ratio, order.by = as.Date(pcr$DATE, "%m/%d/%Y"))
names(pcr) <- "PCR"

ff_3_factor <- read.csv("data/ff_3_factors_daily.csv", skip = 4)
ff_3_factor <- xts(ff_3_factor[,-1], order.by = as.Date(as.character(ff_3_factor[,1]), "%Y%m%d"))
ff_3_factor <- as.xts(ff_3_factor[index(stock_prices)])
ff_3_factor <- ff_3_factor / 100
names(ff_3_factor) <- c("mkt", "smb", "hml", "rf")

# check whether all time series have the same time index and no NA ----
sum((index(stock_prices) == index(pcr)) == FALSE)
sum((index(stock_prices) == index(vix)) == FALSE)
sum((index(stock_prices) == index(ff_3_factor)) == FALSE)

sum(is.na(stock_prices))
sum(is.na(vix))
sum(is.na(pcr))
sum(is.na(ff_3_factor))

# calculate daily excess stock returns ----
rf <- ff_3_factor[,4]
daily_returns <- diff(stock_prices, lag = 1) / stock_prices
daily_returns <- sweep(daily_returns, 1, rf,"-")

# calculate 14-day RSI ----
rsi14 <- apply(stock_prices, 2, RSI)
rsi14 <- xts(rsi14, order.by = index(stock_prices))
names(rsi14) <- tickers

# generate targets: optimal portfolios (in-sample, long-only, risk aversion = 1) ----
  # mv.r = mean-variance optimal with rolling window 
targets_mv.r <- markowitz(daily_returns, mode = "mv", window = 250)
colnames(targets_mv.r) <- tickers
targets_mv.r <- xts(targets_mv.r, order.by = index(stock_prices))

  # mv.e = mean-variance optimal with expanding window 
targets_mv.e <- markowitz(daily_returns, mode = "mv")
colnames(targets_mv.e) <- tickers
targets_mv.e <- xts(targets_mv.e, order.by = index(stock_prices))

  # min.r = minimum-variance optimal with rolling window 
targets_min.r <- markowitz(daily_returns, mode = "min", window = 250)
colnames(targets_min.r) <- tickers
targets_min.r <- xts(targets_min.r, order.by = index(stock_prices))

  # min.e = minimum-variance optimal with expanding window   
targets_min.e <- markowitz(daily_returns, mode = "min")
colnames(targets_min.e) <- tickers
targets_min.e <- xts(targets_min.e, order.by = index(stock_prices))

# combine all features ----
features <- as.xts(merge.zoo(daily_returns, rsi14, vix, pcr, 
                             suffixes = c("returns", "rsi", "", "")))   # merge.xts suffixes not working !!!

# remove first 14 rows, because NA from calculating RSI ----
features <- tail(features, n = -14)
targets_mv.r <- tail(targets_mv.r, n = -14)
targets_mv.e <- tail(targets_mv.e, n = -14)
targets_min.r <- tail(targets_min.r, n = -14)
targets_min.e <- tail(targets_min.e, n = -14)
daily_returns <- tail(daily_returns, n = -14)
ff_3_factor <- tail(ff_3_factor, n = -14)

# save data as .rds ----
data <- list(
  features = features,
  targets_mv.r = targets_mv.r,
  targets_mv.e = targets_mv.e,
  targets_min.r = targets_min.r,
  targets_min.e = targets_min.e,
  daily_returns = daily_returns,
  benchmarks = ff_3_factor[,1:3],
  rf_rate = ff_3_factor[,4])

saveRDS(data, file = "temp/dataset.rds")