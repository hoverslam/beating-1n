# ###
# This script gets the stock tickers from the iShares S&P 100 ETF (OEF) Holdings 
# (downloaded 2020-09-03) and creates a dataset of closing prices with stocks  
# where data is available from 2006-11-01 to 2019-10-02  
# ###

# clean environment ----
rm(list = ls())

# load libraries functions ----
library(pdfetch)
library(zoo)

# import data ----
data <- read.csv("data/oef_holdings.csv", sep = ",", header = TRUE, skip = 9)

# remove all non-equity entries in index holdings ----
equities <- data[which(data[,"Asset.Class"] == "Equity"),]

# clean up data manually ----
equities <- equities[-which(equities[,"Ticker"] == "GOOG"),]      # REMOVE: Alphabet Class C shares
equities$Ticker[equities$Ticker == "BRKB"] <- "BRK-B"             # CHANGE: Berkshire Hathaway Ticker from "BRKB" to "BRK-B"

# get ticker symbols and fetch stock closing prices from Yahoo Finance ----
tickers <- equities[,"Ticker"]
company_names <- equities[,"Name"]
stock_data <- pdfetch_YAHOO(tickers, fields = "close", from = as.Date("2006-11-01"), to = as.Date("2019-10-05"))

# remove stocks where the closing prices isn't available over the whole period ----
stock_data <- stock_data[,colSums(is.na(stock_data)) == 0]

# save data as .csv ----
write.zoo(stock_data, file = "temp/stock_prices_daily.csv" , sep = ",", index.name = "Date", col.names = TRUE)