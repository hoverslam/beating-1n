stock_picker <- function (data, n = 10, tickers = NULL) {
  # input: dataset of stock closing prices
  # output: closing prices of n random stock or by given ticker symbols
  if(is.null(tickers)) tickers <- sample(colnames(data), size = n)
  return(stock_prices[,tickers])
}

markowitz <- function(returns, mode, window = 0) {
  # input: matrix of returns x stocks, mv/min size of rolling window (0 = expanding window)
  # output: markowitz optimal portfolio weights (long-only)

  solution <- array(NA, dim = dim(returns))
  
  if (window > 0) {
    start_index <- window + 1     # start loop one index later, because first returns are NA
    for(i in (start_index):dim(returns)[1]) {
      cur_returns <- returns[(i-window+1):i,]
      if (mode == "mv") solution[i,] <- mv_optimal(cur_returns)
      if (mode == "min") solution[i,] <- min_optimal(cur_returns)
    }
  } else {
    start_index <- 30             # start loop with index 3, because with less samples cov matrix is not definite
    for(i in start_index:dim(returns)[1]) {
      cur_returns <- returns[2:i,]
      if (mode == "mv") solution[i,] <- mv_optimal(cur_returns)
      if (mode == "min") solution[i,] <- min_optimal(cur_returns)
    }
  }

  return(solution)
}

mv_optimal <- function(returns) {
  mu <- apply(returns, 2, mean)
  cov <- cov(returns)
  n <- length(mu)

  Amat <- cbind(rep(1, n), diag(1, nrow = n))
  bvec <- c(1, rep(0, n))
  weights <- solve.QP(Dmat = cov, dvec = mu, Amat, bvec, meq = 1)$solution
  
  return(weights)
}

min_optimal <- function(returns) {
  cov <- cov(returns)
  n <- ncol(returns)

  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  weights <- solve.QP(Dmat = 2*cov, dvec, Amat, bvec, meq = 1)$solution
  
  return(weights)
}

seq_generator <- function(x, y, lookback, delay, min_index, max_index, shuffle = FALSE) {
  # output: a list ([[1]] = samples, [[2]] = targets) of sequences and the 
  # corresponding targets for multiple stocks / modified version of the 
  # generator function from Chollet, 2018: Deep learning with R
  if (is.null(max_index)) {
    max_index <- nrow(data) - delay
  } else {
    max_index <- max_index - delay
  }
  
  batch_size <- max_index - min_index - lookback
  
  if (shuffle) {
    rows <- sample(c((min_index+lookback):max_index), size = batch_size)
  } else {
    i <- min_index + lookback
    rows <- c(i:min(i+batch_size, max_index))
  }

  samples <- array(NA, dim = c(length(rows), lookback, ncol(x)))
  targets <- array(NA, dim = c(length(rows), ncol(y)))

  for (j in 1:length(rows)) {
    indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, length.out = dim(samples)[[2]])
    samples[j,,] <- x[indices,]
    targets[j,] <- y[rows[[j]]+delay,]
  }
  
  list(samples, targets)
}

portfolio_returns <- function(returns, weights) {
  # input: matrix of stock returns and matrix of portfolio weights
  # output: portfolio returns
  len <- nrow(returns)
  portfolio <- rep(NA, len)
  for (i in 1:len) {
    portfolio[i] <- returns[i,] %*% weights[i,]
  }
  return(portfolio)
}

net_portfolio_returns <- function(returns, weights, tc = 0) {
  # input: matrix of portfolio weights, matrix of stock returns, transactions costs in bps
  # output: portfolio net returns after transaction costs
  max_index <- nrow(weights) - 1
  net_returns <- rep(NA, max_index)
  for (i in 1:max_index) {
    wealth_before_rebalancing <- weights[i,] * (1 + returns[i,])
    weights_before_rebalancing <- wealth_before_rebalancing / sum(wealth_before_rebalancing)
    turnover <- sum(abs(weights[i+1,] - weights_before_rebalancing))
    
    net_returns[i] <- (1 + returns[i,] %*% weights[i,]) * (1 - tc * turnover) - 1
  }
  
  return(net_returns)
}

sharpe_ratio <- function(returns, rf = 0) {
  # input: vector of returns, risk free rate
  # output: sharpe ratio
  mu <- mean(returns)
  sigma <- sd(returns)
  sr <- mu / sigma
  return(sr)
}

average_turnover <- function(weights, returns) {
  # input: matrix of portfolio weights, matrix of stock returns
  # output: portfolio turnover, defined as the average
  #   sum of the absolute value of the trades across all stocks
  max_index <- nrow(weights) - 1
  turnover <- rep(NA, max_index)
  for (i in 1:max_index) {
    wealth_before_rebalancing <- weights[i,] * (1 + returns[i,])
    weights_before_rebalancing <- wealth_before_rebalancing / sum(wealth_before_rebalancing)
    turnover[i] <- sum(abs(weights[i+1,] - weights_before_rebalancing))
  }
  return(list(mean(turnover), turnover))
}