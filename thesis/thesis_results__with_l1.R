# ###
# This script is the same as 3_performance_evaluation.R, but evaluates the predictions 
# used for thesis (thesis_predictions.rds). This is done because of slight variations in
# the predictions after each new training. Overall result is always the same!
# ###

# clean environment ----
rm(list = ls())

# import libraries and functions ----
library(SharpeR)
library(xts)
library(car)
source("custom_functions.R")

# import data ----
data <- readRDS("temp/dataset.rds")
predictions <- readRDS("thesis/predictions_with_l1.rds")
factors <- tail(data[["benchmarks"]], n = 250)
daily_returns <- tail(data[["daily_returns"]], n = 250)

  # calculating net returns costs use the last day, so we end up with 249 returns and
  # have to cut the last returns of the Fama-French factors
factors <- head(factors, n = -1)

# 1/N benchmark w/o transactions costs ----
n <- dim(daily_returns)[2]
ew_weights <- array(1/n, dim = c(250, n))
ew_returns <- net_portfolio_returns(daily_returns, ew_weights, 0)
ew_sr <- sharpe_ratio(ew_returns)
ew_capm_alpha <- lm(ew_returns ~ factors[,1])[["coefficients"]][["(Intercept)"]]
ew_ff_alpha <- lm(ew_returns ~ factors)[["coefficients"]][["(Intercept)"]]
ew_turnover <- average_turnover(ew_weights, daily_returns)

results_before_tc <- vector("list", length = length(predictions)+1)
names(results_before_tc) <- c("ew", names(predictions))
results_before_tc[[1]] <- list(sr = ew_sr,
                               capm_alpha = ew_capm_alpha,
                               ff_alpha = ew_ff_alpha,
                               turnover = ew_turnover[[2]],
                               mean_turnover = ew_turnover[[1]])

# strategy evaluation w/o transactions costs ----
for (i in 1:length(predictions)) {
  weights <- as.matrix(predictions[[i]])
  returns <- net_portfolio_returns(daily_returns, weights, 0)
  
  sr <- sharpe_ratio(returns)
  sr_significance <- sr_test(returns, ew_returns)$p.value
  
  capm_lm <- lm(returns ~ factors[,1])
  capm_test <- linearHypothesis(capm_lm, hypothesis.matrix = c(1,0), rhs = ew_capm_alpha, white.adjust = "hc0")
  
  ff_lm <- lm(returns ~ factors)
  ff_test <- linearHypothesis(ff_lm, hypothesis.matrix = c(1,0,0,0), rhs = ew_ff_alpha, white.adjust = "hc0")
  
  turnover <- average_turnover(weights, daily_returns)
  relative_turnover <- turnover[[1]] / ew_turnover[[1]]
  
  results_before_tc[[i+1]] <- list(sr = sr,
                                   sr_p.value = sr_significance,
                                   capm_alpha = capm_lm[["coefficients"]][["(Intercept)"]],
                                   capm_p.value = capm_test[2, 4],
                                   ff_alpha = ff_lm[["coefficients"]][["(Intercept)"]],
                                   ff_p.value = ff_test[2, 4],
                                   turnover = turnover[[2]],
                                   mean_turnover = turnover[[1]],
                                   relative_turnover = relative_turnover)
}

# 1/N benchmark w/ transactions costs ----
n <- dim(daily_returns)[2]
ew_weights <- array(1/n, dim = c(250, n))
ew_returns <- net_portfolio_returns(daily_returns, ew_weights, 0.0005)
ew_sr <- sharpe_ratio(ew_returns)
ew_capm_alpha <- lm(ew_returns ~ factors[,1])[["coefficients"]][["(Intercept)"]]
ew_ff_alpha <- lm(ew_returns ~ factors)[["coefficients"]][["(Intercept)"]]
ew_turnover <- average_turnover(ew_weights, daily_returns)

results_after_tc <- vector("list", length = length(predictions)+1)
names(results_after_tc) <- c("ew", names(predictions))
results_after_tc[[1]] <- list(sr = ew_sr,
                              capm_alpha = ew_capm_alpha,
                              ff_alpha = ew_ff_alpha,
                              turnover = ew_turnover[[2]],
                              mean_turnover = ew_turnover[[1]])

# strategy evaluation w/ transactions costs ----
for (i in 1:length(predictions)) {
  weights <- as.matrix(predictions[[i]])
  returns <- net_portfolio_returns(daily_returns, weights, 0.0005)
  
  sr <- sharpe_ratio(returns)
  sr_significance <- sr_test(returns, ew_returns)$p.value
  
  capm_lm <- lm(returns ~ factors[,1])
  capm_test <- linearHypothesis(capm_lm, hypothesis.matrix = c(1,0), rhs = ew_capm_alpha, white.adjust = "hc0")
  
  ff_lm <- lm(returns ~ factors)
  ff_test <- linearHypothesis(ff_lm, hypothesis.matrix = c(1,0,0,0), rhs = ew_ff_alpha, white.adjust = "hc0")
  
  turnover <- average_turnover(weights, daily_returns)
  relative_turnover <- turnover[[1]] / ew_turnover[[1]]
  
  results_after_tc[[i+1]] <- list(sr = sr,
                                  sr_p.value = sr_significance,
                                  capm_alpha = capm_lm[["coefficients"]][["(Intercept)"]],
                                  capm_p.value = capm_test[2, 4],
                                  ff_alpha = ff_lm[["coefficients"]][["(Intercept)"]],
                                  ff_p.value = ff_test[2, 4],
                                  turnover = turnover[[2]],
                                  mean_turnover = turnover[[1]],
                                  relative_turnover = relative_turnover)
}

# print results ----
print("Results BEFORE transaction costs:")
results_before_tc

print("Results AFTER transaction costs:")
results_after_tc