# ###
# This main script runs all relevant scripts in the correct order
#
# 1_data_generation.R: collects data from different sources, cleans it and merges it
#     to one single dataset. The prepared data gets saved to a temporary dataset.rds.
# 2_lstm_predictions.R: loads dataset.rds, normalize/standardize data, makes sequences
#     for input layer. Predictions are saved to temp folder.
# 3_performance_evaluation.R: loads temporary dataset and predictions. Does 
#     the performance evaluation and prints out final results for all strategies.
# ###

source("1_data_generation.R")
source("2_lstm_predictions.R")
source("3_performance_evaluation.R")