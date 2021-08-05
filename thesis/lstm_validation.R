# ###
# This script is used to test/validate different models and hyperparameters
# with the "mean-variance rolling window" strategy
# ###


# clean environment ----
rm(list = ls())

# import libraries and functions ----
library(keras)
library(xts)
library(ggplot2)
source("custom_functions.R")

# import data ----
data <- readRDS("temp/dataset.rds")
features <- data[["features"]]
targets <- data[["targets_mv.r"]]

# normalize data ----
train_data <- features[1:1988,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

x_all <- scale(features, center = mean, scale = std)

# make batch of sequences for training, validation and testing ----
y_all <- targets

lookback <- 250
delay <- 0

train_data <- seq_generator(
  x = x_all, 
  y = y_all, 
  lookback = lookback, 
  delay = delay,
  shuffle = TRUE,
  min_index = 1, 
  max_index = 1988)
x_train <- train_data[[1]]
y_train <- train_data[[2]]

val_data <- seq_generator(
  x = x_all, 
  y = y_all, 
  lookback = lookback, 
  delay = delay,
  min_index = 1989, 
  max_index = 2738)
x_val <- val_data[[1]]
y_val <- val_data[[2]]

test_data <- seq_generator(
  x = x_all, 
  y = y_all, 
  lookback = lookback, 
  delay = delay,
  min_index = 2739, 
  max_index = 3238)
x_test <- test_data[[1]]
y_test <- test_data[[2]]

# model architecture ----
model <- keras_model_sequential() %>%
  layer_lstm(units = 7, input_shape = dim(x_train[1,,]), dropout = 0.1, kernel_regularizer = regularizer_l1(0.05)) %>%
  layer_dense(units = dim(y_all)[2], activation = "softmax")
  
model %>% compile(
  optimizer = "rmsprop",
  loss = "kullback_leibler_divergence"
)

# train model ----
history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

# evaluate model ----
model %>% evaluate(x_test, y_test)