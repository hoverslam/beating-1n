# clean environment ----
rm(list = ls())

# import libraries and functions ----
library(keras)
library(xts)
source("custom_functions.R")

# import data ----
data <- readRDS("temp/dataset.rds")
features <- data[["features"]]
targets <- data[2:5]

# create a list to store the predictions for every strategy ----
n_strategies <- length(targets)
predictions <- vector("list", length = n_strategies)
names(predictions) <- c("mv.r", "mv.e", "min.r", "min.e")

# normalize data ----
train_data <- features[1:1988,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

x_all <- scale(features, center = mean, scale = std)

for (i in 1:n_strategies) {
  cat("Processing strategy: ", i, "/", n_strategies, "\n")
  
  # make batch of sequences for training, validation and testing ----
  y_all <- targets[[i]]
  
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
    #layer_lstm(units = 7, input_shape = dim(x_train[1,,]), dropout = 0.1, kernel_regularizer = regularizer_l1(0.05)) %>%
    layer_lstm(units = 7, input_shape = dim(x_train[1,,]), dropout = 0.1) %>%
    layer_dense(units = dim(y_all)[2], activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "kullback_leibler_divergence"
  )
  
  # train model ----
  history <- model %>% fit(
    x_train, y_train,
    epochs = 8,
    batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  # make predictions and assign the correct time index ----
  cur_predictions <- model %>% predict(x_test)
  colnames(cur_predictions) <- colnames(targets)

  predicitons_dates <- head(tail(index(targets[[1]]), n = 251), n = -1)
  cur_predictions <- xts(cur_predictions, order.by = predicitons_dates)

  predictions[[i]] <- cur_predictions
}

# save model and predictions ----
saveRDS(predictions, file = "temp/predictions.rds")