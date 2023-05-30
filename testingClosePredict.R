library(tidyverse)
library(patchwork)
library(zoo)
library(caret)
library(stringr)
library(dplyr)


remove.packages("purrr")
install.packages('purrr')

df = read.csv('TVData/BTCUSD1day.csv')
df$time = str_replace(string = df$time, pattern = "T", replacement = " ")
df$time = str_replace(string = df$time, pattern = "Z", replacement = "")

df$time = as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S")
df$date = df$time

closedf <- select(df, date, close)
cat("Shape of close dataframe:", dim(closedf))

closedf <- closedf %>% filter(date > as.Date('2020-09-13'))
close_stock <- closedf
cat("Total data for prediction: ", nrow(closedf))

closedf <-
  predict(select(closedf, close) %>% preProcess(method = "range"),
          select(closedf, close))
dim(closedf)

training_size <-round(nrow(closedf)*0.70)

train_data <- data.frame(close = closedf[1:training_size, ])
test_data <- data.frame(close = closedf[(training_size + 1):nrow(closedf), ])

cat("train_data: ", dim(train_data))

ggplot(NULL) +
  geom_line(
    aes(
      x = close_stock$date[1:training_size],
      y = train_data$close,
      col = "train"
    )
  ) +
  geom_line(
    aes(
      x = close_stock$date[(training_size + 1):nrow(closedf)],
      y = test_data$close,
      col = "test"
    )
  ) +
  scale_color_manual(
    breaks = c("train", "test"),
    values = c("train" = "black", "test" = "red")
  ) +
  labs(title = 'Train & Test data', x = 'Date', y = 'Weekly Sales', col = element_blank()) +
  theme_bw()

create_dataset <- function (dataset, time_step=1) {
  dataX <- list()
  dataY <- list()
  
  for (i in 1:(nrow(dataset)-time_step-1)) {
    a <- dataset[i:(i+time_step-1), 1]
    dataX[[i]] <- a
    dataY[[i]] <- dataset[i + time_step, 1]
  }
  
  dataX <- matrix(unlist(dataX), ncol = time_step, byrow = TRUE)
  colnames(dataX) <- paste0("V", 1:time_step)
  dataY <- unlist(dataY)
  
  return(list(dataX = dataX, dataY = dataY))
}


train_set <- create_dataset(train_data, time_step = 15)
X_train <- train_set[[1]]
Y_train <- train_set[[2]]
rm(train_set)

test_set <- create_dataset(test_data, time_step = 15)
X_test <- test_set[[1]]
Y_test <- test_set[[2]]
rm(test_set)

cat("X_train: ", dim(X_train))

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds=1000,
    # base_score=0.5,
    # colsample_bylevel=1,
    # colsample_bynode=1,
    colsample_bytree = 1, # subsample ratio of columns when construction each tree
    # # nrounds = 1000,
    max_depth = 6, # maximum depth of a tree
    eta = 0.3, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
    # tree_method='exact',
    # interaction_constraints='',
    # max_delta_step=0,
    # n_estimators=1000,
    # validate_parameters=1,
    # random_state=0,
    # n_jobs=4
  ))

xgb_model <- train(
  X_train, Y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

predicted <- predict(xgb_model, X_test)
cat("Mean Absolute Error - MAE : ", MAE(Y_test, predicted))
cat("Root Mean squared Error - RMSE : ", RMSE(Y_test, predicted))

train_predict <- predict(xgb_model, X_train)
test_predict <- predict(xgb_model, X_test)

descaler <- function(xv, mx, nx) {
  dvals <- xv * (mx - nx) + nx
  dvals <- round(dvals, digits = 5)
  return(dvals)
}

scaled_test_predicted_close = test_predict %>% descaler(max(close_stock$close), min(close_stock$close))
scaled_test_original_close = test_data$close[1:length(scaled_test_predicted_close)] %>% descaler(max(close_stock$close), min(close_stock$close))

examine = cbind(scaled_test_original_close, scaled_test_predicted_close)
examine = data.frame(examine)
examine$predicted.up = 0
examine$original.up = 0

for(i in 2:nrow(examine)){
  if(examine$scaled_test_predicted_close[i - 1] < examine$scaled_test_predicted_close[i]){
    examine$predicted.up[i] = 1
  }
  if(examine$scaled_test_original_close[i - 1] < examine$scaled_test_original_close[i]){
    examine$original.up[i] = 1
  }
}

examine.selected = examine[examine$predicted.up == 1,]

accuracy = length(which(examine.selected$predicted.up == examine.selected$original.up)) / nrow(examine.selected) * 100


ggplot(NULL) +
  geom_line(aes(
    x = close_stock$date,
    y = closedf$close %>% descaler(max(close_stock$close), min(close_stock$close)),
    col = 'Original close price'
  ),) +
  geom_line(
    aes(
      x = close_stock$date[17:training_size - 1],
      y = train_predict %>% descaler(max(close_stock$close), min(close_stock$close)),
      col = 'Train predicted close price'),
  ) +
  geom_line(
    aes(
      x = close_stock$date[(training_size + 16):(nrow(close_stock) - 1)],
      y = test_predict %>% descaler(max(close_stock$close), min(close_stock$close)),
      col = 'Test predicted close price'),
    col = "green"
  ) +
  scale_color_manual(
    breaks = c(
      'Original close price',
      'Train predicted close price',
      'Test predicted close price'
    ),
    values = c(
      'Original close price' = "blue",
      'Train predicted close price' = "red",
      'Test predicted close price' = "green"
    ),
  ) +
  theme_bw() +
  labs(
    x = 'Date',
    y = 'Close price',
    title = 'Comparision between original close price vs predicted close price',
    col = element_blank()
  )

