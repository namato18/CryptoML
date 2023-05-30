library(xgboost)
library(caret)
library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(CandleStickPattern)
# predict_week = function(symbol){
# symbol = 'BTC-USD'
# timeframe = 'daily'
data = data.frame(getSymbols(symbol, auto.assign = FALSE, periodicity = timeframe))
data = data[-nrow(data),1:4]
data = round(data, digits = 0)

colnames(data) = c('open','high','low','close')

data$time = row.names(data)

# lfiles.names = list.files("TVData_T")
# lfiles.names = str_replace_all(string = lfiles.names, pattern = '.csv', replacement = "")
# lfiles = list.files("TVData_T", full.names = TRUE)

# all.files = lapply(lfiles, read.csv)
# for(i in 1:length(lfiles)){
  
# data = all.files[[i]]

# CLEAN DATA
# data <- economics %>% dplyr::select(date, unemploy)
# data <- read.csv('TVData/SOLUSDT1day.csv')
# data <- read.csv('TVData/ETHUSDT4hour.csv')

str(data)



# data$time = str_replace(string = data$time, pattern = "T", replacement = " ")
# data$time = str_replace(string = data$time, pattern = "Z", replacement = "")
if(timeframe == 'daily'){
  data.add = data.frame(time = seq(from = as_date(Sys.Date()) + 1,
                                   by = "day", length.out = 7),
                        open = NA,
                        high = NA,
                        low = NA,
                        close = NA)
}else{
  data.add = data.frame(time = seq(from = as_date(Sys.Date()) + 1,
                                   by = "week", length.out = 7),
                        open = NA,
                        high = NA,
                        low = NA,
                        close = NA)
}

data.add$time = as.character(data.add$time)
data = rbind(data, data.add)

data.xts = data

data.xts$time = as.POSIXct(data.xts$time, format = "%Y-%m-%d")


data.xts = as.xts(data.xts)

# Add lagged values
# Add lagged values
for(k in 7:21){
  lagging = lag(data$close, k)
  # lagging = LagOHLC(data.xts, 7)
  # ind = which(names(lagging) == paste0("close.Lag.",7))
  data = cbind(data, lagging)
  
}

# data = cbind(data, lagged)
data= data[-c(1:21),]


data$month = lubridate::month(data$time)
data$day = lubridate::day(data$time)
# data$hour = lubridate::hour(data$time)

# extended_data <- data %>% 
#   rbind(tibble::tibble(date = seq(from = lubridate::as_date("2015-05-01"),
#                                   by = "month", length.out = 12), 
#                        unemploy = rep(NA, 12)))

# extended_data = data
# extended_data$unemploy[as.Date(data$date) > as.Date('2014-04-01')] = NA




# extended_data_mod <- extended_data %>%
#   dplyr::mutate(., 
#                 months = lubridate::month(date),
#                 years = lubridate::year(date))

data_selected = data[,-c(1:5)]
# data_selected = data

# SPLIT INTO TRAIN AND TEST
train <- data_selected[1:(nrow(data)-7), ] # initial data

pred <- data_selected[((nrow(data) - 7 + 1)):nrow(data), ] # extended time index
# row.names(pred) = NULL



x_train = as.matrix(train)
# x_train <- xgboost::xgb.DMatrix(as.matrix(train))
# x_pred <- xgboost::xgb.DMatrix(as.matrix(pred))
x_pred = as.matrix(pred)

y_train <- data[1:(nrow(data)-7), 4]


# CREATE MODEL
# xgb_trcontrol <- caret::trainControl(
#   method = "cv", 
#   number = 5,
#   allowParallel = TRUE, 
#   verboseIter = FALSE, 
#   returnData = FALSE
# )

# xgb_grid <- base::expand.grid(
#   list(
#     nrounds = c(400),
#     max_depth = c(25), # maximum depth of a tree
#     colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
#     eta = 0.1, # learning rate
#     gamma = 0, # minimum loss reduction
#     min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
#     subsample = 1 # subsample ratio of the training instances
#   ))
# 
# 
# # TEST MULTIPLE MODELS
# xgb_model <- caret::train(
#   x_train, y_train,
#   trControl = xgb_trcontrol,
#   tuneGrid = xgb_grid,
#   method = "xgbTree",
#   nthread = 1
# )


# Created boosted model
bst = xgboost(data = x_train,
              label = y_train,
              objective = "reg:linear",
              max.depth = 20,
              nrounds = 200)

# bst = readRDS(paste0('bsts_T/bst_T_',symbol,'.rds'))

saveRDS(bst, paste0('bsts_T/bst_T_',symbol,timeframe,'.rds'))
# 
# # xgb_model$bestTune
# 
# 
# xgb_pred <- predict(bst, x_pred)
# # saveRDS(bst, file = paste0("bsts_T/bst_",lfiles.names[i],".rds"))
# 
# data_y = data[((nrow(data) - 100 + 1)):(nrow(data) - 7), 4]
# add.na = rep(NA, 7)
# 
# predicted_y = rep(NA, 93)
# predicted_y[93] = data_y[93]
# 
# predicted_y = c(predicted_y, xgb_pred)
# data_y = c(data_y, add.na)
# times = data$time[(nrow(data)-99):nrow(data)]
# x = data.frame(cbind(data_y, predicted_y))
# x = round(x, digits = 0)
# x = cbind(x, times)
# x$times = as.Date(x$times)
# 
# plot.out = ggplot(data = x, aes(x = times)) + 
#   geom_line(aes(y = data_y), color = "blue") +
#   geom_line(aes(y = predicted_y), color = "red") +
#   xlab("Date") +
#   ylab("Price") +
#   ggtitle(paste0("Predicted Stock Price for ",symbol))
# 
# return(plot.out)
# }
# plot(data[((nrow(data) - 100 + 1)):(nrow(data) - 7), 5], type = 'l', xlim = c(0,110))
# lines(x = 94:100, y = xgb_pred, col = 'red')

# 
# # CREATING FORECAST OBJECTS
# fitted <- xgb_model %>%
#   stats::predict(x_train) %>%
#   stats::ts(start = zoo::as.yearmon(min(train$date)), 
#             end = zoo::as.yearmon(max(train$date)),
#             frequency = 12)
# 
# xgb_forecast <- xgb_pred %>%
#   stats::ts(start = zoo::as.yearmon(min(pred$date)),
#             end = zoo::as.yearmon(max(pred$date)),
#             frequency = 12)
# 
# 
# # original data as ts object
# ts <- y_train %>% 
#   stats::ts(start = zoo::as.yearmon(min(train$date)), 
#             end = zoo::as.yearmon(max(train$date)), 
#             frequency = 12)
# 
# # forecast object
# forecast_list <- list(
#   model = xgb_model$modelInfo,
#   method = xgb_model$method,
#   mean = xgb_forecast,
#   x = ts, 
#   fitted = fitted,
#   residuals = as.numeric(ts) - as.numeric(fitted)
# )
# class(forecast_list) <- "forecast"
# forecast::autoplot(forecast_list)
