library(quantmod)
library(tensorflow)
# Core Tidyverse #
library(tidyverse)
library(glue)
library(forcats)
# Time Series #
library(timetk)
library(tidyquant)
library(tibbletime)
# Visualization #
library(cowplot)
# Preprocessing #
library(recipes)
# Sampling / Accuracy #
library(rsample)
library(yardstick) 
# Modeling #
library(keras)
library(ggplot2)


# Read Data #
BTC <- (getSymbols("BTC-USD", env=NULL))
BTC_DF <- as.data.frame(BTC) #Convert data to dataframe
head(BTC_DF)

# Add the date to the data frame as a 7th column #
BTC_DF$Date <- BTC_DF$time
# Remove unnecessary columns and rename columns #
BTC_Data <- BTC_DF[, c(3,1)]
colnames(BTC_Data) <- c('High', 'Date')
# Remove Null rows #
Bit_Coin <- na.exclude(BTC_Data)

# Convert data frame to a tibble #
Bit_Coin %>% tk_tbl(Bit_Coin)
# Visualize #
ggplot(data=Bit_Coin, aes(x=Date, y=High, group=1)) +
  geom_line()+
  geom_point()

# Craete ACF #
tidy_acf <- function(data, High, lags = 0:20) {
  
  value_expr <- enquo(High)
  
  acf_values <- data %>%
    pull(High) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  na.action = na.omit
  return(ret)
}
max_lag <- 12 * 50
# Find Bit Coin's ACF Values #
Bit_Coin %>%
  tidy_acf(High, lags = 0:max_lag)
# Plot the ACF data #
Bit_Coin %>%
  tidy_acf(High, lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 365, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "1 Year Mark", x = 250, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Bit Coin")

# Train and Test Data #
periods_train <- 3000
periods_test  <- 120
rolling_origin_resamples <- rolling_origin(
  Bit_Coin,
  initial = periods_train,
  assess  = periods_test,
  cumulative = FALSE,
  skip = 0
)
rolling_origin_resamples
# Create a function that plots the Split #
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data #
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = Date) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes #
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize #
  g <- data_manipulated %>%
    ggplot(aes(x = Date, y = High, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Training vs Testing"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    Bit_Coin_time_summary <- Bit_Coin %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(Bit_Coin_time_summary$start, 
                              Bit_Coin_time_summary$end))
  }
  
  return(g)
}
# Plot #
rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")


split    <- rolling_origin_resamples$splits[[1]]
split_id <- rolling_origin_resamples$id[[1]]
df_trn <- training(split)
df_tst <- testing(split)
df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_tst %>% add_column(key = "testing")
) %>% 
  as_tbl_time(index = Date)
df




# Preprocess using recipe #
rec_obj <- recipe(High ~ ., df) %>%
  step_sqrt(High) %>%
  step_center(High) %>%
  step_scale(High) %>%
  prep()
df_processed_tbl <- bake(rec_obj, df)
df_processed_tbl


# Capture the Center and Scale history #
center_history <- rec_obj$steps[[2]]$means["High"]
scale_history  <- rec_obj$steps[[3]]$sds["High"]
c("center" = center_history, "scale" = scale_history)




# Model inputs #
lag_setting  <- 120
batch_size   <- 30
train_length <- 3000
tsteps       <- 1
epochs       <- 500


# Training Set #
lag_train_tbl <- df_processed_tbl %>%
  mutate(High_lag = lag(High, n = lag_setting)) %>%
  filter(!is.na(High_lag)) %>%
  filter(key == "training") %>%
  tail(train_length)
x_train_vec <- lag_train_tbl$High_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
y_train_vec <- lag_train_tbl$High
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
head(lag_train_tbl)
tail(lag_train_tbl)




# Testing Set #
lag_test_tbl <- df_processed_tbl %>%
  mutate(
    High_lag = lag(High, n = lag_setting)
  ) %>%
  filter(!is.na(High_lag)) %>%
  filter(key == "testing")
x_test_vec <- lag_test_tbl$High_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
y_test_vec <- lag_test_tbl$High
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
head(lag_test_tbl)
tail(lag_test_tbl)




# Build the LSTM Model #
model <- keras_model_sequential()
model %>%
  layer_lstm(units            = 24, 
             input_shape      = c(tsteps, 1), 
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 24, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 1)
model %>% 
  compile(loss = 'mae', optimizer = 'adam')
model
for (i in 1:epochs) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}
# Make Predictions #
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1]
# Retransform values #
pred_tbl <- tibble(
  Date   = lag_test_tbl$Date,
  High   = (pred_out * scale_history + center_history)^2
)
# Combine actual data with predictions #
tbl_1 <- df_trn %>%
  add_column(key = "actual")
tbl_2 <- df_tst %>%
  add_column(key = "actual")
tbl_3 <- pred_tbl %>%
  add_column(key = "predict")
# Create time_bind_rows() to solve dplyr issue #
time_bind_rows <- function(data_1, data_2, Date) {
  index_expr <- enquo(Date)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
}
ret <- list(tbl_1, tbl_2, tbl_3) %>%
  reduce(time_bind_rows, Date = Date) %>%
  arrange(key, Date) %>%
  mutate(key = as_factor(key))
ret
# Determining Model Performance #
MSE<-mean((tbl_2$High - tbl_3$High)^2)
RMSE<-sqrt(MSE)
# Setup single plot function #
plot_prediction <- function(data, id, alpha = 1, size = 2, base_size = 14) {
  
  rmse_val <- RMSE
  
  g <- data %>%
    ggplot(aes(Date, High, color = key)) +
    geom_point(alpha = alpha, size = size) + 
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      title = glue("{id}, RMSE: {round(rmse_val, digits = 1)}"),
      x = "", y = ""
    )
  
  return(g)
}
ret[ret$Date > as.Date('2022-11-03'),] %>% 
  plot_prediction(id = split_id, alpha = 0.65) +
  theme(legend.position = "bottom")
