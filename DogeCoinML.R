library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(caret)
library(riingo)
library(usethis)
library(CandleStickPattern)

readRenviron(".Renviron")

createModel <- function(TargetIncreasePercent, SuccessThreshold, Symbol, Timeframe, TP=0){
# 
# # Riingo get data
# df = riingo_crypto_prices(Symbol, end_date = Sys.Date(), resample_frequency = Timeframe)
# # Testing quantmod
# # df = data.frame(getSymbols(Symbol,
# #            from = '2010-01-01',
# #            to = Sys.Date(),
# #            warnings = FALSE,
# #            auto.assign = FALSE))
# # 
# # df = na.omit(df)
# 
# # Modify for Riingo
# df$Percent.Change = NA
# df = df[-1,-c(1:3,10:11)]
# colnames(df) = c("Date","Open","High","Low","Close","Volume","Percent.Change")
# df$Percent.Change = round((((df$Close / df$Open) * 100) - 100), digits = 1)
# 
# 
# # Modify data to be more useable
# # df$Date = row.names(df)
# # df$Percent = NA
# # df = df[,-c(5:6)]
# # df = df[,c(5:4,1:3,6)]
# # colnames(df) = c("Date","Close","Open","High","Low","Percent.Change")
# # df$Percent.Change = round((((df$Close / df$Open) * 100) - 100), digits = 0)
# 
# 
# 
# 
# # Add column for binary previouos day change
# df$Previous = NA
# for(i in 2:nrow(df)){
#   if(df$Percent.Change[i - 1] <= 0){
#     df$Previous[i] = 0
#   }else{
#     df$Previous[i] = 1
#   }
# }
# 
# # Remove first row since we can't use it
# df = df[-1,]
# 
# 
# # Round columns to be more general
# df$Close = round(df$Close, digits = 3)
# df$Open = round(df$Open, digits = 3)
# df$High = round(df$High, digits = 3)
# df$Low = round(df$Low, digits = 3)
# df$Volume = round(df$Volume, digits=0)
# 
# 
# # Convert to actual dates and remove year and change to numeric
# df$Date = as.Date(df$Date)
# df$Date = str_match(string = df$Date, pattern = "-(.*)")[,2]
# df$Date = str_replace(df$Date, pattern = "-.*", replacement = "")
# df$Date = as.numeric(df$Date)
# 
# # outcomes
# outcome = rep(NA, nrow(df))
# for(i in 1:length(outcome)-1){
#   if(df$Percent.Change[i + 1] >= TargetIncreasePercent){
#     outcome[i] = 1
#   }else{
#     outcome[i] = 0
#   }
# }
# 
# 
# # Remove last row from df since we can't use it
# outcome = outcome[-(length(outcome))]
# df = df[-(nrow(df)),]
# 
# 
# # Remove Previous column for testing
# # df = df[,-ncol(df)]
# 
# # One hot encode everything
# 
# # df[,c(1:7)] = lapply(df[,c(1:7)], factor)
# 
# # remove some variables
# # df = df[,-c(2,4,5)]
# # 
# # dmy <- dummyVars(" ~ .", data = df)
# # trsf <- data.frame(predict(dmy, newdata = df))
# 
# Split data into train and test
# set.seed(123)
# sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))
# 
# # Remvoe last sample int since I said so
# #sample.split = sample.split[-which(sample.split == nrow(df))]
# 
# train = df[sample.split,]
# test = df[!sample.split,]
# 
# train = as.matrix(train)
# test = as.matrix(test)
# Symbol = 'ETHUSD'
# Timeframe = '1D'
# TargetIncreasePercent = "4"
# df = readRDS(paste0("bsts/df_",'ETHUSD','4hour',".rds"))
# sample.split = readRDS(paste0("bsts/sample.split_",'ETHUSD','4hour',"1",".rds"))
# outcome = readRDS(paste0("bsts/outcome_",'ETHUSD','4hour',1,".rds"))
# test = readRDS(paste0("bsts/test_",'ETHUSD','4hour',1,".rds"))
# train = readRDS(paste0("bsts/train_",'ETHUSD','4hour',1,".rds"))

  
df = readRDS(paste0("bsts/df_",Symbol,Timeframe,".rds"))
sample.split = readRDS(paste0("bsts/sample.split_",Symbol,Timeframe,TargetIncreasePercent,".rds"))
outcome = readRDS(paste0("bsts/outcome_",Symbol,Timeframe,TargetIncreasePercent,".rds"))
test = readRDS(paste0("bsts/test_",Symbol,Timeframe,TargetIncreasePercent,".rds"))
train = readRDS(paste0("bsts/train_",Symbol,Timeframe,TargetIncreasePercent,".rds"))

outcome.train = outcome[sample.split]
outcome.test = outcome[!sample.split]


assign('train',train,.GlobalEnv)

# Created boosted model
# bst = xgboost(data = train,
#               label = outcome.train,
#               objective = "binary:logistic",
#               max.depth = 10,
#               nrounds = 50)
bst = readRDS(paste0("bsts/bst_",Symbol,Timeframe,TargetIncreasePercent,".rds"))

# bst = readRDS(paste0("bsts/bst_",'ETHUSD','4hour',1,".rds"))

# Predict
predictions = predict(bst, test)
Actual.Percent.High = round((((df$High / df$Open) * 100) - 100), digits = 1)
Actual.Percent.Close = round((((df$Close / df$Open) * 100) - 100), digits = 1)
Actual.Percent.Low = round((((df$Low / df$Open) * 100) - 100), digits = 1)
compare = data.frame("Actual" = outcome.test,
                     "Actual.Percent.High" = Actual.Percent.High[which(!sample.split) + 1],
                     "Actual.Percent.Low" = Actual.Percent.Low[which(!sample.split) + 1],
                     "Actual.Percent.Close" = Actual.Percent.Close[which(!sample.split) + 1],
                     "Probability" = round(predictions, digits = 4),
                     "Prediction" = NA)
compare$Prediction[compare$Probability >= SuccessThreshold] = 1
compare$Prediction[compare$Probability < SuccessThreshold] = 0
compare = na.omit(compare)


accuracy = length(which(compare$Actual == compare$Prediction)) / nrow(compare) * 100
print(accuracy)


if(TP == 0){
  examine = compare[compare$Prediction == 1, ]
  accuracy2 = sum(as.numeric(as.character(examine$Actual.Percent.Close)))
  print(accuracy2)
}else{

  examine = compare[compare$Prediction == 1, ]
  winning.trades = examine[examine$Actual == 1,]
  winning.trades$Actual.Percent.High[winning.trades$Actual.Percent.High > TP ] = TP
  winning.trades.above = winning.trades[winning.trades$Actual.Percent.High == TP,]
  winning.trades.below = winning.trades[winning.trades$Actual.Percent.High < TP,]
  winning.sum.below = sum(as.numeric(as.character(winning.trades.below$Actual.Percent.Close)))
  winning.sum.above = sum(as.numeric(as.character(winning.trades.above$Actual.Percent.High)))
  winning.sum = winning.sum.above + winning.sum.below
  # missed.trades = examine[examine$Actual == 0,]
  # missed.trades$Actual.Percent.Close[missed.trades$Actual.Percent.Close < SL] = SL
  # missed.sum = sum(as.numeric(as.character(missed.trades$Actual.Percent.Close)))
  accuracy2 = winning.sum
  # accuracy2 = sum(as.numeric(as.character(examine$Actual.Percent.Close)))
  print(accuracy2)
}


yes.buy = compare[compare$Prediction == 1, ]
yes.buy.above.zero = length(which(yes.buy$Actual == 0 & yes.buy$Actual.Percent.Close > 0))
yes.buy.correct.perc = (length(which(yes.buy$Prediction == yes.buy$Actual)) + yes.buy.above.zero)  / nrow(yes.buy) * 100

no.buy = compare[compare$Prediction == 0, ]
no.buy.correct.perc = length(which(no.buy$Prediction == no.buy$Actual)) / nrow(no.buy) * 100


assign('yes.buy.correct.perc',yes.buy.correct.perc,.GlobalEnv)
assign("no.buy.correct.perc",no.buy.correct.perc,.GlobalEnv)
assign("overall.accuracy",accuracy,.GlobalEnv)
assign("compare",compare,.GlobalEnv)
assign("sum.percentage",accuracy2,.GlobalEnv)
assign('bst',bst,.GlobalEnv)
}

# # Predict Best
# predict.best <- function(SuccessThreshold = 0.3, all.bst, all.bst.names){
#   all.predictions = c()
#   
#   for(j in 1:length(all.bst)){
#     
#     
#     bst = all.bst[[j]]
#     names = all.bst.names[j]
#     
#     current = getQuote("DOGE-USD")
#     current = current[,-c(3,8)]
#     current = current[,c(1,2,4:6,3)]
#     colnames(current) = c("Date","Close","Open","High","Low","Percent.Change")
#     #if(df2$DOGE.USD.Close[1] < df2$DOGE.USD.Close[2]){
#     #  current$Previous = 1
#     #}else if(df2$DOGE.USD.Close[1] > df2$DOGE.USD.Close[2]){
#     #  current$Previous = 0
#     #}
#     
#     # Format it the same as the model
#     current$Date = as.Date(current$Date)
#     current$Date = str_match(string = current$Date, pattern = "-(.*)")[,2]
#     current$Date = str_replace(current$Date, pattern = "-.*", replacement = "")
#     current$Date = as.numeric(current$Date)
#     
#     current = as.matrix(current)
#     
#     predict.now = predict(bst, current)
#     if(predict.now >= SuccessThreshold){
#       print("buy")
#     }else{
#       print("not good")
#     }
#     
#     all.predictions = c(all.predictions, predict.now)
#     
#     assign("all.predictions",all.predictions,.GlobalEnv)
#     assign(paste0("predict_",names) ,predict.now,.GlobalEnv)
#   }
# }

# predict.tomorrow <- function(SuccessThreshold, Symbol){
# 
#   current = getQuote(Symbol)
#   current = current[,-c(3,8)]
#   current = current[,c(1,2,4:6,3)]
#   colnames(current) = c("Date","Close","Open","High","Low","Percent.Change")
#   # if(df2$DOGE.USD.Close[1] < df2$DOGE.USD.Close[2]){
#   #  current$Previous = 1
#   # }else if(df2$DOGE.USD.Close[1] > df2$DOGE.USD.Close[2]){
#   #  current$Previous = 0
#   # }
#   
#   # Format it the same as the model
#   current$Date = as.Date(current$Date)
#   current$Date = str_match(string = current$Date, pattern = "-(.*)")[,2]
#   current$Date = str_replace(current$Date, pattern = "-.*", replacement = "")
#   current$Date = as.numeric(current$Date)
#   
#   current = as.matrix(current)
#   assign('current',current,.GlobalEnv)
#   
#   predict.now = predict(bst, current)
#   if(predict.now >= SuccessThreshold){
#     print("buy")
#   }else{
#     print("not good")
#   }
#   
#   assign(paste0("predict.now") ,predict.now,.GlobalEnv)
# }


#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################

predict.tomorrow.multiple <- function(Symbols, Timeframe, SuccessThreshold, .GlobalEnv){
  # Symbols = Symbols
  # Symbols = c('btcusd','ethusd')
  # Timeframe = '4hour'
  # i = 1
  predictions.df.comb = data.frame("Coin" = character(),
                              "Price Change" = character(),
                              "Probability" = character(),
                              "Confidence Score" = character())  
  
  for(i in 1:length(Symbols)){

    if(Timeframe == '4hour' | Timeframe == '8hour'){
      df1 = riingo_crypto_prices(Symbols[i], end_date = Sys.Date(), resample_frequency = Timeframe)
      df1 = df1[-nrow(df1),]
      df2 = riingo_crypto_latest(Symbols[i], resample_frequency = Timeframe)
      df = rbind(df1,df2)
    }else{
      df = riingo_crypto_prices(Symbols[i], end_date = Sys.Date(), resample_frequency = Timeframe)
    }
    # Modify data to be more useable
    df = df[,4:8]
    df$Percent.Change = NA

    colnames(df) = c("Date","Open","High","Low","Close","Percent.Change")
    df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)
    
    #Add column for binary previouos day change+
    df$Previous = NA
    for(k in 2:nrow(df)){
      if(df$Percent.Change[k - 1] <= 0){
        df$Previous[k] = 0
      }else{
        df$Previous[k] = 1
      }
    }
    
    # Remove first row since we can't use it
    df = df[-1,]
    
    
    # Adding Moving Averages
    df$MA10 = NA
    df$MA20 = NA
    
    for(k in 21:nrow(df)){
      df$MA10[k] = mean(df$Close[k-10:k])
      df$MA20[k] = mean(df$Close[k-20:k])
    }
    df$MA10 = round(df$MA10, digits = 2)
    df$MA20 = round(df$MA20, digits = 2)
    

    
    # Add column for if MA10 is above or below MA20
    df$MAAB = 0
    
    df$MAAB[df$MA10 > df$MA20] = 1
    
    
    # Convert to actual dates and remove year and change to numeric
    # df$Date = str_replace(string = df$Date, pattern = "T", replacement = " ")
    # df$Date = str_replace(string = df$Date, pattern = "Z", replacement = "")
    
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
    
    df = as.xts(df)
    
    
    
    # Add candelstick patterns
    # candle.list = list(CSPDarkCloudCover(df),CSPDoji(df),CSPEngulfing(df),CSPGap(df),CSPHammer(df),CSPHarami(df),
    #                    CSPInsideDay(df),CSPInvertedHammer(df),CSPKicking(df),CSPLongCandle(df),CSPMarubozu(df),
    #                    CSPNLongWhiteCandles(df),CSPPiercingPattern(df),CSPStar(df),
    #                    CSPStomach(df),CSPTasukiGap(df),CSPThreeBlackCrows(df),CSPThreeInside(df),CSPThreeLineStrike(df),
    #                    CSPThreeMethods(df),CSPThreeOutside(df),CSPThreeWhiteSoldiers(df))
    candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
    # trend = candlesticks::TrendDetectionSMA(df)
    
    # Remove unusable rows
    
    
    for(k in 1:length(candle.list)){
      df = cbind(df, candle.list[[k]])
    }
    # df = cbind(df, trend$Trend)
    df = df[-(1:20),]
    
    
    # Add lagged values
    for(k in 1:5){
      high.lag = lag(df$High, k)
      close.lag = lag(df$Close, k)
      # lagging = LagOHLC(df, k)
      # ind = which(names(lagging) == paste0("High.Lag.",k))
      # ind = c(ind,which(names(lagging) == paste0("Close.Lag.",k)))
      df = cbind(df, high.lag, close.lag)
      
    }
    
    df = df[-c(1:5),]
    
    df[is.na(df)] = 0
    # Round columns to be more general
    df$Close = round(df$Close, digits = 3)
    df$Open = round(df$Open, digits = 3)
    df$High = round(df$High, digits = 3)
    df$Low = round(df$Low, digits = 3)
    
    df = df[nrow(df)-1,]
    
    
    
    predictions.df = data.frame("Coin" = toupper(Symbols[i]),
                                "Price Change" = 1:5,
                                "Probability" = rep(0,5),
                                "Confidence Score" = NA)
    
    predictions = c()
    for(j in 1:5){
      # bst = readRDS(paste0('bsts/bst_',toupper(Symbols[i]),Timeframe,j,'.rds'))
      
      bst = readRDS(paste0('bsts/bst_',toupper(Symbols[i]),Timeframe,j,'.rds'))
      df = as.matrix(df)
      predict.next = predict(bst, df)
      predictions = c(predictions,predict.next)
    }
    predictions.df$Price.Change = paste0(predictions.df$Price.Change,"%")
    predictions.df$Confidence.Score = predictions
    predictions.df$Probability[predictions.df$Confidence.Score >= SuccessThreshold] = 1

    predictions.df.comb = rbind(predictions.df.comb,predictions.df)

  }
  predictions.df.comb$Confidence.Score = round(predictions.df.comb$Confidence.Score, digits = 4)
  assign("predictions.df.comb",predictions.df.comb,.GlobalEnv)
  
  
  

  
}

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################





predict_week = function(symbol, timeframe){
  # symbol = 'ETH-USD'
  # timeframe = 'daily'
  data = data.frame(getSymbols(symbol, auto.assign = FALSE, periodicity = timeframe))
  data = data[-nrow(data),1:4]
  data = round(data, digits = 0)
  
  colnames(data) = c('open','high','low','close')
  
  data$time = row.names(data)
  
  str(data)
  
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
  for(k in 7:21){
    lagging = lag(data$close, k)
    # lagging = LagOHLC(data.xts, 7)
    # ind = which(names(lagging) == paste0("close.Lag.",7))
    data = cbind(data, lagging)
    
  }
  
  data= data[-c(1:21),]
  
  
  data$month = lubridate::month(data$time)
  data$day = lubridate::day(data$time)
  

  
  data_selected = data[,-c(1:5)]

  
  # SPLIT INTO TRAIN AND TEST
  train <- data_selected[1:(nrow(data)-7), ]
  
  pred <- data_selected[((nrow(data) - 7 + 1)):nrow(data), ]

  
  
  x_train = as.matrix(train)
  x_pred = as.matrix(pred)
  
  y_train <- data[1:(nrow(data)-7), 4]
  
  # symbol = 'BTC-USD'
  # timeframe = 'daily'
  bst = readRDS(paste0('bsts_T/bst_T_',symbol,timeframe,'.rds'))
  
  
  # xgb_model$bestTune
  
  
  xgb_pred <- predict(bst, x_pred)
  # saveRDS(bst, file = paste0("bsts_T/bst_",lfiles.names[i],".rds"))
  
  data_y = data[((nrow(data) - 30 + 1)):(nrow(data) - 7), 4]
  add.na = rep(NA, 7)
  
  predicted_y = rep(NA, 23)
  predicted_y[23] = data_y[23]
  
  predicted_y = c(predicted_y, xgb_pred)
  data_y = c(data_y, add.na)
  times = data$time[(nrow(data)-29):nrow(data)]
  x = data.frame(cbind(data_y, predicted_y))
  x = round(x, digits = 0)
  x = cbind(x, times)
  x$times = as.Date(x$times)
  
  plot.out = ggplot(data = x, aes(x = times)) + 
    geom_line(aes(y = data_y), color = "blue") +
    geom_line(aes(y = predicted_y), color = "red") +
    xlab("Date") +
    ylab("Price") +
    ggtitle(paste0("Predicted Stock Price for ",symbol))
  
  return(plot.out)
}
