library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)

######### EXTRA CODE TO CREATE BST MODELS
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
x = list.files(path = 'TVData',full.names = TRUE)
file.names = list.files('TVData')
file.names = str_replace(string = file.names, pattern = '\\.csv', replacement = "")
ls.files = lapply(x, read.csv)

for(i in 1:length(ls.files)){
  for(j in 1:25){
    df = ls.files[[i]]
    # Testing quantmod
    # df = data.frame(getSymbols("DOGE-USD",
    #                            from = '2017-01-01',
    #                            to = Sys.Date(),
    #                            warnings = FALSE,
    #                            auto.assign = FALSE))
    # 
    # df = na.omit(df)
    
    # Remove uncecessary columns
    df = df[,1:5]
    
    # Modify data to be more useable
    df$Percent.Change = NA
    #df = df[-1,-c(1:3,10:11)]
    colnames(df) = c("Date","Open","High","Low","Close","Percent.Change")
    df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)
    
    # Adding Moving Averages
    df$MA10 = NA
    df$MA20 = NA
    
    for(k in 21:nrow(df)){
      df$MA10[k] = mean(df$Close[k-10:k])
      df$MA20[k] = mean(df$Close[k-20:k])
    }
    df$MA10 = round(df$MA10, digits = 2)
    df$MA20 = round(df$MA20, digits = 2)
    
    # Remove unusable rows
    df = df[-(1:20),]
    
    # Add column for if MA10 is above or below MA20
    df$MAAB = 0
    
    df$MAAB[df$MA10 > df$MA20] = 1
    
    
    # Add column for binary previouos day change
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
    
    
    # Round columns to be more general
    df$Close = round(df$Close, digits = 3)
    df$Open = round(df$Open, digits = 3)
    df$High = round(df$High, digits = 3)
    df$Low = round(df$Low, digits = 3)
    
    
    # Convert to actual dates and remove year and change to numeric
    df$Date = as.Date(df$Date)
    df$Date = str_match(string = df$Date, pattern = "-(.*)")[,2]
    df$Date = str_replace(df$Date, pattern = "-.*", replacement = "")
    df$Date = as.numeric(df$Date)
    
    saveRDS(df, file = paste0("bsts/df_",file.names[i],".rds"))
    
    # outcomes
    
    outcome = rep(NA, nrow(df))
    for(z in 1:length(outcome)-1){
      if(df$Percent.Change[z + 1] >= j){
        outcome[z] = 1
      }else{
        outcome[z] = 0
      }
    }
    
    
    
    
    # Remove last row from df since we can't use it
    outcome = outcome[-(length(outcome))]
    df = df[-(nrow(df)),]
    
    saveRDS(outcome, file = paste0("bsts/outcome_",file.names[i],j,".rds"))
    
    
    # Remove Previous column for testing
    # df = df[,-ncol(df)]
    
    
    # Split data into train and test
    set.seed(123)
    sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))
    
    saveRDS(sample.split, file = paste0("bsts/sample.split_",file.names[i],j,".rds"))
    
    
    # Remvoe last sample int since I said so
    #sample.split = sample.split[-which(sample.split == nrow(df))]
    
    train = df[sample.split,]
    test = df[!sample.split,]
    
    train = as.matrix(train)
    test = as.matrix(test)
    
    saveRDS(train, file = paste0("bsts/train_",file.names[i],j,".rds"))
    saveRDS(test, file = paste0("bsts/test_",file.names[i],j,".rds"))
    
    outcome.train = outcome[sample.split]
    outcome.test = outcome[!sample.split]
    
    
    
    
    # Created boosted model
    bst = xgboost(data = train,
                  label = outcome.train,
                  objective = "binary:logistic",
                  max.depth = 10,
                  nrounds = 50)
    
    saveRDS(bst, file = paste0("bsts/bst_",file.names[i],j,".rds"))
    print(file.names[i])
  }
}
