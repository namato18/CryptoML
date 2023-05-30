library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(stringr)
library(ggplot2)
library(DT)

# Define UI
ui <- dashboardPage(
  # skin = "purple",
  dashboardHeader(title = "Crypto ML"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Creating a Model", tabName = "create"),
      #menuItem("Predict Tomorrow", tabName = "predict"),
      menuItem("Predict Next Candle (Multiple)", tabName = 'predictMultiple'),
      menuItem("Predict Next 7 Days/Weeks", tabName = 'predictNextWeek')
      # menuItem("Most Likely Outcome", tabName = "likely")
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      tabItem(tabName = "create",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                
                strong(h2("Creating a Model:")),
                paste0("On this tab you can use the sliders to modify how the predictive model is created. First you need to select a timeframe ",
                       "and coin that you're interested in predicting. ","The first slider is used ",
                       "to select the percentage increase in the timeframe that you've selected. The second slider is used ",
                       "to select how confident you want the model to be in order to classify a 'hit'. The model will make a prediction on a scale from ",
                       "0-1, the closer to 1 the prediction is, the more confident the model is that your selected percentage increase will happen in your selected timeframe."),
                br(),
                br(),
                selectInput("timeframe","Pick a Timeframe", choices = list("4 Hour" = "4hour",
                                                                           "8 Hour" = "8hour",
                                                                           "1 Day" = "1day",
                                                                           "1 Week" = "7day",
                                                                           "1 Month" = '1month')),
                selectInput("select","Pick a crypto to predict", choices = list("Ethereum" = "ETHUSD",
                                                                               "BitCoin" = "BTCUSD"
                                                                               # "Ethereum Futures" = "ETHUSDT",
                                                                               # "BitCoin Futures" = 'BTCUSDT',
                                                                               # "Solana Futures" = 'SOLUSDT',
                                                                               # "Dogecoin Futures" = 'DOGEUSDT'
                                                                               )),
                column(width = 6,
                       box(title = "Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                         strong("Note if you're using a mobile device you can just tap the number on the slider instead of actually sliding it."),
                         br(),
                         br(),
                         sliderInput("slider1","Select Percentage Increase", min = 1, max = 5, step = 1, value = 1),
                         sliderInput("slider2", "Select Prediction 'hit' Threshold", min = 0.1, max = 1, step = 0.02, value = 0.9),
                         strong("Note:"),
                         paste0("Metrics by default are calculated based on the candles closing value. ",
                                "You can use the TP (take profit) input field to specify your TP. ",
                                "Setting a TP can limit your gains, but can also limit your losses! ",
                                "Leaving the TP value at 0 will set the metrics to be calculated based on candles closing value."),
                         numericInput('tp',"Set TP (must be positive)", value = 0, min = 0),
                         # numericInput("sl","Set SL (must be negative)", value = 0, max = 0),
                         actionButton('action1', label = "Generate"),
                         br(),
                         br(),
                       ),
                       box(title = "Metrics", width = NULL, status = "primary", solidHeader = TRUE,
                         # infoBoxOutput("OverallAccuracy", width = 6),
                         infoBoxOutput("Buy", width = 6),
                         infoBoxOutput("SumPercentage", width = 6),
                         # infoBoxOutput("DontBuy", width = 6),
                         infoBoxOutput("Predictions", width = 6),
                         infoBoxOutput("Hits", width = 6)
                         
                       ),
                       box(title = "Histogram", width = NULL, status = "primary", solidHeader = TRUE,
                         paste0("Ideally, we'd like there to be a near 0 probability or a near 1 probability for all predictions. ",
                                "Values that are more in the middle can give us an unclear prediction. Since it's rare that the stock ",
                                "market will give a clear indicator of an increase over the next 24hrs, it's usually useful to set the ",
                                "prediction 'hit' threshold somewhat low."),
                         plotOutput("modelPlot")
                       )
                ),
                column(width = 6,
                box(width = NULL, title = "Backtest", status = "primary", solidHeader = TRUE,
                  strong(h4("Variable Info:")),
                  strong('Actual:'),
                  paste0("If the next candle actually hit the target percentage increase, this will be a 1, otherwise 0"),
                  br(),
                  strong("Actual.Percent.High:"),
                  paste0("This was the next candles high"),
                  br(),
                  strong("Actual.Percent.Low:"),
                  paste0("This was the next candles low"),
                  br(),
                  strong("Actual.Percent.Close:"),
                  paste0("This was the next candles close"),
                  br(),
                  strong("Probability:"),
                  paste0("This is the probability the model predicted that the next candle would reach the target percentage increase"),
                  br(),
                  strong("Prediction:"),
                  paste0("If the 'Probability' is higher than the selected prediction hit threshold, this will be a 1, otherwise 0"),
                  br(),
                  br(),
                  dataTableOutput("table1")
                )
              )
              )
      ),
      # tabItem(tabName = "predict",
      #         fluidRow(
      #           strong(h3("About:")),
      #           strong("Note that you must create a model on the previous tab before predicting tomorrow!"),
      #           br(),
      #           paste0("This tab will simply generate a prediction using the model you created in the 'Creating a Model' Tab. ",
      #                  "Remember that the probability is on a scale of 0-1, where a larger value represents a higher probability of your ",
      #                  "model's prediction coming true."),
      #           br(),
      #           br(),
      #           box(title = "Predict Tomorrow", status = "primary", solidHeader = TRUE,
      #             textInput("open","Open"),
      #             textInput("close","Close"),
      #             textInput("low","Low"),
      #             textInput("high","High"),
      #             actionButton("action2","Predict"),
      #             br(),
      #             br(),
      #             infoBoxOutput("predict", width = NULL)
      #           )
      #         )
      #         ),
      tabItem(tabName = "predictMultiple",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h3("About:")),
                paste0("On this tab you can generate predictions for multiple coins! Simply use the check boxes to select which coins you'd like to predict.",
                       " If you'd like to export these results, simply press the 'csv' button on top of the table below."),
                br(),
                br(),
                box(title = "Predict Multiple", status = "primary", solidHeader = TRUE,
                    checkboxGroupInput('checkGroup', label = 'Select Coin(s)',
                                       choices = list("Bitcoin" = 'btcusd', "Ethereum" = 'ethusd'),
                                                      # "Ethereum Futures" = 'ethusdt',
                                                      # "Bitcoin Futures" = 'btcusdt', "Solana Futures" = 'solusdt', "Dogecoin Futures" = 'dogeusdt'),
                                       selected = 'btcusd'),
                    selectInput("timeframePredict","Pick a Timeframe", choices = list("4 Hour" = "4hour",
                                                                                      "8 Hour" = "8hour",
                                                                                      "1 Day" = "1day",
                                                                                      "1 Week" = "7day")),
                    sliderInput("slider3", "Select Prediction 'hit' Threshold", min = 0.1, max = 1, step = 0.05, value = 0.9),
                    actionButton("action4","Predict"),
                    br(),
                    br()
                    

                ),
                dataTableOutput("multipleOutput")
              )
      ),
      
      tabItem(tabName = "predictNextWeek",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h3("About:")),
                paste0("On this tab you may pick a crypto to forecast for the next 7 days/weeks! The machine learning model utilizes the past 14 candles of data ",
                       "to predict the next 7 candles price movements!"),
                br(),
                br(),
                strong('Note: Previous data is displayed in BLUE while forecasted data is displayed in RED'),
                br(),
                br(),
                box(title = "Predict Next 7 Days/Weeks", status = "primary", solidHeader = TRUE,
                    selectInput('selectTimeFrame', 'Select a time frame', choices = list('7 Days' = 'daily',
                                                                                         '7 Weeks' = 'weekly')),
                    selectInput('selectNextWeek', "Select a Coin", choices = list("Ethereum" = 'ETH-USD',
                                                                                  "Bitcoin" = 'BTC-USD')),
                    actionButton("action5", "Predict"),
                    br(),
                    br()


                ),
                plotOutput("nextWeekOutput")
              )
      )
      # tabItem(tabName = "likely",
      #         fluidRow(
      #           strong(h3("About:")),
      #           paste0("This tab will run models with a target 24hr percentage increase from -5 to 10. Each success threshold is set ",
      #                  "to a probability value of 0.3. The most probable prediction will then be displayed."),
      #           br(),
      #           br(),
      #           box(
      #             actionButton("action3","Predict Most Likely"),
      #             br(),
      #             br(),
      #             strong("Most Probable Outcome:"),
      #             textOutput("mostLikely"),
      #             textOutput("percentChance")
      #           )
      #         )
      # )
    )
  )
  
  
)

# Define server logic
server <- function(input, output) {
# .GlobalEnv = environment()
  # Read in functions
  source("DogeCoinML.R")
  
  observeEvent(input$action3, {
    showModal(modalDialog("Predicting Most Likely...", footer = NULL))
    on.exit(removeModal())
    all.bst.names = list.files(path = "bsts", pattern = ".rds")
    all.bst.numbers = str_match(string = all.bst.names, pattern = "bst_(.*)\\.")[,2]
    all.bst.path = list.files(path = "bsts", pattern = ".rds", full.names = TRUE)
    all.bst = lapply(all.bst.path, readRDS)
    assign('all.bst.numbers',all.bst.numbers,.GlobalEnv)
    assign('all.bst',all.bst,.GlobalEnv)
    
    predict.best(0.3, all.bst, all.bst.names)
    
    all.predictions = round(all.predictions, digits = 4)
    max.pred = which(all.predictions == max(all.predictions))
    max.bst = all.bst.numbers[max.pred]
    
    output$mostLikely = renderText(paste0("The most probable outcome over the next 24 hours is a change of ",max.bst,"% or more."))
    output$percentChance = renderText(paste0(max(all.predictions)," Probability Predicted"))
  })

  
  observeEvent(input$action1, {
    showModal(modalDialog("Generating Your Model...", footer = NULL))
    on.exit(removeModal())
    createModel(input$slider1, input$slider2, input$select, input$timeframe, input$tp)
    # output$OverallAccuracy = renderInfoBox({
    #   infoBox("Overall Accuracy",paste0(round(overall.accuracy, digits = 2), "%"), icon = icon('check'))
    #   })
    output$Buy = renderInfoBox({infoBox("Accuracy", paste0(round(yes.buy.correct.perc, digits = 2), "%"), icon = icon("thumbs-up"))
    })
    output$SumPercentage = renderInfoBox({
      infoBox("Sum Percentage", paste0(round(sum.percentage, digits = 2), "%"),icon = icon("money-bill-trend-up"))
      })
    # output$DontBuy = renderInfoBox({infoBox("'Don't Buy' Correct", paste0(round(no.buy.correct.perc, digits = 2),"%"),icon = icon("thumbs-down"))
    #   })
    output$Predictions = renderInfoBox({infoBox("'Number of Predictions", paste0(nrow(compare)))
    })
    output$Hits = renderInfoBox({infoBox("'Number of 'Hits'", paste0(nrow(compare[compare$Prediction == 1,])))
    })
    
    output$table1 = renderDataTable(compare, rownames = FALSE, options = list(pageLength = 20))
    output$modelPlot = renderPlot(hist(compare$Probability))
    output$modelPlot = renderPlot(ggplot(data = compare, aes(x = Probability)) + geom_histogram(colour = "blue", alpha = 0.3))
  })
  
  observeEvent(input$action2, {
    predict.tomorrow(0.3, input$select)
    output$textToday = renderText(paste0("Probability of: ",round(predict.now, digits = 4)))
    output$predict = renderInfoBox({
      infoBox("Predicted Probability",round(predict.now, digits = 4))
    })
  })
  
  observeEvent(input$action4, {
    showModal(modalDialog("Generating predictions...", footer = NULL))
    on.exit(removeModal())
    predict.tomorrow.multiple(input$checkGroup, input$timeframePredict, input$slider3, .GlobalEnv)
    output$multipleOutput = renderDataTable(predictions.df.comb,
                                            rownames = FALSE,
                                            extensions = "Buttons",
                                            options = list(paging = FALSE, searching = FALSE, dom = 'Bfrtip', buttons = c('csv')))
  })

  observeEvent(input$action5, {
    showModal(modalDialog("Generating predictions...", footer = NULL))

    output$nextWeekOutput = renderPlot(predict_week(input$selectNextWeek, input$selectTimeFrame))
    on.exit(removeModal())
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
