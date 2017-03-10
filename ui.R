library(shiny)
library(xlsx)

symbols <- read.xlsx("OMXS30.xlsx", sheetName = 1)
symbols$yahooTicker <- paste0(gsub(" ", "-", symbols$SecuritySymbol), ".ST")


shinyUI(fluidPage(
  
 
  titlePanel("Share Price Analysis"),
  
  hr(),
  sidebarLayout(
    sidebarPanel(
      
      helpText("The application allows you to select a OMXS30 share and analyse both the long term and short time trends. 
                In addition, monthly closing prices are decomposed into a trend component, a seasonal component and the 
                irregular component by loess smoothing. Finally, the best exponential model as well as the best ARIMA model are fitted on 
                the historic data and used to predict future share prices."),
      
      hr(),
      selectInput("tickers", "Ticker:", choices = symbols$CompanyName),
      dateInput('startDate', label = 'Start Date:', value = "2005-01-15"),
      
      hr(),
  
      sliderInput("shortMA",
                  "Moving Average (Short):",
                  min = 5,
                  max = 15,
                  value = 10),
      
      sliderInput("longMA",
                  "Moving Average (Long):",
                  min = 20,
                  max = 50,
                  value = 25),
      
      hr(),
      
      sliderInput("period",
                   "Months to forecast:",
                   min = 1,
                   max = 24,
                   value = 12),
      hr(),
      
      submitButton("Analyse"),
      hr(),
      
      h3("Analysis"),
      hr(),
      h4("Overview"),
      textOutput("text1"),
      hr(),
      h4("Current Trends"),
      textOutput("text2"),
      hr(),
      h4("Seasonality"),
      textOutput("text3"),
      hr(),
      h4("Prediction"),
      textOutput("text4")
    
   ),
    
    mainPanel(
      h3("Historical Price Development"),
      h6("Historical daily closing prices smoothed with LOESS"),
       plotOutput("plot1"), 
      
      h3("Long Term Trend - MA(50) vs. MA(200)"),
      h6("Current share price compared to long term moving averages"),
      
      plotOutput("plot12"),
      #textOutput("text2"),
      
      h3("Short Term Trend - Chosen Period"),
      h6("Current share price compared to short term (user defined) moving averages"),
      
      plotOutput("plot2"),
      
      
      
       h3("Price Decomposition"),
      h6("Monthly closing prices have been decomposed into a trend, a seasonal component and the remaining residuals"), 
      
      
      plotOutput("plot3"),
      h3("Prediction - Exponential Model"),
      h6("Based on the monthly history an optimal exponential model has been used for prediction"),
       plotOutput("plot4"),
      h3("Prediction - ARIMA Model"),
      h6("Based on the monthly history an optimal ARIMA model has been used for prediction"),
      
       plotOutput("plot5")
   
       
        )
  )

))
