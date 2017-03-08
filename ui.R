library(shiny)
library(xlsx)

symbols <- read.xlsx("OMXS30.xlsx", sheetName = 1)
symbols$yahooTicker <- paste0(gsub(" ", "-", symbols$SecuritySymbol), ".ST")


shinyUI(fluidPage(
  
 
  titlePanel("Time Series Analysis"),
  
  hr(),
  sidebarLayout(
    sidebarPanel(
      
      helpText("The application allows you to select a OMXS30 share and analyse both the long term and short time trends. 
                In addition, monthly closing prices are decomposed into a trend component, a seasonal component and the 
                remaining residuals. Finally, the best exponential model as well as the best ARIMA model are fitted on 
                the historic data and used to predict future share prices."),
      
      hr(),
      selectInput("tickers", "Ticker:", choices = symbols$CompanyName),
      dateInput('startDate', label = 'Start Date:', value = "2012-01-02"),
      
      sliderInput("period",
                   "Months to forecast:",
                   min = 1,
                   max = 24,
                   value = 12),
      hr(),
      
      submitButton("Analyse"),
      hr(),
      h3("Summary"),
      textOutput("text1")
     
    
   ),
    
    mainPanel(
      h3("Long Term Share Price Development"),
      h6("Historical daily closing prices smoothed with LOESS"),
       plotOutput("plot1"), 
      h3("Short Term Price Trends"),
      h6("Current share price compared to moving averages"),
      
      plotOutput("plot2"),
      #verbatimTextOutput("summary"),
      h3("Monthly Share Price Decomposition"),
      h6("Monthly closing prices have been decomposed into a trend, a seasonal component and the remaining residuals"), 
      
      
      plotOutput("plot3"),
      h3("Share Price Prediction - Exponential Model"),
      h6("Based on the monthly history an optimal exponential model has been used for forecasting"),
       plotOutput("plot4"),
      h3("Share Price Prediction - ARIMA Model"),
      h6("Based on the monthly history an optimal ARIMA model has been used for forecasting"),
      
       plotOutput("plot5")
   
       
        )
  )
))
