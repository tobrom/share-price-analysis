library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(smooth)
library(reshape2)
require(scales)
library(TTR)


shinyServer(function(input, output) {
   
  
  dailyData <- reactive({

    
  ticker <- paste(symbols[(symbols$CompanyName == input$tickers),][3])
   start.date <- input$startDate
   end.date <- Sys.Date()
 
    sd <- substr(start.date, 9, 10) 
    sm <- as.character(as.numeric(substr(start.date, 6, 7)) - 1)
    sy <- substr(start.date, 1, 4)
    
    ed <- substr(end.date, 9, 10) 
    em <- substr(end.date, 6, 7)
    ey <- substr(end.date, 1, 4) 
    
    url <- paste0("http://real-chart.finance.yahoo.com/table.csv?s=",ticker,
                  "&a=",sm,"&b=",sd,"&c=",sy,"&d=",em,
                  "&e=",ed,"&f=",ey,"&g=d&ignore=.csv")
    
   url %>% read.table(header = TRUE, sep = ",") %>% 
     arrange(Date) %>% 
    subset(Volume > 0) 

    
})
  

  

  

  
  
  
  
  
  
  
  
  
  
  
  monthlyTimeData <- reactive({
    
    monthlyData <- dailyData()
    monthlyData$Date <- as.Date(monthlyData$Date) 
    monthlyData$month <- format(as.Date(monthlyData$Date), "%m")
    monthlyData$year <- format(as.Date(monthlyData$Date), "%Y")
    
    monthlyData <- monthlyData %>% 
      group_by(year, month) %>% 
      summarise(Date = max(as.Date(Date))) %>%
      merge(monthlyData, by = c("Date", "year", "month")) %>% 
      as.data.frame()
    
    ##Creating The time series
    ts(monthlyData$Adj.Close, 
       start = c(as.numeric(monthlyData$year[1]),
                as.numeric(monthlyData$month[1])), 
                frequency = 12)
    
    
  })
  
  
  
  
  
  
  textData <- reactive({
   #data <- dailyData
    data <- dailyData()
    
    maxDate <- max(as.Date(data$Date))
    minDate <- min(as.Date(data$Date))
    
   firstClose <- paste(data[(data$Date == as.character(minDate)),][7])
   lastClose <- paste(data[(data$Date == as.character(maxDate)),][7])
    
    prevClose <- paste(data[(data$Date == as.character(max(
                      as.Date(data[(format(as.Date(data$Date), "%Y") == (as.numeric(
                        max(format(as.Date(data$Date), "%Y"))) - 1)),][,1])))),][7])
    
    ytdReturn <- round((as.numeric(lastClose)/as.numeric(prevClose)-1)*100, digits = 1)
    totalReturn <- round((as.numeric(lastClose)/as.numeric(firstClose)-1)*100, digits = 1)
    
    ytdText <- ifelse(ytdReturn > 0, "increase", "decrease")
    
    
    
    
    
    
   data.frame(company = paste(input$tickers),
               minDate = minDate,
             maxDate = maxDate,
               lastClose = lastClose,
               ytdReturn = ytdReturn,
               totalReturn = totalReturn,
               ytdText = ytdText)
    
  })
  
  
  
  
  
  
  
  
  output$plot1 <- renderPlot({
    
    data1 <- dailyData()
    
    ggplot(data1, aes(as.Date(Date), Adj.Close)) + 
      geom_line() + geom_smooth(method = "loess") + 
      labs(x = "Year",
           y = "Daily Closing Price") +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
            plot.title = element_text(size = rel(1.5)))
    
    
  })  
  
  
  
output$plot2 <- renderPlot({

      data2 <- dailyData()
    
      data2 <- data2 %>% 
        mutate(shortMA = SMA(Adj.Close, input$shortMA),longMA = SMA(Adj.Close, input$longMA)) %>% 
        select(Date, Adj.Close, shortMA, longMA) %>% as.data.frame() 

      colnames(data2)[2] <- "Price"
      colnames(data2)[3] <- paste0("MA(",input$shortMA,")") 
      colnames(data2)[4] <- paste0("MA(",input$longMA,")")
      
      data2 <- melt(data2, id = "Date")
     
      ggplot(subset(data2, as.Date(Date) > Sys.Date()-100), aes(as.Date(Date), value, color = variable)) + 
        geom_line(size = 1) + 
        labs(x = "Date",
             y = "Daily Closing Price") +
        scale_colour_manual(values = c("#000000", 
                                       "#0000FF", 
                                       "#B0C4DE")) +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              legend.title=element_blank(),
              legend.key = element_blank(),
              panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
              plot.title = element_text(size = rel(1.5)))
           
    
  })  
  
  
output$plot3 <- renderPlot({
    
    ts.stock <- monthlyTimeData()
    
    ###Decomposing the time series
    decom <- stl(ts.stock, s.window = "period")
    
    
    autoplot(decom) +  
      labs(x = "Year",
           y = "") +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
            plot.title = element_text(size = rel(1.5)))
    
    
  })   
  
  
  output$plot4 <- renderPlot({
    
 
    ts.stock1 <- monthlyTimeData()

    ###Fitting the best exponential model based on historic data
    
    fit1 <- ets(ts.stock1, model = "ZZZ")
    
    pred1 <- forecast(fit1,input$period)
    
    autoplot(pred1) +labs(x = "Year",
                         y = "Monthly Closing Price",
                         caption = paste("Model: ", pred1$model[13])) +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
            plot.title = element_blank())
    
    
  })   
  
  
  output$plot5 <- renderPlot({
    
    ts.stock2 <- monthlyTimeData()
    
    ###Fitting the best arima model
    
    fit2 <- auto.arima(ts.stock2)
    
    pred2 <- forecast(fit2, input$period)
    
    #tes <<- paste(forecast(fit2)$method)
    
      autoplot(pred2) +labs(x = "Year",
                         y = "",
                         caption = paste("Model:", forecast(fit2)$method)) +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
            plot.title = element_blank())
    
  })   
  
  output$text1 <- renderText({
    
    td <- textData()
    
    sprintf("The last closing price SEK %s (%s) for %s indicates a year-to-date share 
            price %s with %s percentage. The share price performance for the whole period (since %s) has been %s percentage", 
            td$lastClose, td$maxDate, td$company, td$ytdText, td$ytdReturn, td$minDate, td$totalReturn)
       
  
    
    }) 
  
})
  




