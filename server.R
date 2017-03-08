library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(smooth)
library(reshape2)
require(scales)
library(TTR)


shinyServer(function(input, output) {
   
  
  yahooData <- reactive({
 
   ticker <- paste(symbols[(symbols$CompanyName == input$tickers),][3])
   start.date <- input$startDate
   end.date <- Sys.Date()
    
    sd <- substr(start.date, 9, 10) 
    sm <- substr(start.date, 6, 7)
    sy <- substr(start.date, 1, 4)
    
    ed <- substr(end.date, 9, 10) 
    em <- substr(end.date, 6, 7)
    ey <- substr(end.date, 1, 4) 
    
    url <- paste0("http://real-chart.finance.yahoo.com/table.csv?s=",ticker,
                  "&a=",sm,"&b=",sd,"&c=",sy,"&d=",em,
                  "&e=",ed,"&f=",ey,"&g=d&ignore=.csv")
    
    url %>% read.table(header = TRUE, sep = ",") %>% 
     arrange(Date) %>% 
    subset(Volume > 0) %>%
     mutate(MA5 = SMA(Adj.Close, 5),
            MA20 = SMA(Adj.Close, 20))
    
})
  
  
  
output$plot1 <- renderPlot({
    
    data1 <- yahooData()
    
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

      data2 <- yahooData()

      data2 <- data2[,c(1,7, 8,9)]
      colnames(data2)[2] <- "Price"
      
      data2 <- melt(data2, id = "Date")
     
      ggplot(subset(data2, as.Date(Date) > Sys.Date()-50), aes(as.Date(Date), value, color = variable)) + 
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
    
    data3 <- yahooData()
    data3$Date <- as.Date(data3$Date) 
    data3$month <- format(as.Date(data3$Date), "%m")
    data3$year <- format(as.Date(data3$Date), "%Y")
    
    data3 <- data3 %>% 
             group_by(year, month) %>% 
             summarise(Date = max(as.Date(Date))) %>%
             merge(data3, by = c("Date", "year", "month")) %>% 
             as.data.frame()
    
    ##Creating The time series
    ts.stock <- ts(data3$Adj.Close, 
                   start = c(as.numeric(data3$year[1]),
                             as.numeric(data3$month[1])), 
                   frequency = 12)
    
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
    
    data4 <- yahooData()
    data4$Date <- as.Date(data4$Date) 
    data4$month <- format(as.Date(data4$Date), "%m")
    data4$year <- format(as.Date(data4$Date), "%Y")
    
    data4 <- data4 %>% 
      group_by(year, month) %>% 
      summarise(Date = max(as.Date(Date))) %>%
      merge(data4, by = c("Date", "year", "month")) %>% 
      as.data.frame()
    
    
    ##Creating The time series
    ts.stock1 <- ts(data4$Adj.Close, 
                   start = c(as.numeric(data4$year[1]),
                             as.numeric(data4$month[1])), 
                   frequency = 12)
    
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
    
    data5 <- yahooData()
    data5$Date <- as.Date(data5$Date) 
    data5$month <- format(as.Date(data5$Date), "%m")
    data5$year <- format(as.Date(data5$Date), "%Y")
    
    data5 <- data5 %>% 
      group_by(year, month) %>% 
      summarise(Date = max(as.Date(Date))) %>%
      merge(data5, by = c("Date", "year", "month")) %>% 
      as.data.frame()
    
    
    ##Creating The time series
    ts.stock2 <- ts(data5$Adj.Close, 
                    start = c(as.numeric(data5$year[1]),
                              as.numeric(data5$month[1])), 
                    frequency = 12)
    
    ###Fitting the best arima model
    
    fit2 <- auto.arima(ts.stock2)
    
    pred2 <- forecast(fit2, input$period)
    
    tes <<- paste(forecast(fit2)$method)
    
      autoplot(pred2) +labs(x = "Year",
                         y = "",
                         caption = paste("Model:", forecast(fit2)$method)) +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
            plot.title = element_blank())
    
  })   
  
  output$text1 <- renderText(paste(tes)) 
  
})
  
  
 




