library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(smooth)
library(reshape2)
require(scales)
library(TTR)
Sys.setlocale(category = "LC_TIME", locale="us")


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
    
    monthlyDataRaw <<- monthlyData
    
    ##Creating The time series
    ts(monthlyData$Adj.Close, 
       start = c(as.numeric(monthlyData$year[1]),
                as.numeric(monthlyData$month[1])), 
                frequency = 12)
    
    
  })
  
  
  output$plot12 <- renderPlot({
    
    data2 <- dailyData()
    #data2 <- t
    
    data2 <- data2 %>% 
      mutate(shortMA = SMA(Adj.Close, 50),longMA = SMA(Adj.Close, 200)) %>% 
      select(Date, Adj.Close, shortMA, longMA) %>% as.data.frame() 
    
    colnames(data2)[2] <- "Price"
    colnames(data2)[3] <- "MA(50)" 
    colnames(data2)[4] <- "MA(200)"
    
    maLong <<- tail(data2, 1)
    
    data2 <- melt(data2, id = "Date")
    
    ggplot(subset(data2, as.Date(Date) > Sys.Date()-500), aes(as.Date(Date), value, color = variable)) + 
      geom_line(size = 0.8) + 
      labs(x = "Date",
           y = "Daily Closing Price") +
      scale_x_date(breaks = date_breaks("3 months"),
                   labels = date_format("%Y-%m")) +
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
  
  
  
  output$plot2 <- renderPlot({
    
    data2 <- dailyData()
    
    data2 <- data2 %>% 
      mutate(shortMA = SMA(Adj.Close, input$shortMA),longMA = SMA(Adj.Close, input$longMA)) %>% 
      select(Date, Adj.Close, shortMA, longMA) %>% as.data.frame() 
    
    colnames(data2)[2] <- "Price"
    colnames(data2)[3] <- paste0("MA(",input$shortMA,")") 
    colnames(data2)[4] <- paste0("MA(",input$longMA,")")
    
    maShort <<- tail(data2, 1)
    
    data2 <- melt(data2, id = "Date")
    
    ggplot(subset(data2, as.Date(Date) > Sys.Date()-100), aes(as.Date(Date), value, color = variable)) + 
      geom_line(size = 0.8) + 
      labs(x = "Date",
           y = "Daily Closing Price") +
      scale_x_date(breaks = date_breaks("months"),
                   labels = date_format("%Y-%m")) +
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
    seasonData <<- decom
    
    autoplot(decom) +  
      labs(x = "Year",
           y = "") +
      theme(panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = 20),
            plot.title = element_text(size = rel(1.5)))
    
    
  })   
  
  
  
  
  textData <- reactive({
 
    data <- dailyData()
    
    maxDate <- max(as.Date(data$Date))
    minDate <- min(as.Date(data$Date))
    
   firstClose <- paste(data[(data$Date == as.character(minDate)),][7])
   lastClose <- paste(data[(data$Date == as.character(maxDate)),][7])
    
    prevClose <- paste(data[(data$Date == as.character(max(
                      as.Date(data[(format(as.Date(data$Date), "%Y") == (as.numeric(
                        max(format(as.Date(data$Date), "%Y"))) - 1)),][,1])))),][7])
    
    ytdReturn <- (as.numeric(lastClose)/as.numeric(prevClose)-1)*100
    totalReturn <- (as.numeric(lastClose)/as.numeric(firstClose)-1)*100
    
    ytdText <- ifelse(ytdReturn > 0, "increase", "decrease")
    
##########    
    
    longTrend <- maLong
    
    longAboveBelow <- ifelse(longTrend[,3] > longTrend[,4], "above", "below")
    longPosNeg <- ifelse(longTrend[,3] > longTrend[,4], "positive", "negative")
    
    
    
    shortTrend <- maShort
    
    shortAboveBelow <- ifelse(shortTrend[,3] > shortTrend[,4], "above", "below")
    shortPosNeg <- ifelse(shortTrend[,3] > shortTrend[,4], "positive", "negative")
    
    sameTrueFalse <- ifelse(longPosNeg == shortPosNeg, "The same", "However, the opposite")
    
    seasonData <- seasonData
    monthlyDataRaw <- monthlyDataRaw
    
    season <- head(cbind(monthlyDataRaw, data.frame(seasonData$time.series)), 12)
    season$monthName <-  format(season$Date, "%B")
    season <- season[order(-season$seasonal),]
    
    bestMonths <- paste0(head(season$monthName, 2), collapse = " and ")
    worstMonths <- paste0(tail(season$monthName, 2), collapse = " and ")
    
##########    
    
    
    expModelIn <- expModelOut
    expModelIn$yearMonth <- row.names(expModelIn)
    
    arimaModelIn <- arimaModelOut
    arimaModelIn$yearMonth <- row.names(arimaModelIn)
    
    expTarget <- tail(expModelIn[,1], 1)
    expTargetMonth <- tail(expModelIn[,6], 1)
    expPotential <- (as.numeric(expTarget)/as.numeric(lastClose)-1)*100
    
    
    arimaTarget <- tail(arimaModelIn[,1], 1)
    arimaPotential <- (as.numeric(arimaTarget)/as.numeric(lastClose)-1)*100
    
    
      data.frame(company = paste(input$tickers),
              minDate = minDate,
              maxDate = maxDate,
              lastClose = format(round(as.numeric(lastClose), digits = 2), nsmall = 2),
              ytdReturn = paste(format(round(ytdReturn, digits = 1), nsmall = 1), "%"),
              totalReturn = paste(format(round(totalReturn, digits = 1), nsmall = 1), "%"),
              ytdText = ytdText,
              longAboveBelow = longAboveBelow,
              longPosNeg = longPosNeg,
              shortAboveBelow = shortAboveBelow,
              shortPosNeg = shortPosNeg,
              sameTrueFalse = sameTrueFalse,
              bestMonths = bestMonths,
              worstMonths = worstMonths,
              expTarget = format(round(as.numeric(expTarget), digits = 2), nsmall = 2),
              expTargetMonth = expTargetMonth,
              expPotential = paste(format(round(expPotential, digits = 1), nsmall = 1), "%"),
              arimaTarget = format(round(as.numeric(arimaTarget), digits = 2), nsmall = 2),
              arimaPotential = paste(format(round(arimaPotential, digits = 1), nsmall = 1), "%"))
    
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
  
 
  output$plot12 <- renderPlot({
    
    data2 <- dailyData()
    #data2 <- t
    
    data2 <- data2 %>% 
      mutate(shortMA = SMA(Adj.Close, 50),longMA = SMA(Adj.Close, 200)) %>% 
      select(Date, Adj.Close, shortMA, longMA) %>% as.data.frame() 
    
    colnames(data2)[2] <- "Price"
    colnames(data2)[3] <- "MA(50)" 
    colnames(data2)[4] <- "MA(200)"
    
    maLong <<- tail(data2, 1)
    
    data2 <- melt(data2, id = "Date")
    
    ggplot(subset(data2, as.Date(Date) > Sys.Date()-500), aes(as.Date(Date), value, color = variable)) + 
      geom_line(size = 0.8) + 
      labs(x = "Date",
           y = "Daily Closing Price") +
      scale_x_date(breaks = date_breaks("3 months"),
                   labels = date_format("%Y-%m")) +
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
  
  
   
  

  

  
  output$plot4 <- renderPlot({
    
 
    ts.stock1 <- monthlyTimeData()

    ###Fitting the best exponential model based on historic data
    
    fit1 <- ets(ts.stock1, model = "ZZZ")
    
    pred1 <- forecast(fit1,input$period)
    expModelOut <<- data.frame(pred1)
    
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
    arimaModelOut <<- data.frame(pred2)
    
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
    
    sprintf("Based on the last closing price %s (%s) for %s, the year-to-date share price  %s has been %s and 
            the share price performance since %s has been %s", 
            td$lastClose, td$maxDate, td$company, td$ytdText, td$ytdReturn, td$minDate, td$totalReturn)
       
  
    
    }) 
  
  
  output$text2 <- renderText({
    
    td <- textData()
    sprintf("Currently the 50 day moving average for %s is %s the 200 day moving average 
    indicating that the share is in a %s long term trend. %s applies for the short term 
    trend where the %s day moving average is %s the %s day moving average.", 
            td$company, td$longAboveBelow, td$longPosNeg, td$sameTrueFalse, input$shortMA, td$shortAboveBelow, input$longMA)

    
    
    
  }) 
  
  
  output$text3 <- renderText({
    
    td <- textData()
   
    sprintf("After decomposing the time series there are tendencies of %s being the strongest months for %s. 
            The worst performing months have on the other hand been %s", td$bestMonths, td$company, td$worstMonths)  
    
    
    
  }) 
  
  
  
  output$text4 <- renderText({
    
    td <- textData()
    
    sprintf("The monthly closing prices for %s have been modeled with both exponetial and ARIMA models. For 
            each of these methods, the best model based on historic data has been chosen and used for prediction. 
            The best exponential model was chosen by minimizing the log-likelihood and the best ARIMA model 
            was picked based on the lowest AIC. Based on the best exponetial model, the target price for %s is 
            %s. The target price for the same period based on the best ARIMA model is %s. Based on these 
            prediction models, the potential for the share is %s and %s respectively.", 
            td$company, td$expTargetMonth, td$expTarget, td$arimaTarget, td$expPotential, td$arimaPotential)  
    
    
    
  }) 
  
  
  
  
})
  





