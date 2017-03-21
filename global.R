###Function for converting company name to ticker

getTicker <- function(company) {

  paste(symbols[(symbols$CompanyName == company),][3])
  
}  
  
###Function for extracting the daily data  

getData <- function(ticker, start.date, end.date) {
  
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
  subset(Volume > 0) %>% return() 

}


