library(tidyverse) 
library(tidyquant) 
library(lubridate) 
library(quantmod) 


######Problem 1 ----
# 1.1. 
library(TTR)

price <- c(1, 7, 4, 10, 9, 15)
newSMA <- function (price,n){
  f <- c()
  for (i in n:length(price)){
    f[i]<-mean(price[(i-n+1):i])
  }
  return(f)
} 

newSMA(price, n=2) 

SMA(price, n=2)

#####Problem 2 ----


for(i in 1:100){
  if(i==1){
    next
  }
  else if(i==2){
    i =2}
  else if(i%%2==0){
    next}
  else if(i==3){
    i=3}
  else if(i%%3==0){
    next
  }
  else if (i==5){
    i=5}
  else if(i%%5==0){
    next
  }
  else if(i==7){
    i=7}
  else if(i%%7==0){
    next
  }
  print(i)}


stock_prices <- tidyquant::tq_get("NKE")

# 1. 
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ))


# 2.
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12))

# 3.
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12), 
                MACD_line = EMA12 - EMA26)

# 4.
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(stock_prices$adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12), 
                MACD_line = EMA12 - EMA26, 
                Signal_Line = TTR::EMA(MACD_line, n =9))

# 5
stock_prices <- tidyquant::tq_get("NKE") %>% 
  dplyr::mutate(EMA26 = TTR::EMA(adjusted, n = 26 ),
                EMA12 = TTR::EMA(adjusted, n = 12), 
                MACD_line = EMA12 - EMA26, 
                Signal_Line = TTR::EMA(MACD_line, n =9)) %>% 
  ungroup() %>%
  dplyr::filter(!is.na(MACD_line & EMA26)) %>% 
  dplyr::mutate(cross = case_when(MACD_line > Signal_Line & dplyr::lag(Signal_Line) > dplyr::lag(MACD_line) ~ "buy",
                                  MACD_line < Signal_Line & dplyr::lag(Signal_Line)< dplyr::lag(MACD_line)~ "sell",
                                  TRUE ~ "hold"))


# 6. 

Benchmark <- 100
Buy_hold <- stock_prices %>% 
  mutate(product_coeff = adjusted/lag(adjusted), 
         Price_Benchmark = Benchmark*product_coeff, 
         decision = case_when(Price_Benchmark > 100 ~ "hold",
                              Price_Benchmark < 100 ~ "buy",
                              TRUE ~ "indifferent"))
