# Problem 1---- 

library(tidyquant)
library(tidyverse)
library(lubridate)


info = tidyquant::tq_get("AMZN",from = lubridate::ymd(FromDate),
                        to = lubridate::ymd(ToDate)) %>%
                        dplyr::select(symbol, date, adjusted)

dates = base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd(FromDate),
                                                           to = lubridate::ymd(ToDate),
                                                           by = "day")),
Symbol = c(base::rep("AMZN",base::as.numeric(lubridate::ymd(ToDate) - lubridate::ymd(FromDate)) + 1)))

DATA = dates %>%
  dplyr::left_join(info, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")


SMA = DATA %>%
  dplyr::mutate(sma20 = TTR::SMA(adjusted, n = 20)) %>%
  dplyr::mutate(sd20 = RcppRoll::roll_sd(adjusted, n = 20, align = "right", fill = NA),
              UpperBound = sma20 + 2*sd20,
              LowerBound = sma20 - 2*sd20)%>%
  dplyr::filter(!is.na(sma20)) %>%
  dplyr::mutate(signal = dplyr::case_when(dplyr::lag(adjusted) < dplyr::lag(UpperBound) & adjusted > UpperBound ~ "sell",
                                          TRUE ~"buy"))
# Problem 2----

RSI = DATA %>%
  dplyr::mutate(gain_loss = (adjusted/lag(adjusted))-1,
                gain_loss = if_else(is.na(gain_loss), 0, gain_loss),
                average_gain1 = case_when(gain_loss >= 0 ~gain_loss, 
                                          gain_loss <0 ~ 0),
                average_gain = SMA(average_gain1, 14),
                average_loss1 = case_when(gain_loss <= 0 ~abs(gain_loss), 
                                          gain_loss >0 ~ 0),
                average_loss = SMA(average_loss1, 14),
                RSI1 = 100-(100/(1+(average_gain/average_loss))),
                RSI = 100-(100/(1+((13*lag(average_gain)+ average_gain1)/(13*lag(average_loss)+average_loss1)))),
                buy_sell = case_when(RSI > 65 ~ "sell",
                                     RSI < 35 ~ "buy"),
                ProfitCoeff = adjusted/lag(adjusted),
                ProfitCoeff = if_else(is.na(ProfitCoeff), 1, ProfitCoeff),
                BenchmarkMoney = 100 * cumprod(ProfitCoeff),
                buy_sell_for_comparison = buy_sell)%>%
  tidyr::fill(buy_sell_for_comparison, .direction = "down")%>%
  dplyr::mutate(buy_sell_for_comparison = if_else(is.na(buy_sell_for_comparison), "buy", buy_sell_for_comparison),
                ProfitCoeff_strategy = case_when(buy_sell_for_comparison == "sell"~ 1,
                                                 buy_sell_for_comparison == "buy"~ ProfitCoeff),
                StrategyMoney = 100 * cumprod(ProfitCoeff_strategy))
