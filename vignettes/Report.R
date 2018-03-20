## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE-------------------------------------------------------
require(fluxcapacitor)

universe <- c("SPY", "EFA", "IEF", "DBC", "VNQ")

data(list = c(universe))

## ------------------------------------------------------------------------
report_example <- init_strategy(universe) %>% add_indicator(indicator_name = "SMA_200",
                           generator = "TTR::SMA",
                           generator_args = list("x = CLOSE", "n = 200")
                           ) %>%
             add_signal(signal_name = "my_buy_signal",
                        signal = "CLOSE > SMA_200") %>%
            add_signal(signal_name = "my_sell_signal",
                       signal = "CLOSE < SMA_200",
                       direction = "sell")

## ----echo=TRUE, results='hide'-------------------------------------------
report_example <- report_example %>% 
                  compile_strategy(signals = c("my_buy_signal", "my_sell_signal")) %>%
                  backtest()

## ------------------------------------------------------------------------
chart_equity_curve(report_example)

