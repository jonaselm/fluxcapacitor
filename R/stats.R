return_calculate <- function(strategy_object, method = "arithmetic") {

  value <- strategy_object$ledger %>% dplyr::select(Date, Acct_Val)

  # Default to arithmetic returns
    Returns <- value %>% dplyr::mutate(Returns = Acct_Val / dplyr::lag(Acct_Val) - 1) %>% dplyr::select(Date, Returns)


  if (method == "log") {
    Returns <- value %>% dplyr::mutate(Returns = c(0, diff(log(Acct_Val)))) %>% dplyr::select(Date, Returns)
  }


    Returns
}


sharpe_ratio <- function(strategy_object, risk_free = 0, scale = 1) {

  returns <- return_calculate(strategy_object) %>% dplyr::select(Returns)

  if (length(risk_free) == 1 | length(risk_free) == nrow(returns)) {
    returns <- returns %>%
      dplyr::mutate(risk_rree = risk_free) %>% stats::na.omit() %>%
      dplyr::mutate(Excess_Returns = Returns - risk_free)

  } else stop("Risk-free rate must either be a single number or a vector of equal periodicity to the data.")

  returns %>% summarize(Sharpe_Ratio = (mean(Excess_Returns)/sd(Excess_Returns)) * sqrt(scale))

}

information_ratio <- function(strategy_object, benchmark_returns) {

  returns <- return_calculate(strategy_object) %>% dplyr::select(Returns)

  if (length(benchmark_returns) == 1 | length(benchmark_returns) == nrow(returns)) {
    returns <- returns %>%
      dplyr::mutate(benchmark = benchmark_returns) %>% stats::na.omit() %>%
      dplyr::mutate(Excess_Returns = Returns - benchmark)

  } else stop("Benchmark returns must either be a single number or a vector of equal periodicity to the data.")

  returns %>% summarize(Information_Ratio = (mean(Excess_Returns)/sd(Excess_Returns)))


}

strategy_drawdowns <- function(strategy_object) {

  drawdowns <- rep(0, nrow(strategy_object$ledger))
  prices <- strategy_object$ledger$Acct_Val

  for (i in seq_along(prices)) {
    if(max(prices[1:i]) > prices[i]) {
      drawdowns[i] <- (max(prices[1:i]) - prices[i]) / max(prices[1:i])
    }

  }

  strategy_object$ledger %>% dplyr::mutate(Drawdowns = drawdowns) %>% dplyr::select(Date, Drawdowns)

}


