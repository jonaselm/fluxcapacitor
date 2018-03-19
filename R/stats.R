#' Calculate strategy returns
#'
#' This function calulates the per-period returns for a backtested strategy object. This is useful for
#' any portfolio analytics calculations that require portfolio values to be passed as returns. This function
#' can calculate either arithmetic or log returns by specifying the method argument.
#'
#' @param strategy_object a strategy object
#' @param method "arithmetic" or "log" returns.
#'
#' @return A tibble of dates and per-period returns
#' @export
#'
#' @examples
return_calculate <- function(strategy_object, method = "arithmetic") {

  value <- strategy_object$ledger %>% dplyr::select(Date, Acct_Val)

  # Default to arithmetic returns
    Returns <- value %>% dplyr::mutate(Returns = Acct_Val / dplyr::lag(Acct_Val) - 1) %>% dplyr::select(Date, Returns)


  if (method == "log") {
    Returns <- value %>% dplyr::mutate(Returns = c(0, diff(log(Acct_Val)))) %>% dplyr::select(Date, Returns)
  }


    Returns
}


#' Calculate a strategy's Sharpe ratio
#'
#' The Sharpe ratio use used to measure risk-adjusted return. This function also implements a naive form
#' of scaling. For instance, for daily periodicity, using 252 as the scaling factor will annualize the result.
#' Note that per Andrew Lo's 2003 paper
#' \link{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=377260}{The Statistics of Sharpe Ratios}, this
#' method is not recommended on its own except under very special circumstances.
#'
#' @param strategy_object a strategy object
#' @param risk_free the risk-free rate, either numeric or a vector with the same periodicity as the strategy object's ledger.
#' @param scale a scaling factor.
#'
#' @return a tibble contaning the strategy's Sharpe ratio.
#' @export
#'
#' @examples
sharpe_ratio <- function(strategy_object, risk_free = 0, scale = 1) {

  returns <- return_calculate(strategy_object) %>% dplyr::select(Returns)

  if (length(risk_free) == 1 | length(risk_free) == nrow(returns)) {
    returns <- returns %>%
      dplyr::mutate(risk_rree = risk_free) %>% stats::na.omit() %>%
      dplyr::mutate(Excess_Returns = Returns - risk_free)

  } else stop("Risk-free rate must either be a single number or a vector of equal periodicity to the data.")

  returns %>% summarize(Sharpe_Ratio = (mean(Excess_Returns)/sd(Excess_Returns)) * sqrt(scale))

}

#' Calculate a strategy's information ratio
#'
#' The information ratio is used to measure risk-adjusted return relative to a benchmark.
#'
#' @param strategy_object a strategy object
#' @param benchmark_returns the benchmark returns, either numeric or a vector with the same
#' periodicity as the strategy object's ledger.
#'
#' @return a tibble containing the strategy's information ratio
#' @export
#'
#' @examples
information_ratio <- function(strategy_object, benchmark_returns) {

  returns <- return_calculate(strategy_object) %>% dplyr::select(Returns)

  if (length(benchmark_returns) == 1 | length(benchmark_returns) == nrow(returns)) {
    returns <- returns %>%
      dplyr::mutate(benchmark = benchmark_returns) %>% stats::na.omit() %>%
      dplyr::mutate(Excess_Returns = Returns - benchmark)

  } else stop("Benchmark returns must either be a single number or a vector of equal periodicity to the data.")

  returns %>% summarize(Information_Ratio = (mean(Excess_Returns)/sd(Excess_Returns)))


}

#' Calulate a strategy's drawdowns
#'
#' Drawdowns measure the performance deterioration from a previous high water mark.
#'
#' @param strategy_object a strategy object
#'
#' @return a tibble containing per-period drawdown statistics
#' @export
#'
#' @examples
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


