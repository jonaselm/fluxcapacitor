#' Optimize a strategy parameter
#'
#' This function can be used to find the optimal value of a range of values for a strategy parameter.
#'
#' From the list of values in the optimize range, the function will evaluate the profitability of the strategy
#' under each optimize_range value. The function returns the value that results in the highest ledger value at the end
#' of the test.
#'
#'
#' @param strategy_object a \code{\link{strategy}} object
#' @param generator the function used to generate the indicator value being optimized
#' @param generator_args a named list of parameters to use with the generator function. Parameters to be optimized should be denoted with a question mark (?).
#' @param optimize_range a range of values to optimize amongst.
#' @param signal the signal to evaluate the reward function on.
#'
#' @return the value of optimize_range that results in the highest ledger balance.
#' @export
#'
#' @examples
optimize_strategy <- function(strategy_object, generator, generator_args, optimize_range, signal) {

  generator_args_sequence <- lapply(optimize_range, FUN = function(x,generator_args){
                                                          x <- as.character(x)
                                                          stringr::str_replace_all(generator_args, "\\?", x)
                                                          }, generator_args)

  signal <- stringr::str_replace_all(signal, "\\?", "optimized_indicator")

  cat("Optimizing Strategy: \n")
  pb <- txtProgressBar(min = 0, max = length(generator_args_sequence), style = 3)

  opt <- rep(NA, length(generator_args_sequence))

  # For loop used here to take advantage of progress bar w/ negligible speed cost
  for (i in seq_along(generator_args_sequence)) {

    setTxtProgressBar(pb, i)

    results <- strategy_object %>%
      add_indicator(indicator_name = "optimized_indicator",
                    generator = generator,
                    generator_args = generator_args_sequence[[i]]) %>%
      add_signal(signal_name = "optimizer",
                 signal = signal) %>%
      compile_strategy(signals = "optimizer") %>%
      backtest(progress = FALSE)

    opt[i] <- dplyr::last(results$ledger$Acct_Val)
  }

  close(pb)

  strategy_object$Optimized <- list( Parameters = optimize_range,
                                     Equity = opt,
                                     best = optimize_range[which.max(opt)]
  )

  # Track tests for overfitting
  if (length(strategy_object$Tests) > 0) {

    strategy_object$Tests <- strategy_object$Tests + length(generator_args_sequence)

  } else {

    strategy_object$Tests <- length(generator_args_sequence)

  }

  return(strategy_object)

}
