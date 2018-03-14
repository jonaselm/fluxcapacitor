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
optimize_strategy <- function(strategy_object, generator, generator_args, optimize_range, signal){

  generator_args_sequence <- lapply(optimize_range, FUN = function(x,generator_args){
                                                          x <- as.character(x)
                                                          str_replace_all(generator_args, "\\?", x)
                                                          }, generator_args)

  signal <- stringr::str_replace_all(signal, "\\?", "optimized_indicator")

  opt <- lapply(generator_args_sequence,
                FUN = function(x, strategy_object, generator, signal){

                  results <- strategy_object %>%
                    add_indicator(indicator_name = "optimized_indicator",
                                                       generator = generator,
                                                       generator_args = x) %>%
                    add_signal(signal_name = "optimizer",
                               signal = signal) %>%
                    compile_strategy(signals = "optimizer") %>%
                    backtest()

                  dplyr::last(results$ledger$Acct_Val)




  }, strategy_object, generator, signal)


  optimize_range[which.max(opt)]

}
