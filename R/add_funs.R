
#' Add an indicator to your strategy
#'
#' Calculates an indicator, and adds it to a column in each security's timeseries data.
#'
#' @param strategy_object a \code{\link{strategy}} object
#' @param indicator_name the desired column name of the indicator, used to build signals
#' @param generator the function used to generate the indicator value
#' @param generator_args a named list of parameters to use with the generator function
#'
#' @return a strategy object with indicator applied to all securities
#' @export
#'
add_indicator <- function(strategy_object, indicator_name, generator, generator_args){

  #Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("add_indicator can only be applied to a strategy object.")

  indicator <- paste(generator, "(", paste(as.character(generator_args), collapse = ", "), ")", sep = "")

  strategy_object$Data <- strategy_object$Data %>% dplyr::group_by(Ticker) %>%
    dplyr::mutate(!!indicator_name := eval(parse(text = indicator))) %>%
    dplyr::ungroup()

  return(strategy_object)

}


#' Add a signal to your strategy
#'
#' Determines whether a signal has occurred based on one or more columns in the strategy data, and adds it to a column in each security's timeseries data.
#'
#' @param strategy_object a \code{\link{strategy}} object
#' @param signal_name the desired column name of the signal, used to build rules
#' @param signal a logical argument based on the indicator columns of the security object
#' @param direction the direction of the market bet, buy or sell
#' @param crossover defines whether signal should be a crossover signal. If TRUE, the signal will only occur on first bar of each run of signal condition being met.
#'
#' @return a strategy object with signal applied to all securities
#' @export
#'
add_signal <- function(strategy_object, signal_name, signal, direction = "buy", crossover = TRUE){

  #Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("add_signal can only be applied to a strategy object.")

    strategy_object$Data <- strategy_object$Data %>%
      dplyr::mutate(sig_result = eval(parse(text = signal))) %>%
      dplyr::mutate(sig_result = ifelse(sig_result == 0 | is.na(sig_result),0, 1))

    if (direction == "sell") {

      strategy_object$Data <- strategy_object$Data %>%
      dplyr::mutate(sig_result = -sig_result) #if short, use negative position

    }

    if (crossover == TRUE) {

      strategy_object$Data <- strategy_object$Data %>% dplyr::group_by(Ticker) %>%
        dplyr::mutate(sig_result = ifelse(sig_result != 0 & sig_result != dplyr::lag(sig_result, n = 1L), sig_result, 0)) %>%
        dplyr::ungroup()

    }

    strategy_object$Data <- strategy_object$Data %>% dplyr::rename(!!signal_name := sig_result)

  return(strategy_object)

}


