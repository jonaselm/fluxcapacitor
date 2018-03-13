# This file includes the main backtest engine code for fluxcapacitor.
#
#

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
#' @examples
add_indicator <- function(strategy_object, indicator_name, generator, generator_args){

  #Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("add_indicator can only be applied to a strategy object.")

  indicator <- paste(generator, "(", paste(as.character(generator_args), collapse = ", "), ")", sep = "")
  strategy_object$Data <- lapply(strategy_object$Data, function(security){

    security %>% dplyr::mutate(!!indicator_name := eval(parse(text = indicator)))

  })

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
#' @examples
add_signal <- function(strategy_object, signal_name, signal, direction = "buy", crossover = TRUE){

  #Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("add_signal can only be applied to a strategy object.")

  #Add indicator to each security dataset in the strategy universe

  strategy_object$Data <- lapply(strategy_object$Data, function(security){

    sig_result <- security %>% dplyr::transmute(!!signal_name := eval(parse(text = signal)))

    sig_result <- ifelse(sig_result == 0 | is.na(sig_result),0, 1) #Convert to 1s and 0s. NAs are zeroed (no signal)

    if (direction == "sell") sig_result <- -sig_result #if short, use negative position

    if (crossover == TRUE){

      sig_result <- ifelse(sig_result != 0 & dplyr::lag(sig_result, n = 1L) != sig_result, sig_result, 0)
    }

    #Parse the logical argument in the signal string, and put 1/-1 in the signal_name column depending on direction argument
    security[signal_name] <- as.numeric(sig_result)

    security

  })

  return(strategy_object)

}


