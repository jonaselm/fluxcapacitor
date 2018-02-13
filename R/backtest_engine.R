# This file includes the main backtest engine code for fluxcapacitor.
#
#

#' add_indicator
#'
#' Adds an indicator to a security's timeseries data.
#'
#' @param security an object of type tibble contanining security data
#' @param indicator_name the desired column name of the indicator, used to build signals
#' @param generator the function used to generate the indicator value
#' @param generator_args a named list of parameters to use with the generator function
#'
#' @return a tibble of security data with the indicator added as a new column
#' @export
#'
#' @examples
add_indicator <- function(security, indicator_name, generator, generator_args){

  #Ensure that the security object is a tibble before proceeding.
  if(!is_tibble(security)) stop("Price data must be in Tibble format.")

  #load column names into the R search path for evaluation of the signal string
  attach(security)
  security[indicator_name] <- do.call(generator, generator_args)

  return(security)

}


#' add_signal
#'
#' Adds a signal to a security's timeseries data.
#'
#' @param security an object of type tibble containing security data
#' @param signal_name the desired column name of the signal, used to build rules
#' @param signal a logical argument based on the indicator columns of the security object
#'
#' @return a tibble of security data with the signal added as a new column
#' @export
#'
#' @examples
add_signal <- function(security, signal_name, signal){

  #load column names into the R search path for evaluation of the signal string
  attach(security)

  #Parse the logical argument in the signal string, and put TRUE/FALSE in the signal_name column
  security[signal_name] <- eval(parse(text = signal))

  return(security)

}

