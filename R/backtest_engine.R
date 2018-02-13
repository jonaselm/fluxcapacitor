# This file includes the main backtest engine code for fluxcapacitor.
#
#

#' Add an indicator to your strategy
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
  if(!tibble::is_tibble(security)) stop("Price data must be in Tibble format.")

  #load column names into the R search path for evaluation of the signal string
  attach(security)
  security[indicator_name] <- do.call(generator, generator_args)

  return(security)

}


#' Add a signal to your strategy
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

#' Initialize a strategy object
#'
#' The strategy() function creates a strategy object (of S3 class fc_strategy), which will hold all of the
#' data, rules, and parameters for the strategy being backtested. The function takes just one argument,
#' universe, which must be a character vector of the securities to be included in the strategy.
#'
#' Further, the securities named in universe must exist as objects in the global environment containing
#' the data for the backtest. For instance, a strategy being run in AAPL must have AAPL in the universe and
#' must have an object named AAPL that contains the stock price data for AAPL.
#'
#' @param universe a character vector of the securities to be included in the strategy.
#'
#' @return
#' @export
#'
#' @examples
#' data(SPX)
#' SPX <- SPX %>% xts_to_tibble()
#' strat <- strategy("SPX")
#'
strategy <- function(universe){

  if(!all(sapply(universe,exists))) stop("Error: You must have data loaded for each security in universe. See documentation.")

  strat_object <- list( Universe = universe,
                        Data = lapply(1:length(universe), FUN = function(ticker) get(universe[ticker]))
                        )

  names(strat_object$Data) <- universe
  class(strat_object) <- append("fc_strategy", class(strat_object))
  return(strat_object)

}

print.fc_strategy <- function(strat_object){

  paste("A fluxcapacitor strategy backtest object with", length(strat_object$Data), "securities", sep = " ")

}

