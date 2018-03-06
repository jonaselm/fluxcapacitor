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
  if(!any(class(strategy_object) == "fc_strategy")) stop("add_indicator can only be applied to a strategy object.")

  #Add indicator to each security dataset in the strategy universe
  strategy_object$Data <- purrr::map(strategy_object$Data, function(security){

    #load column names into the R search path for evaluation of the signal string
    attach(security)
    security[indicator_name] <- do.call(generator, generator_args)

    #clean up
    detach(security)
    return(security)
  } )


  return(strategy_object)

}


#' Add a signal to your strategy
#'
#' Determines whether a signal has occurred based on an indicator, and adds it to a column in each security's timeseries data.
#'
#' @param strategy_object a \code{\link{strategy}} object
#' @param signal_name the desired column name of the signal, used to build rules
#' @param signal a logical argument based on the indicator columns of the security object
#'
#' @return a strategy object with signal applied to all securities
#' @export
#'
#' @examples
add_signal <- function(strategy_object, signal_name, signal){

  #Sanity Check
  if(!any(class(strategy_object) == "fc_strategy")) stop("add_signal can only be applied to a strategy object.")

  #Add indicator to each security dataset in the strategy universe
  strategy_object$Data <- purrr::map(strategy_object$Data, function(security){

    #load column names into the R search path for evaluation of the signal string
    attach(security)

    #Parse the logical argument in the signal string, and put TRUE/FALSE in the signal_name column
    security[signal_name] <- eval(parse(text = signal))

    #clean up
    detach(security)
    return(security)
  } )

  return(strategy_object)

}

#' Initialize a strategy object
#'
#' The strategy() function creates a strategy object (of S3 class fc_strategy), a container that holds all of the
#' data, rules, and parameters for the strategy being backtested. The function takes just one argument,
#' universe, which must be a character vector of the securities to be included in the strategy.
#'
#' Further, the securities named in universe must exist as objects in the global environment containing
#' the data for the backtest. For instance, a strategy being run on AAPL must have "AAPL" in the universe vector and
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
  if(!is.character(universe)) stop("Universe must be a character vector.")
  if(!all(sapply(universe,exists))) stop("You must have data loaded for each security in universe. See documentation.")

  #Create and format named list of universe data
  strat_data <- lapply(seq_along(universe), FUN = function(ticker) get(universe[ticker]))
  strat_data <- lapply(seq_along(universe), FUN = function(ticker){
    #Make sure data is in a usable format
    if(!tibble::is_tibble(strat_data[[ticker]]) & !xts::is.xts(strat_data[[ticker]])) stop("Price data must be in Tibble or xts format.")

    #Convert any xts data to tibble w/ date column
    if(xts::is.xts(strat_data[[ticker]])) return(xts_to_tibble(strat_data[[ticker]]))

    #Otherwise, simply get tibble
    if(tibble::is.tibble(strat_data[[ticker]])) return(strat_data[[ticker]])
  })


  strat_object <- list( Universe = universe,
                        Data = set_names(strat_data, universe)
                        )

  class(strat_object) <- append("fc_strategy", class(strat_object))
  return(strat_object)

}



