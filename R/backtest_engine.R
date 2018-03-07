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
#' Determines whether a signal has occurred based on one or more columns in the strategy data, and adds it to a column in each security's timeseries data.
#'
#' @param strategy_object a \code{\link{strategy}} object
#' @param signal_name the desired column name of the signal, used to build rules
#' @param signal a logical argument based on the indicator columns of the security object
#' @param direction the direction of the market bet, long or short
#' @param crossover defines whether signal should be a crossover signal. If TRUE, the signal will only occur on first bar of each run of signal condition being met.
#'
#' @return a strategy object with signal applied to all securities
#' @export
#'
#' @examples
add_signal <- function(strategy_object, signal_name, signal, direction = "long", crossover = TRUE){

  #Sanity Check
  if(!any(class(strategy_object) == "fc_strategy")) stop("add_signal can only be applied to a strategy object.")

  #Add indicator to each security dataset in the strategy universe
  strategy_object$Data <- purrr::map(strategy_object$Data, function(security){

    #load column names into the R search path for evaluation of the signal string
    attach(security)

    sig_result <- eval(parse(text = signal))
    sig_result <- ifelse(sig_result != 0 | is.na(sig_result),0, 1) #Convert to 1s and 0s. NAs are zeroed (no signal)
    if(direction == "short") sig_result <- -sig_result #if short, use negative position

    if(crossover == TRUE){

    sig_result <- ifelse(sig_result != 0 & dplyr::lag(sig_result, n = 1L) != sig_result, sig_result, 0)

    }

    #Parse the logical argument in the signal string, and put 1/-1 in the signal_name column depending on direction argument
    security[signal_name] <- sig_result

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

backtest <- function(strategy_object, signals, ordersize = 100, use_price = "CLOSE", tx_fees = 0){

  #Sanity Check
  if(!any(class(strategy_object) == "fc_strategy")) stop("backtesting can only be performed on a fluxcapacitor strategy object.")

  #Find first date in strategy data
  start_date <- base::as.Date(min(unlist(purrr::map(strategy_object$Data, ~ min(.$Date)))), origin="1970-01-01")

  #End date in strategy data
  end_date <- base::as.Date(max(unlist(purrr::map(strategy_object$Data, ~ max(.$Date)))), origin="1970-01-01")
  date_sequence <- seq.Date(start_date, end_date, 1)

  #Add orderbook to strategy object
  orderbook <- tibble()
  ledger <- tibble(Date = date_sequence)

  #loop over all dates in date_sequence - for future versions, Rcpp vs. parallel for speed increase?
  for(i in seq_along(date_sequence)){
    #loop over all securities in strategy data
    for(j in seq_along(strategy_object$Data)){
      #if the date exists and a trade is signaled, apply trade logic
      if(nrow(strategy_object$Data[[j]] %>% filter(Date == date_sequence[i])) > 0){
        if(strategy_object$Data[[j]][[signals]][[which(strategy_object$Data[[j]]$Date == date_sequence[i])]] != 0){

          #Use next bar's price for tx_price
          tx_price <- strategy_object$Data[[j]][[use_price]][[which(strategy_object$Data[[j]]$Date == date_sequence[i])+1]]

          trade_data <- tibble(Trade_ID = nrow(orderbook) + 1,
                               Date = date_sequence[i],
                               Ticker = names(strategy_object$Data)[j],
                               Trade_Price = tx_price,
                               Size = ordersize,
                               Cost = ordersize * tx_price + tx_fees)

          orderbook <- orderbook %>% rbind(trade_data)

        }

      }

    }
  }

  strategy_object$orderbook <- orderbook

  return(strategy_object)

}














