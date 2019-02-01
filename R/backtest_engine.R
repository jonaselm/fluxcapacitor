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
#' @return a strategy object
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' data(SPY)
#' strat <- init_strategy("SPY")
#'
init_strategy <- function(universe, data = FALSE) {

  if (data == FALSE) {
    if (!is.character(universe)) stop("Universe must be a character vector.")
    if (!all(sapply(universe, base::exists))) stop("You must have data loaded for each security in universe. See documentation.")

    # Create and format named list of universe data
    strat_data <- lapply(universe, FUN = function(x) {

      stock <- get(noquote(x))

      # Make sure data is in a usable format
      if (!tibble::is_tibble(stock) & !xts::is.xts(stock)) stop("Price data must be in Tibble or xts format.")
      if (!xts::is.xts(stock) & !("Date" %in% colnames(stock))) stop(paste("No 'Date' column found in", ticker, "security.", sep = " "))
      if (!xts::is.xts(stock) & !("Ticker" %in% colnames(stock))) stop(paste("No 'Ticker' column found in", ticker, "security.", sep = " "))


      # Convert any xts data to tibble w/ date column
      if (xts::is.xts(stock)) {
        stock_tibble <- xts_to_tibble(stock)
        stock_tibble <- stock_tibble %>% dplyr::mutate(Ticker = x)

        return(stock_tibble)
      }

      # Otherwise, simply get tibble
      if (tibble::is.tibble(stock)) return(stock)
    })

    # Merge data into a single tibble
    strat_data <- do.call(base::rbind, strat_data)

  } else if (data == TRUE){

    strat_data <- universe

    if (!("Date" %in% colnames(strat_data))) stop("No 'Date' column found in universe.")
    if (!("Ticker" %in% colnames(strat_data))) stop("No 'Ticker' column found in universe.")

    universe <- strat_data %>% dplyr::select(Ticker) %>% base::unique() %>% dplyr::pull()

  }

  strat_data <- strat_data %>% arrange(Date)

  strat_object <- list( Universe = universe,
                        Data = strat_data
                  )

  class(strat_object) <- base::append("fc_strategy", class(strat_object))
  return(strat_object)

}

#' Compile a strategy for backtesting
#'
#' Determines buy and sell trades based on a strategy's signals.
#'
#' @param strategy_object  a strategy object
#' @param signals a list of signal columns
#'
#' @return a strategy object
#' @export
#' @importFrom magrittr %>%
#'
compile_strategy <- function(strategy_object, signals) {

  # Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("backtesting can only be performed on a fluxcapacitor strategy object.")

  trades <- strategy_object$Data %>% dplyr::select(signals) %>% dplyr::transmute(Trade = rowSums(.))
  strategy_object$Data <- strategy_object$Data %>% base::cbind(trades)

  return(strategy_object)
}

#' Run a backtest on a strategy object
#'
#' This function performs a backtest on a compiled strategy object, and returns the same strategy object with
#' trade data included.
#'
#' Backtesting tracks the number of tests done on data to quantify the risks of overfitting due to multiple testing. If a
#' strategy parameter has been optimized using the \code{\link{optimize}} function prior to backtesting the final strategy,
#' tests should be set to the total number of values in the optimize_range parameter used with that function during that step.
#'
#' @param strategy_object strategy_object  a strategy object
#' @param ordersize the number of shares that will be traded per transaction
#' @param use_price the price column to prefer for transactions
#' @param tx_fees transaction fees
#' @param init_equity starting cash value
#' @param prior_tests the number of tests done on the data before backtesting (usually zero.)
#' @param progress TRUE/FALSE. Specifies whether a progress bar should be displayed in the console during backtest.
#'
#' @return a strategy object
#' @export
#' @importFrom magrittr %>%
#'
backtest <- function(strategy_object, ordersize = 100, use_price = "CLOSE", tx_fees = 0, init_equity = 100000, prior_tests = 0,
                     progress = TRUE) {

  # Initialize Progress Bar
  if (progress == TRUE) {
    cat("Backtesting Strategy: \n")
    pb <- txtProgressBar(style = 3)
  }

  # Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("backtesting can only be performed on a fluxcapacitor strategy object.")

  cash <- c(init_equity, rep(NA, nrow(strategy_object$Data) -1))

    bt <- strategy_object$Data %>% dplyr::group_by(Ticker) %>%
      dplyr::mutate(Cost = Trade * ordersize * dplyr::lead(eval(parse(text = use_price)))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Cost = ifelse(is.na(Cost), 0, Cost), Cash = cash, Filled = NA, Tx = 0)

    # Loop over transactions to update cash, positions, etc.
    universe <- strategy_object$Universe
    positions <- rep(0, length(universe))
    names(positions) <- universe

    for (i in 2:(nrow(bt)-1)) {
      # If a buy is signalled
      if (bt[["Trade"]][[i]] > 0){

        # Check for sufficient cash
        if (bt[["Cash"]][[i-1]] > bt[["Cost"]][[i]]){
          bt[["Cash"]][[i]] <- bt[["Cash"]][[i-1]] - bt[["Cost"]][[i]]
          positions[[bt[["Ticker"]][[i]]]] <- positions[[bt[["Ticker"]][[i]]]] + ordersize
          bt[["Filled"]][[i]] <- TRUE
          bt[["Tx"]][[i]] <- bt[["Trade"]][[i]] * ordersize

        } else {
          # If cash is insufficient
          bt[["Cash"]][[i]] <- bt[["Cash"]][[i-1]]
          bt[["Filled"]][[i]] <- FALSE
          bt[["Tx"]][[i]] <- 0
        }
      } else if (bt[["Trade"]][[i]] < 0) {# if a sell is signalled

        # And a position exists to sell
        if(positions[[bt[["Ticker"]][[i]]]] >= ordersize){
          bt[["Cash"]][[i]] <- bt[["Cash"]][[i-1]] - bt[["Cost"]][[i]]
          positions[[bt[["Ticker"]][[i]]]] <- positions[[bt[["Ticker"]][[i]]]] - ordersize
          bt[["Filled"]][[i]] <- TRUE
          bt[["Tx"]][[i]] <- bt[["Trade"]][[i]] * ordersize

        } else {
          # If no position to sell
          bt[["Cash"]][[i]] <- bt[["Cash"]][[i-1]]
          bt[["Filled"]][[i]] <- FALSE
          bt[["Tx"]][[i]] <- 0
        }
      } else {
        # No trade
        bt[["Cash"]][[i]] <- bt[["Cash"]][[i-1]]
        bt[["Filled"]][[i]] <- FALSE
        bt[["Tx"]][[i]] <- 0
      }

      if (progress == TRUE) setTxtProgressBar(pb, i/(nrow(bt)-2))
    }

  # Calculate values and remove last date (because trades cannot be evaluated without lag = 1)
  strategy_object$Data <- bt %>% dplyr::group_by(Ticker) %>% dplyr::mutate(Position = cumsum(Tx)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Val = Position * eval(parse(text = use_price))) %>% dplyr::filter(Date != max(Date))

  # Save ledger
  strategy_object$ledger <- strategy_object$Data %>% dplyr::group_by(Date) %>%
    dplyr::summarise(Equity = sum(Val), Cash = last(Cash), Acct_Val = Cash + Equity)

  # Track tests for overfitting
  if (length(strategy_object$Tests) > 0) {
    strategy_object$Tests <- strategy_object$Tests + 1
  } else {
    strategy_object$Tests <- 1
  }

  # Close progress bar
  if (progress == TRUE) close(pb)

  return(strategy_object)

}



backtest_equal <- function(strategy_object, use_price = "CLOSE", tx_fees = 0, init_equity = 100000, prior_tests = 0,
                           progress = TRUE) {

  # Initialize Progress Bar
  if (progress == TRUE) {
    cat("Backtesting Strategy: \n")
    pb <- txtProgressBar(style = 3)
  }

  count_trades <- function(df) {
    sum(df$Trade != 0)
  }

  # Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("backtesting can only be performed on a fluxcapacitor strategy object.")

  bt <- strategy_object$Data %>% dplyr::group_by(Ticker) %>% dplyr::mutate(val = 0, cost = 0, tx = 0, position = 0,
                                                                           next_price = dplyr::lead(eval(parse(text = use_price))))

  # Collapse trades by date as a list-column, add equity column to parent tibble, and count trades per day
  bt <- bt %>% group_by(Date) %>% tidyr::nest(.key = "securities") %>%
    dplyr::mutate(equity = init_equity, n_trades = purrr::map_int(securities, count_trades))

  # Loop over days to update cash, positions, etc. -- all actions within "securities" list-col can be vectorized
  universe <- strategy_object$Universe
  positions <- rep(0, length(universe))
  names(positions) <- universe

  for (i in 2:(nrow(bt) - 1)) {

    # If no trades
    if (bt[["n_trades"]][[i]] == 0){
      if(sum(bt$securities[[i-1]][["position"]]) == 0) { # if no positions added yesterday, just pull equity from prior day

        bt$equity[[i]] <- bt$equity[[i-1]]

      } else { # if we have positions on that day, update equity based on current day's price (vectorized)

        # find where current security lives in the position object and update it
        matching_securities <- which(names(positions) %in% bt$securities[[i]][["Ticker"]])
        bt$securities[[i]][["position"]] <- positions[matching_securities]

        # Calculate today's equity
        bt$securities[[i]][["val"]] <- bt$securities[[i]][["position"]] * bt$securities[[i]][[use_price]]

        bt$equity[[i]] <- sum(bt$securities[[i]][["val"]])

      }

    } else if (bt[["n_trades"]][[i]] > 0) { # If a trade is signalled (we don't know if it's a buy or sell)

      prior_equity <- bt$equity[[i-1]] # pull yesterday's equity value

      # find where the current security lives in the position object and update it
      matching_securities <- which(names(positions) %in% bt$securities[[i]][["Ticker"]])
      bt$securities[[i]][["position"]] <- positions[matching_securities]

      # If a sell, deduct trading costs, zero out position and update the position object, and add transaction
      sell_costs <- tx_fees * sum(bt$securities[[i]][["Trade"]] < 0)
      prior_equity <- prior_equity - sell_costs

      bt$securities[[i]][["tx"]][which(bt$securities[[i]][["Trade"]] < 0)] <- -bt$securities[[i]][["position"]][which(bt$securities[[i]][["Trade"]] < 0)]
      bt$securities[[i]][["position"]][which(bt$securities[[i]][["Trade"]] < 0)] <- 0
      positions[bt$securities[[i]][["Ticker"]][which(bt$securities[[i]][["Trade"]] < 0)]] <- 0


      # If a buy, give it an equal share of prior_equity, and update positions, transaction, and position object
      buy_costs <- tx_fees * bt[["n_trades"]][[i]]

      bt$securities[[i]][["val"]][which(bt$securities[[i]][["Trade"]] > 0)] <- (prior_equity - buy_costs)/bt[["n_trades"]][[i]]
      bt$securities[[i]][["position"]][which(bt$securities[[i]][["Trade"]] > 0)] <- bt$securities[[i]][["val"]][which(bt$securities[[i]][["Trade"]] > 0)] / bt$securities[[i]][["next_price"]][which(bt$securities[[i]][["Trade"]] > 0)]
      bt$securities[[i]][["tx"]][which(bt$securities[[i]][["Trade"]] > 0)] <- bt$securities[[i]][["val"]][which(bt$securities[[i]][["Trade"]] > 0)] / bt$securities[[i]][["next_price"]][which(bt$securities[[i]][["Trade"]] > 0)]
      positions[bt$securities[[i]][["Ticker"]][which(bt$securities[[i]][["Trade"]] > 0)]] <- bt$securities[[i]][["position"]][which(bt$securities[[i]][["Trade"]] > 0)]

      # Update equity
      if(all(positions == 0)) bt$equity[[i]] <- prior_equity # If we sold out the portfolio, update equity to yesterday's minus tx costs
      if(all(positions != 0)) bt$equity[[i]] <- sum(bt$securities[[i]][["val"]]) # Otherwise, sum remaining positions

    }

    if (progress == TRUE) setTxtProgressBar(pb, i/(nrow(bt)-2))

  }

  # Save ledger
  strategy_object$ledger <- bt %>% dplyr::group_by(Date) %>%
    dplyr::summarise(Acct_Val = equity) %>% dplyr::filter(Date != max(Date))

  # Unnest list-columns
  strategy_object$Data <- bt %>% tidyr::unnest() %>% dplyr::filter(Date != max(Date))



  # Track tests for overfitting
  if (length(strategy_object$Tests) > 0) {
    strategy_object$Tests <- strategy_object$Tests + 1
  } else {
    strategy_object$Tests <- 1
  }

  # Close progress bar
  if (progress == TRUE) close(pb)

  return(strategy_object)

}










