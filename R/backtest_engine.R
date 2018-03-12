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
  if (!is.character(universe)) stop("Universe must be a character vector.")
  if (!all(sapply(universe,exists))) stop("You must have data loaded for each security in universe. See documentation.")

  #Create and format named list of universe data
  strat_data <- lapply(seq_along(universe), FUN = function(ticker) get(universe[ticker]))
  strat_data <- lapply(seq_along(universe), FUN = function(ticker){

    #Make sure data is in a usable format
    if (!tibble::is_tibble(strat_data[[ticker]]) & !xts::is.xts(strat_data[[ticker]])) stop("Price data must be in Tibble or xts format.")

    #Convert any xts data to tibble w/ date column
    if (xts::is.xts(strat_data[[ticker]])) return(xts_to_tibble(strat_data[[ticker]]))

    #Otherwise, simply get tibble
    if (tibble::is.tibble(strat_data[[ticker]])) return(strat_data[[ticker]])
  })

  strat_object <- list( Universe = universe,
                        Data = set_names(strat_data, universe)
                  )

  class(strat_object) <- append("fc_strategy", class(strat_object))
  return(strat_object)

}

backtest <- function(strategy_object, signals, ordersize = 100, use_price = "CLOSE", tx_fees = 0, init_equity = 100000){

  #Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("backtesting can only be performed on a fluxcapacitor strategy object.")

  #Find first date in strategy data
  start_date <- base::as.Date(min(unlist(purrr::map(strategy_object$Data, ~ min(.$Date)))), origin="1970-01-01")

  #End date in strategy data
  end_date <- base::as.Date(max(unlist(purrr::map(strategy_object$Data, ~ max(.$Date)))), origin="1970-01-01")
  date_sequence <- seq.Date(start_date, end_date, 1)

  #Create Progress Bar
  pb <- txtProgressBar(style = 3)

  #Initialize orderbook and ledger
  consolidated_orderbook <- tibble()
  ledger <- tibble(Date = start_date-1, Description = "Strategy Initialized", Cash = init_equity, Cost = 0, Equities = 0, Acct_Val = init_equity)

  #Initialize Date, Price, Position, Weight, Cost, and Value for each security orderbook
  strategy_object$Positions <- lapply(strategy_object$Data, function(security){

    security %>% select(Date, !!use_price) %>% mutate(Position = 0, Weight = 0, Cost = 0, Value = 0)

  })

  portfolio_cash <- init_equity
  Acct_Val <- init_equity


  #loop over all dates in date_sequence - for future versions, Rcpp vs. parallel for speed increase?
  for (i in seq_along(date_sequence)){

    #Reset daily aggregate ledger values to zero each day
    ledger_cost <- 0
    ledger_value <- 0

    #loop over all securities in strategy data
    for (j in seq_along(strategy_object$Data)){

      #if a bar exists for day i...
      if (nrow(strategy_object$Data[[j]] %>% filter(Date == date_sequence[i])) > 0){

        #and if a trade is signalled
        if (strategy_object$Data[[j]][[signals]][[which(strategy_object$Data[[j]]$Date == date_sequence[i])]] != 0){

          #Use next bar's price for tx_price
          tx_price <- strategy_object$Data[[j]][[use_price]][[which(strategy_object$Data[[j]]$Date == date_sequence[i]) + 1]]
          cost <-  ordersize * tx_price + tx_fees

          #Determine whether cash is sufficient for trade
          if (ledger$Cash[nrow(ledger)] >= cost){

            portfolio_cash <- portfolio_cash - cost

            trade_data <- tibble(Trade_ID = nrow(consolidated_orderbook) + 1,
                                 Date = date_sequence[i],
                                 Ticker = names(strategy_object$Data)[j],
                                 Trade_Price = tx_price,
                                 Size = ordersize,
                                 Cost = cost,
                                 Status = "Filled")

            #Update Ticker Position Table
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Position + ordersize
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Weight <- cost/ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Acct_Val
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Cost <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Cost + cost
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position * strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),][[use_price]]

          } else { #Cash is insufficient to place the trade

            trade_data <- tibble(Trade_ID = nrow(consolidated_orderbook) + 1,
                                 Date = date_sequence[i],
                                 Ticker = names(strategy_object$Data)[j],
                                 Trade_Price = tx_price,
                                 Size = ordersize,
                                 Cost = cost,
                                 Status = "Cancelled -- Cash Insufficient")

            if (which(strategy_object$Positions[[j]]$Date == date_sequence[i]) > 1){ #Begin pulling forward data at the second bar
              strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Position
              strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Cost <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Cost
              strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position * strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),][[use_price]]
              strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Weight <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value/ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]),]$Acct_Val

            }


          }

          #Add transaction row to the consolidated orderbook
          consolidated_orderbook <- consolidated_orderbook %>% rbind(trade_data)

        } else { #If no trade is signalled above, simply update positions table
          if (which(strategy_object$Positions[[j]]$Date == date_sequence[i]) > 1){ #Begin pulling forward data at the second bar
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Position
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Cost <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Cost
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position * strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),][[use_price]]
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Weight <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value/ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]),]$Acct_Val

          }
        }

        #Tally Positions

        ledger_cost <- ledger_cost + strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),]$Cost
        ledger_value <- ledger_value + strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),]$Value

      } else if (nrow(strategy_object$Data[[j]] %>% filter(Date == date_sequence[i])) == 0){
        #If day doesn't exist in dataset (i.e. weekends), carry over ledger cost and ledger value from previous bar

        ledger_cost <- ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Cost
        ledger_value <-  ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Equities


      }

    }

    #Add position tallies as ledger entry
    ledger_entry <- tibble(Date = date_sequence[i], Description = "",
                           Cash = portfolio_cash,
                           Cost = ledger_cost, Equities = ledger_value,
                           Acct_Val = ledger_value + portfolio_cash)

    ledger <- ledger %>% rbind(ledger_entry)

    #Update progress bar
    setTxtProgressBar(pb, value = i/length(date_sequence))

  }

  #Save orderbook and ledger objects to strategy_object to be returned by function
  strategy_object$orderbook <- consolidated_orderbook
  strategy_object$ledger <- ledger

  return(strategy_object)

}














