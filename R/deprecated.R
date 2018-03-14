#' Run a backtest on a strategy object
#'
#' This function performs a backtest on a compiled strategy object, and returns the same strategy object with
#' trade data included.
#'
#' Backtesting tracks the number of tests done on data to quantify the risks of overfitting due to multiple testing. If a
#' strategy parameter has been optimized using the \code{\link{optimize}} function prior to backtesting the final strategy,
#' tests should be set to the total number of values in the optimize_range parameter used with that function during that step.
#'
#' @param strategy_object strategy_object  a \code{\link{strategy}} object
#' @param ordersize the number of shares that will be traded per transaction
#' @param use_price the price column to prefer for transactions
#' @param tx_fees transaction fees
#' @param init_equity starting cash value
#' @param prior_tests the number of tests done on the data before backtesting (usually zero.)
#'
#' @return a strategy object
#' @export
#'
#' @examples
backtest_deprecated <- function(strategy_object, ordersize = 100, use_price = "CLOSE", tx_fees = 0, init_equity = 100000, prior_tests = 0) {

  #Sanity Check
  if (!any(class(strategy_object) == "fc_strategy")) stop("backtesting can only be performed on a fluxcapacitor strategy object.")

  #Find first date in strategy data
  start_date <- base::as.Date(min(unlist(purrr::map(strategy_object$Data, ~ min(.$Date)))), origin="1970-01-01")

  #End date in strategy data
  end_date <- base::as.Date(max(unlist(purrr::map(strategy_object$Data, ~ max(.$Date)))), origin="1970-01-01")
  date_sequence <- seq.Date(start_date, end_date - 1, 1) #End on day t-1 to avoid subscript oob when buying on day t

  print("Running Backtest:")

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

  #loop over all dates in date_sequence - for future versions, Rcpp for speed increase?
  for (i in seq_along(date_sequence)){

    #Reset daily aggregate ledger values to zero each day
    ledger_cost <- 0
    ledger_value <- 0

    #loop over all securities in strategy data
    for (j in seq_along(strategy_object$Data)){

      #if a bar exists for day i...
      if (nrow(strategy_object$Data[[j]] %>% filter(Date == date_sequence[i])) > 0){

        #and if a buy is signalled
        if (strategy_object$Data[[j]]$Trade[[which(strategy_object$Data[[j]]$Date == date_sequence[i])]] > 0){

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
                                 Status = "Filled - Buy")

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

        } else if (strategy_object$Data[[j]]$Trade[[which(strategy_object$Data[[j]]$Date == date_sequence[i])]] < 0) {
          # if a sell is signalled

          #Use next bar's price for tx_price
          tx_price <- strategy_object$Data[[j]][[use_price]][[which(strategy_object$Data[[j]]$Date == date_sequence[i]) + 1]]
          proceeds <-  ordersize * tx_price - tx_fees

          #Determine whether position exists to sell
          if (strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Position >= ordersize){

            portfolio_cash <- portfolio_cash + proceeds

            trade_data <- tibble(Trade_ID = nrow(consolidated_orderbook) + 1,
                                 Date = date_sequence[i],
                                 Ticker = names(strategy_object$Data)[j],
                                 Trade_Price = tx_price,
                                 Size = -ordersize,
                                 Cost = -proceeds,
                                 Status = "Filled - Sell")

            #Add transaction row to the consolidated orderbook
            consolidated_orderbook <- consolidated_orderbook %>% rbind(trade_data)

            #Update Ticker Position Table
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Position - ordersize
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Weight <- cost/ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Acct_Val
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Cost <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Cost - proceeds
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position * strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),][[use_price]]

          }

        } else { #If no trade is signalled above, simply update positions table
          if (which(strategy_object$Positions[[j]]$Date == date_sequence[i]) > 1){ #Begin pulling forward data at the second bar
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Position
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Cost <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]) - 1, ]$Cost
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Position * strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]),][[use_price]]
            strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Weight <- strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value/ledger[which(strategy_object$Positions[[j]]$Date == date_sequence[i]),]$Acct_Val

          }
        }

        #Tally Positions

        ledger_cost <- ledger_cost +
          if_filled(strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Cost)

        ledger_value <- ledger_value +
          if_filled(strategy_object$Positions[[j]][which(strategy_object$Positions[[j]]$Date == date_sequence[i]), ]$Value)

      } else if (nrow(strategy_object$Data[[j]] %>% filter(Date == date_sequence[i])) == 0){
        #If day doesn't exist in dataset (i.e. weekends), carry over ledger cost and ledger value from previous bar

        ledger_cost <- ledger_cost +
          if_filled(strategy_object$Positions[[j]][nearest_date(strategy_object$Positions[[j]], date_sequence[i]), ]$Cost)

        ledger_value <- ledger_value +
          if_filled(strategy_object$Positions[[j]][nearest_date(strategy_object$Positions[[j]], date_sequence[i]), ]$Value)

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

  #Check for existing tests in strategy object
  if (length(strategy_object$Tests) > 0) {

    strategy_object$Tests <- strategy_object$Tests + 1 + prior_tests

  } else {

    strategy_object$Tests <- 1 + prior_tests

  }

  #Save orderbook and ledger objects to strategy_object to be returned by function
  strategy_object$orderbook <- consolidated_orderbook
  strategy_object$ledger <- ledger

  writeLines("\n")

  return(strategy_object)

}














