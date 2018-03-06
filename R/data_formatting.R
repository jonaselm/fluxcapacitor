#Add ticker to stock data object
add_ticker <- function(x, ticker){
  ticker <- as.character(match.call()$x)
  x$Ticker <- ticker
  return(x)
}

# Wrapper function to convert an xts dataset to a tibble with a Date column
xts_to_tibble <- function(xts_object) suppressWarnings(xts_object %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "Date"))


print.fc_strategy <- function(strat_object){

  paste("A fluxcapacitor strategy backtest object with", length(strat_object$Data), "securities", sep = " ")

}

build_universe <- function(ticker){
  #Make sure data is in a usable format
  if(!tibble::is_tibble(get(universe[ticker])) & !xts::is.xts(get(universe[ticker]))) stop("Price data must be in Tibble format.")

  #Convert any xts data to tibble w/ date column
  if(xts::is.xts(get(universe[ticker]))) xts_to_tibble(get(universe[ticker]))

  #Otherwise, simply get tibble
  if(tibble::tibble(get(universe[ticker]))) get(universe[ticker])
}
