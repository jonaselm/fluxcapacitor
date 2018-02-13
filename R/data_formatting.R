#Add ticker to stock data object
add_ticker <- function(x){
  ticker <- as.character(match.call()$x)
  x$"Ticker" <- ticker
  return(x)
}

# Wrapper function to convert an xts dataset to a tibble with a Date column
xts_to_tibble <- function(xts_object) xts_object %>% add_ticker %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "Date")


print.fc_strategy <- function(strat_object){

  paste("A fluxcapacitor strategy backtest object with", length(strat_object$Data), "securities", sep = " ")

}
