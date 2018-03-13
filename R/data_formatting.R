# Wrapper function to convert an xts dataset to a tibble with a Date column
xts_to_tibble <- function(xts_object) suppressWarnings(xts_object %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "Date"))

print.fc_strategy <- function(strat_object) {

  paste("A fluxcapacitor strategy backtest object with", length(strat_object$Data), "securities", sep = " ")

}

# Internal helper function to deal with empty numerics resulting from calls to subset dates that don't exist
if_filled <- function(x) {

  ifelse(length(x) > 0, x, 0)

}

# Internal helper function to find index of nearest filled date in position data
nearest_date <- function(x, i_date){

  as.numeric(x %>% filter(Date <= i_date) %>% select(Date) %>% summarize(Last = n()))

}
