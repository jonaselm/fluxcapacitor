chart_equity_curve <- function(strategy_object){

  strategy_object$ledger %>% ggplot2::ggplot(aes(x = Date, y = Acct_Val)) + geom_line()

}

