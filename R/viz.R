chart_equity_curve <- function(strategy_object){

  strategy_object$ledger %>% ggplot2::ggplot(aes(x = Date, y = Acct_Val)) + geom_line() +
    labs(x = "Date", y = "Equity Curve", title = paste("Equity Curve for", substitute(strategy_object), sep = " ")) + theme_bw()

}

