#' Chart a strategy's equity curve
#'
#' From a backtested strategy object, generate a plot of the equity curve
#'
#' @param strategy_object a strategy object
#'
#' @return a ggplot2 object
#' @export
#' @importFrom magrittr %>%
#'
chart_equity_curve <- function(strategy_object){

  strategy_object$ledger %>% ggplot2::ggplot(aes(x = Date, y = Acct_Val)) + ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "Equity Curve", title = paste("Equity Curve for", substitute(strategy_object), sep = " ")) +
    ggplot2::theme_bw()

}

#' Chart a position
#'
#' Generate a price chart and position plot for a given ticker from the strategy universe.
#'
#' @param strategy_object a strategy object
#' @param ticker the ticker symbol to chart
#' @param use_price price column to chart
#'
#' @return a ggplot2 object
#' @export
#' @importFrom magrittr %>%
#'
chart_positions <- function(strategy_object, ticker, use_price = "CLOSE"){

  security <- strategy_object$Data %>% dplyr::filter(Ticker == ticker) %>%
    dplyr::select(Date, use_price, Position) %>% reshape2::melt(id.vars = "Date")

  security %>% ggplot2::ggplot() + ggplot2::geom_line(aes(x = Date, y = value)) +
    ggplot2::facet_grid(variable ~ ., scales = "free") + ggplot2::theme_bw()

}

#' Chart trading signals
#'
#' Generate a price chart with overlaid buy and sell signals for a given ticker from the strategy universe.
#'
#' @param strategy_object a strategy object
#' @param ticker the ticker symbol to chart
#' @param use_price price column to chart
#'
#' @return a ggplot2 object
#' @export
#' @importFrom magrittr %>%
#'
chart_signals <- function(strategy_object, ticker, use_price = "CLOSE"){

  security <- strategy_object$Data %>% dplyr::filter(Ticker == ticker) %>%
    dplyr::select(Date, use_price, Trade) %>% dplyr::mutate(Signals = ifelse(Trade == 1, "B", Trade)) %>%
    dplyr::mutate(Signals = ifelse(Signals == -1, "S", Signals)) %>%
    dplyr::mutate(Signals = ifelse(Signals == 0, NA, Signals))

  security %>% ggplot2::ggplot(aes(x = Date, y = eval(parse(text = use_price)),label = Signals)) + ggplot2::geom_line() +
    ggplot2::geom_text(aes(colour = Signals), nudge_y = -3, na.rm=TRUE) +
    ggplot2::labs(x = "Date", y = use_price, title = paste("Signals for", ticker, sep = " ")) +
    ggplot2::theme_bw() + ggplot2::scale_colour_manual(values=c("#007F06", "#CC0900")) + guides(colour = "none")

}

#' Chart the results of a strategy optimization
#'
#' Generates a chart of different tested parameter values versus the equity those parameters produced. Maximum
#' equity is annotated with an orange dot.
#'
#' @param strategy_object a strategy object that's been produced by the optimize_strategy() function
#'
#' @return a ggplot2 object
#' @export
#' @importFrom magrittr %>%
#'
chart_optimizer <- function(strategy_object){

  opt_data <- tibble(Parameter = strategy_object$Optimized$Parameters,
                     Equity = strategy_object$Optimized$Equity)

  opt_data %>% ggplot2::ggplot(aes(x = Parameter, y = Equity)) + ggplot2::geom_line() +
    ggplot2::geom_point(aes(x = Parameter[which.max(Equity)], y = Equity[which.max(Equity)]),
                        size = 3, color = "orange") + ggplot2::theme_bw() +
    ggplot2::labs(title = "Optimization Results (In-Sample)")

}
