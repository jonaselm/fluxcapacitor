#' fluxcapacitor: A tidyverse-friendly quantitative backtesting engine.
#'
#' `fluxcapacitor` is a quantitative backtesting engine that plays nicely within the `tidyverse`.
#' The package gets its name from the Back to the Future series of films -- like the flux capacitor in
#' Doc Brown's DeLorean, this package aims to facilitate "time travel", allowing financial researchers to
#' simulate the results of trading strategies.
#'
#' @details
#'  To learn more about tidyquant, start with the vignettes:
#'  `vignette("Report", package = "fluxcapacitor")`
#'
#' @docType package
#' @name fluxcapacitor
#'
#' @import tidyverse
#' @importFrom rlang := !!
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom timetk tk_tbl
#' @importFrom stats sd

# Create global variables for dplyr columns used throughout the package
utils::globalVariables(c(".",
                         "Acct_Val",
                         "Cash",
                         "Cost",
                         "Date",
                         "Drawdowns",
                         "Equity",
                         "Excess_Returns",
                         "Parameter",
                         "Returns",
                         "Signals",
                         "Ticker",
                         "Trade",
                         "Tx",
                         "Val",
                         "aes",
                         "arrange",
                         "benchmark",
                         "guides",
                         "last",
                         "sig_result",
                         "summarize",
                         "tibble",
                         "ticker",
                         "universe",
                         "value"))
