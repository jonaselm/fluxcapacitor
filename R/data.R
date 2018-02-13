#' S&P 500 Price Data for 2017
#'
#' A raw xts dataset of S&P 500 price data from 2017-01-02 through 2018-01-02. Useful for testing
#' whether fluxcapacitor backtests perform as expected. This dataset is as-is, pulled from Quandl's ZEP database.
#'
#' \itemize{
#'   \item OPEN. opening price for the S&P 500 on that day.
#'   \item HIGH. high price for the S&P 500 on that day.
#'   \item LOW. low price for the S&P 500 on that day.
#'   \item CLOSE. diamond colour, from J (worst) to D (best)
#'   \item VOLUME. NA for this particular security
#' }
#'
#' @docType data
#' @keywords datasets
#' @name SPX
#' @usage data(SPX)
#' @format An xts object with 261 rows and 5 variables
NULL
