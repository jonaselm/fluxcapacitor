% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backtest_engine.R
\name{backtest}
\alias{backtest}
\title{Run a backtest on a strategy object}
\usage{
backtest(strategy_object, ordersize = 100, use_price = "CLOSE",
  tx_fees = 0, init_equity = 1e+05, prior_tests = 0, progress = TRUE)
}
\arguments{
\item{strategy_object}{strategy_object  a strategy object}

\item{ordersize}{the number of shares that will be traded per transaction}

\item{use_price}{the price column to prefer for transactions}

\item{tx_fees}{transaction fees}

\item{init_equity}{starting cash value}

\item{prior_tests}{the number of tests done on the data before backtesting (usually zero.)}

\item{progress}{TRUE/FALSE. Specifies whether a progress bar should be displayed in the console during backtest.}
}
\value{
a strategy object
}
\description{
This function performs a backtest on a compiled strategy object, and returns the same strategy object with
trade data included.
}
\details{
Backtesting tracks the number of tests done on data to quantify the risks of overfitting due to multiple testing. If a
strategy parameter has been optimized using the \code{\link{optimize}} function prior to backtesting the final strategy,
tests should be set to the total number of values in the optimize_range parameter used with that function during that step.
}
