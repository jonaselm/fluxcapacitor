% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{DBC}
\alias{DBC}
\title{PowerShares DB Commodity Tracking ETF Price Data}
\format{An xts object with 3,088 rows and 5 variables}
\usage{
data(DBC)
}
\description{
A raw xts dataset of PowerShares DB Commodity Tracking ETF (DBC) price data from 2006-03-01 through 2017-12-31. Useful for testing
whether fluxcapacitor backtests perform as expected. This dataset is as-is, pulled from Quandl's ZEP database.
}
\details{
\itemize{
  \item OPEN. opening price for DBC on that day.
  \item HIGH. high price for DBC on that day.
  \item LOW. low price for DBC on that day.
  \item CLOSE. close price for DBC on that day.
  \item VOLUME. trading volume for DBC on that day.
}
}
\keyword{datasets}
