# This file includes the main backtest engine code for fluxcapacitor.
#
#

#' add_indicator
#'
#' @param security an object of type tibble contanining security data
#' @param indicator_name the desired column name of the indicator, used to identify signals
#' @param generator_fn the function used to generate the indicator value
#' @param generator_args a named list of parameters to use with the generator function
#'
#' @return a tibble of security data with the indicator added as a new column
#' @export
#'
#' @examples
add_indicator <- function(security, indicator_name, generator_fn, generator_args){

  if(!is_tibble(security)) stop("Price data must be in Tibble format.")

  security[indicator_name] <- do.call(generator_fn, generator_args)

  return(security)

}

