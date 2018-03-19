#' Convert an xts to tidy data
#'
#' This is an internal helper utility
#'
#' @param xts_object an xts object
#'
#' @importFrom magrittr %>%
#'
xts_to_tibble <- function(xts_object) suppressWarnings(xts_object %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "Date"))

