# Wrapper function to convert an xts dataset to a tibble with a Date column
xts_to_tibble <- function(xts_object) suppressWarnings(xts_object %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "Date"))

