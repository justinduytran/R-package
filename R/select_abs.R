#' Select relevant columns from readABS data
#'
#' Removes all columns from ABS data aside from 'date', 'series', and 'value'. It is useful when the other information provided in the output, e.g. table_no, sheet_no, table_title, frequency, unit, etc, is not required such as when downloading a specific series or table and you know what it contains. Alternatively you're just lazy and don't want to type out the code yourself every time - much like myself when creating this function.
#'
#' @param read_abs_output output of readabs::read_abs function.
#'
#' @return A tibble of read_abs output with only 3 columns: 'date', 'series', and 'value'.
#' @export
#'

select_abs <- function(read_abs_output) {
  abs_data <-
    abs_data[, c("date", "series", "value")]
}
