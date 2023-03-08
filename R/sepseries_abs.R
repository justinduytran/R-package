#' Separate 'series' column into separate columns
#'
#' Separates the 'series' column in ABS data into it's constituent parts by ';'. Useful when importing a table intending to use several series and multiple useful descriptors are in the series field i.e. "Estimated Resident Population ;  |SEX| ;  |AGE| ;". One day I'll build in a column renaming argument.
#'
#' @param read_abs_output output of readabs::read_abs function - ideally after justinduytran::select_abs.
#' @param lastcol boolean to include the last column or not. Default FALSE. TRUE includes the last column which is usually just a space given the ABS likes to end their series descriptions with "; " and is not recommended.
#'
#' @return A tibble of read_abs output but with the 'series' column separated into component columns.
#' @export
#'

sepseries_abs <- function(read_abs_output, lastcol = FALSE) {
  # Separate series col using ;
  sepseries <- read.table(
    text = read_abs_output$series,
    sep = ";",
    col.names = paste0("series",
                       c(1:length(
                         read.table(text = read_abs_output$series, sep = ";")
                       )))
  ) |>
  # Remove white space
    justinduytran::trimwsdf()

  # Remove last column
  if (!isTRUE(lastcol)) {
    sepseries <- sepseries[, -length(sepseries)]
  }

  read_abs_output <- cbind(date = read_abs_output$date,
                   sepseries,
                   value = read_abs_output$value)

  return(read_abs_output)
}
