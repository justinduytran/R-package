#' Append .csv to string
#'
#' @param x string. Usually intended to be the file name
#'
#' @returns The original string (x) with ".csv" appended.
#' @export
#'
#' @examples
#' csv_suffix(filename)

csv_suffix <- function(x) {
  paste0(x,".csv")
}
