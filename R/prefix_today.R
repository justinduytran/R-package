#' Prefix today's date to string
#' 
#' Prefix today's date in %Y%m%d format to a string
#'
#' @param x string
#'
#' @returns A string in the format of "%Y%m%d x"
#' @export
#'
#' @examples
#' prefix_today("filename.csv")

prefix_today <- function(x) {
  paste(Sys.time() |> format("%Y%m%d"), x)
}