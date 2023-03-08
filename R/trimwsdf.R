#' Removes leading and trailing whitespace from a dataframe
#'
#' trimws only removes leading and trailing whitespace from a vector. trimwsdf is an extension that will remove leading and trailing whitespace in all columns in a dataframe. One day I'll build in the same arguments from trimws into trimwsdf. Function created by MarkusN here: https://stackoverflow.com/a/37815274.
#'
#' @param df dataframe
#'
#' @return dataframe
#' @export
#'

trimwsdf <- function(df) {
  data.frame(lapply(df, function(x)
    if (class(x) == "character" | class(x) == "factor")
      trimws(x)
    else
      (x)),
    stringsAsFactors = F)
}

