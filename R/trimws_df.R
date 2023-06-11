#' Removes leading and trailing whitespace from a dataframe
#'
#' trimws only removes leading and trailing whitespace from a vector. trimws_df is an extension that will remove  whitespace in an entire dataframe.
#' 
#' @param df dataframe
#' @param ... passes arguments from trimws
#'
#' @return dataframe
#' 
#' @export

trimws_df <- function(df, ...) {
  for (col in names(df)) {
    if (is.character(df[[col]]) || is.factor(df[[col]])) {
      df[[col]] <- trimws(df[[col]], ...)
    }
  }
  
  return(df)
}
