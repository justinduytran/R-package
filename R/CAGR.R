#' Calculate compounded growth rate
#' 
#' Calculates the compounded growth rate for between a starting and ending value given a number of (equal) time periods. Usually used to compute the Compound Annual Growth Rate (CAGR) but can be used with different periods. 
#'
#' @param t Number of compounding periods.
#' @param x_i+t Ending value.
#' @param x_i Starting value. 
#' 
#' @details
#' Computes formula: (x_(i+t)/x_i)^(1/t)-1
#'
#' @return numeric
#'
#' @export

CAGR <- function(t, `x_i+t`, x_i) {
  (`x_i+t` / x_i)^(1 / t) - 1
}