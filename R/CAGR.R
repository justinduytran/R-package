#' Compute rate of return (compound annual growth rate)
#'
#' Function to compute rate of return. Most commonly used to calculate the compound annual growth rate of a series.
#'
#' @param period Number of compounding periods. 
#' @param FV Future value.
#' @param PV Present value.
#' 
#' @details
#' Computes formula: (FV/PV)^(1/period)-1
#' 
#' @return A rate of return
#'
#' @export

CAGR <- function(period, FV, PV) {
  growth_rate <- (FV/PV)^(1/period)-1
  return(growth_rate)
}