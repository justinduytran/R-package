% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CAGR.R
\name{CAGR}
\alias{CAGR}
\title{Compute rate of return (compound annual growth rate)}
\usage{
CAGR(period, FV, PV)
}
\arguments{
\item{period}{Number of compounding periods.}

\item{FV}{Future value.}

\item{PV}{Present value.}
}
\value{
A rate of return
}
\description{
Function to compute rate of return. Most commonly used to calculate the compound annual growth rate of a series.
}
\details{
Computes formula: (FV/PV)^(1/period)-1
}
