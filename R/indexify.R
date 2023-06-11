#' Convert vector into an index
#'
#' @param vector Vector or numeric column to convert to an index.
#' @param index_to A number, usually an element in vector, corresponding to the base period.
#' @param index_to_hundred Default TRUE. Multiplies index by 100, i.e. base period will be 100 instead of 1. Any non-TRUE value will not multiply the index by 100.
#' @param rounding Default 2. See base function round, digits argument.
#'
#' @return A vector derived by indexing vector to the index_to value.
#' @export
#'
#' @examples
#' indexify(c(1:100), 1)
#' indexify(c(1:100), 1, FALSE)
#' indexify(c(1:100), c(1:100)[40])
#' indexify(c(1:100), c(1:100)[40], FALSE)
#' indexify(c(1:100), c(1:100)[40], FALSE, 5)

indexify = function(vector,
                    index_to,
                    index_to_hundred = TRUE,
                    rounding = 2) {
  round((vector / index_to) * 
          ifelse(index_to_hundred == TRUE, 100, 1),
        rounding)
}