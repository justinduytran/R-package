#' Converts Excel column references
#'
#' Converts Excel 'A1 reference style' column references to 'numeric' or vice versa.
#'
#' @param col Column number to turn into Excel's 'A1 reference style' for columns. Alternatively, an 'A1 reference style' string to turn into a column number.
#'
#' @return A string with the corresponding 'A1 reference style' column reference. Alternatively, a number corresponding to the given 'A1 reference style' column reference.
#'
#' @export
#'
#' @examples
#' excel_column_conversion(1)
#' excel_column_conversion(26)
#' excel_column_conversion(27)
#' excel_column_conversion("A")
#' excel_column_conversion("Z")
#' excel_column_conversion("AA")
 
excel_column_conversion <- function(col) {
  if (is.numeric(col)) {
    dividend <- col
    cell <- ""
    modulo <- 0
    
    while (dividend > 0) {
      modulo <- (dividend - 1) %% 26
      cell <- paste0(LETTERS[modulo + 1], cell)
      dividend <- (dividend - modulo) %/% 26
    }
    return(cell)
    
  } else if (is.character(col)) {
    letters <- strsplit(col, "")[[1]]
    col_number <- 0
    
    for (i in seq_along(letters)) {
      col_number <- col_number * 26 + match(letters[i], LETTERS)
    }
    return(col_number)
    
  } else {
    stop("Invalid input. Please provide a number or a character string.")
  }
  
}
