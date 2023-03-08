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
#' xlcolconv(1)
#' xlcolconv(26)
#' xlcolconv(27)
#' xlcolconv("A")
#' xlcolconv("Z")
#' xlcolconv("AA")

xlcolconv <- function(col) {
  # test: 1 = A, 26 = Z, 27 = AA, 703 = AAA
  if (is.character(col)) {
    # code is from https://stackoverflow.com/a/34537691/2292993
    # Thanks Jerry T!
    s <- col
    # Uppercase
    s_upper <- toupper(s)
    # Convert string to a vector of single letters
    s_split <- unlist(strsplit(s_upper, split = ""))
    # Convert each letter to the corresponding number
    s_number <- sapply(s_split, function(x) {
      which(LETTERS == x)
    })
    # Derive the numeric value associated with each letter
    numbers <- 26 ^ ((length(s_number) - 1):0)
    # Calculate the column number
    column_number <- sum(s_number * numbers)
    return(column_number)
  } else {
    n = col
    letters = ''
    while (n > 0) {
      r = (n - 1) %% 26  # remainder
      letters = paste0(intToUtf8(r + utf8ToInt('A')), letters) # ascii
      n = (n - 1) %/% 26 # quotient
    }
    return(letters)
  }
}
