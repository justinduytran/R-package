#' write_csv_out
#'
#' @description
#' Wrapper for \code{\link[utils]{write.csv}} with additional functionality to automatically add the .csv file extension, prepend the date, and generate file overwrite warnings.
#'
#' @param output the object to be written as a .csv file.
#' @param file_name the name of the .csv file. Default is to be the same as the name of the output.
#' @param note optional text to append to the file_name. Intended to be a short description of the output supplemental to the \code{file_name}.
#' @param date prepend the system date in '\%Y\%m\%d' format. Default \code{TRUE}.
#' @param increment if a file with the same name exists, save as 'filename-n.csv' with incremented n. Default \code{TRUE}. Cannot be \code{TRUE} when \code{overwrite} is also \code{TRUE}.
#' @param overwrite if a file with the same name exists, overwrite. Default \code{FALSE}. Cannot be \code{TRUE} when \code{increment} is also \code{TRUE}.
#' @param ... additional \code{\link[utils]{write.csv}} arguments. Default change is \code{row.names = F}.
#'
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' # Will write mtcars.csv
#' write_csv_out(mtcars, date = FALSE)
#' # Will increment to mtcars-1.csv
#' write_csv_out(mtcars, date = FALSE)
#' 
#' # Will write the iris dataset to mtcars.csv
#' write_csv_out(iris, "mtcars", date = FALSE, overwrite = TRUE)
#' 
#' ## End(Not run)

write_csv_out <- function(output, file_name = output |> substitute() |> deparse(), note = NULL, date = TRUE, increment = TRUE, overwrite = FALSE, ...) {

  file_ext <- ".csv"
  
  if (grepl("\\.csv$", file_name)){
    file_name <- sub("\\.csv$", "", file_name)
    message("Detected '.csv' at the end of `file_name`. Adding '.csv' is not required.")
  }
  
  if (!is.null(note)){
    file_name <- paste0(file_name, "_", note)
  }

  # Prepend date
  if (date){
    file_name <- paste0(format(Sys.time(), "%Y%m%d"),"_", file_name)
  }

  file_name_full <- file_name |>
    paste0(file_ext)

  # Capture duplicates
  if (file.exists(file_name_full)){

    if (increment && overwrite) {
      stop("Only one of 'increment' or 'overwrite' can be TRUE.")
    }

    if (!overwrite && !increment) {
      stop("Error: file already exists. Set 'overwrite' = T to ignore or 'increment' = T to append numerical values.")
    }

    if (overwrite) {
      message("Overwriting existing file: ", file_name_full)
    }

    # Detect duplicates and increment accordingly
    if (increment) {

      # Running counter
      counter = 1

      while (file.exists(file_name_full)) {

        # Only append counter to filename before the extension
        # test-1.csv instead of test.csv-1
        file_name_full <- file_name |>
          paste0("-", counter) |>
          paste0(file_ext)
        # Increment counter
        counter <- counter + 1
      }
      if (counter > 1) {
        message(paste0("Filename in use. Appending numerical identifier: ", file_name_full))
      }
    }
  }
  write.csv(output, file_name_full, row.names = F, ...)
}
