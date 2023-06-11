#' Extracts and formats ABS Census TableBuilder output
#'
#' Takes ABS Census TableBuilder .xlsx files and reorganises rows, columns, and wafers into a 'long' formatted dataframe.
#'
#' @param table ABS Census TableBuilder .xlsx export (.csv exports will not work)
#' @param totals Default FALSE. TRUE will include the total columns/rows in the export which can be annoying.
#' @param short_names Default FALSE. TRUE will label columns using TableBuilder abbreviations instead of the entire label e.g. 'AGE5P' instead of 'Age in Five Year Groups'.
#'
#' @return A data frame with up to 4 columns: 'row', 'column', 'wafer' columns depending on ABS variables and a 'value' variable.
#' 
#' @export
#' 
#' @importFrom stats reshape
#' @importFrom utils head
#' @importFrom readxl read_excel

read_abscensustablebuilder <- function(table,
                                       totals = FALSE,
                                       short_names = FALSE) {
  # Constants ---------------------------------------------------------------
  
  # For adjusting bounds
  margin <- 2
  
  # Equals 1 when wafers are present
  # For relative adjustment from bound_top_left to read wafer
  # Also used for ensuring output is formatted correctly
  adjust_wafer <- 0
  
  # Define bounds of data ---------------------------------------------------
  
  # This is done by finding the top right and bottom left 'bounds' of the data
  #
  # The top right corner is given by the rightmost column's first non-NA value
  # It exploits the defaults of read_excel() to do this:
  # - Empty cells are coded NA
  # - Only columns with non-empty cells are read
  #
  # The bottom left corner is marked by the cell with "Total"
  # ABS always includes a "Total" row so this is a safe assumption (for now)
  
  # Read the first sheet to determine bounds
  sample <- suppressMessages(readxl::read_excel(table))
  
  bound_top_right <- c(ncol(sample),
                       as.numeric(head(which(!is.na(
                         sample[, ncol(sample)]
                       )), 1)))
  
  bound_bottom_left <- c(2,
                         which(apply(sample[, 2], 1, function(x)
                           any(x %in% "Total"))))
  
  bound_top_left <- c(bound_bottom_left[1],
                      bound_top_right[2])
  
  bound_bottom_right <- c(bound_top_right[1],
                          bound_bottom_left[2])
  
  # Extract data ------------------------------------------------------------
  
  # Determine how many sheets (i.e. if wafers) are present
  sheets <- suppressMessages(readxl::excel_sheets(table)) |>
    # Remove the 'template_rse' and 'format' sheets that ABS has hidden
    head(-2)
  
  # Flag if wafers present
  # Used in loop to create a wafer column
  if (length(sheets) > 1) {
    wafers <- TRUE
    # Change from 0 to impact certain formulas (wafer definition & long format)
    adjust_wafer <- 1
  } else {
    wafers <- FALSE
  }
  
  # Extract data as defined by bounds over the number of sheets/wafers
  output_table = c()
  suppressMessages(for (i in sheets) {
    extract <- readxl::read_excel(
      table,
      sheet <- i,
      range <-
        paste(
          excel_column_conversion(bound_top_left[1]),
          bound_top_left[2] + margin,
          ":",
          excel_column_conversion(bound_bottom_right[1]),
          bound_bottom_right[2] + margin,
          sep = ""
        )
    )
    
    # Fix up ABS formatting in row 1
    colnames(extract)[1] = c(extract[1, 1])
    extract <- extract[-c(1), ]
    
    # Define wafer in a column (if wafers exist)
    if (isTRUE(wafers)) {
      wafer_name <- readxl::read_excel(
        table,
        sheet <- i,
        # The cell defining which wafer the sheet represents is always one cell
        # up and one cell left from the top left bound
        range <- paste(
          excel_column_conversion(bound_top_left[1] - adjust_wafer),
          bound_top_left[2] + adjust_wafer,
          sep = ""
        ),
        col_names = FALSE
      )
      extract <- cbind(wafer_name, extract)
    }
    # Merge loop output into one data set
    output_table <- rbind(output_table, extract) |>
      # Solve issues with tibbles not playing well with reshape
      as.data.frame()
  })
  
  # Format output_table -----------------------------------------------------
  
  # Convert to long format
  output_table = reshape(
    output_table,
    direction = "long",
    # If there are no wafers, only one column (the first) is non-varying
    # Otherwise the first two are non-varying (wafer variable & row variable)
    varying = list(colnames(output_table)[(2 + adjust_wafer):length(output_table)]),
    times = colnames(output_table)[(2 + adjust_wafer):length(output_table)]
  )
  
  # Remove unnecessary output from reshape(): rownames, id column
  row.names(output_table) <- NULL
  output_table <- output_table[-c(length(output_table))]
  
  # Convert variables to factors to ease future data manipulation
  output_table[sapply(output_table, is.character)] <-
    lapply(output_table[sapply(output_table, is.character)],
           as.factor)
  
  # Totals ------------------------------------------------------------------
  
  # Remove all rows containing a "Total" string. They distort summary output.
  if (isFALSE(totals)) {
    output_table <-
      subset(output_table,
             !apply(output_table, 1, function(x)
               any(x %in% "Total")),
             drop = FALSE)
  }
  
  # Variable Names ----------------------------------------------------------
  
  # Uses ABS cell that says "X by Y (by Z)" to find variable names
  column_names <- sample[1, 1] |>
    unlist() |>
    strsplit(" by ") |>
    unlist() |>
    unname()
  
  # Use ABS abbreviations for variables
  # i.e. remove unabbrieviated names
  if (isTRUE(short_names)) {
    column_names <- gsub("(\\w+).*", "\\1", column_names)
  }
  
  # Rename columns correctly by variable
  if (isTRUE(wafers)) {
    colnames(output_table) <-
      c(column_names[1], column_names[2], column_names[3], "value")
  } else {
    colnames(output_table) <-
      c(column_names[1], column_names[2], "value")
  }
  
  # Output ------------------------------------------------------------------
  
  return(output_table)
  
}