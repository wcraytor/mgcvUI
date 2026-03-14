#' Import Data from CSV or Excel
#'
#' Reads a CSV or Excel file and returns a data frame with cleaned column
#' names (snake_case, no special characters).
#'
#' @param filepath Character path to a `.csv`, `.xlsx`, or `.xls` file.
#' @param sheet Integer or character sheet identifier for Excel files.
#'   Ignored for CSV.
#' @param sep Character. Field separator for CSV files. Default `","`.
#'   Use `";"` for European-style CSVs.
#' @param dec Character. Decimal separator for CSV files. Default `"."`.
#'   Use `","` for European-style CSVs.
#' @return A data frame.
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(mtcars, tmp, row.names = FALSE)
#' df <- import_data(tmp)
#' head(df)
#' unlink(tmp)
import_data <- function(filepath, sheet = 1L, sep = ",", dec = ".") {
  ext <- tolower(tools::file_ext(filepath))
  df <- switch(ext,
    csv = utils::read.csv(filepath, stringsAsFactors = FALSE,
                          check.names = FALSE, sep = sep, dec = dec),
    xlsx = readxl::read_excel(filepath, sheet = sheet),
    xls  = readxl::read_excel(filepath, sheet = sheet),
    stop("Unsupported file type: ", ext,
         ". Use .csv, .xlsx, or .xls", call. = FALSE)
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- clean_names_(names(df))
  df
}

#' Clean column names to snake_case
#' @param nms Character vector of names.
#' @return Character vector of cleaned names.
#' @noRd
clean_names_ <- function(nms) {
  nms <- gsub("[^[:alnum:]_.]", "_", nms)
  nms <- gsub("\\.+", "_", nms)
  nms <- gsub("([a-z])([A-Z])", "\\1_\\2", nms)
  nms <- tolower(nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  make.unique(nms, sep = "_")
}
