#' Detect Column Types from a Data Frame
#'
#' Returns the R type of each column as a named character vector.
#'
#' @param df A data frame.
#' @return Named character vector with values like `"numeric"`,
#'   `"integer"`, `"factor"`, `"character"`, `"logical"`, `"Date"`,
#'   `"POSIXct"`.
#' @export
#' @examples
#' detect_column_types(mtcars)
#' df <- data.frame(
#'   x = 1:5, y = letters[1:5],
#'   d = Sys.Date() + 1:5,
#'   t = Sys.time() + 1:5
#' )
#' detect_column_types(df)
detect_column_types <- function(df) {
  vapply(df, function(col) {
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) return("POSIXct")
    if (inherits(col, "Date")) return("Date")
    if (is.factor(col)) return("factor")
    if (is.logical(col)) return("logical")
    if (is.integer(col)) return("integer")
    if (is.numeric(col)) return("numeric")
    if (is.character(col)) return("character")
    "character"
  }, character(1))
}

# Flag columns that are UNAMBIGUOUSLY categorical by type: character, factor,
# and logical. Numeric columns are NOT auto-flagged — a numeric variable is
# only a factor if the appraiser ticks the Factor box. Counting unique values
# can't distinguish a discrete continuous predictor (e.g. bath_count 0-5) from a
# numeric category code. Used to pre-check the Factor box in the variable table.
#' @noRd
detect_categoricals_ <- function(df) {
  res <- vapply(df, function(col) {
    is.character(col) || is.factor(col) || is.logical(col)
  }, logical(1))
  names(res) <- names(df)
  res
}
