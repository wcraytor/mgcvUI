#' Launch the mgcvUI Shiny Application
#'
#' Opens the interactive GAM builder in your default web browser.
#'
#' @param port Integer port number. Defaults to 7880.
#' @param launch.browser Logical; open the app in a browser. Defaults to
#'   `TRUE` in interactive sessions, `FALSE` otherwise.
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return Called for its side effect (launches the app). Returns the
#'   value of [shiny::runApp()] invisibly.
#' @export
#' @examples
#' if (interactive()) {
#'   mgcvUI()
#' }
mgcvUI <- function(port = 7880L, launch.browser = interactive(), ...) {
  if (getRversion() < "4.1.0") {
    stop("mgcvUI requires R >= 4.1.0 (you have ", getRversion(), "). ",
         "Please update R from https://cran.r-project.org/", call. = FALSE)
  }
  app_dir <- system.file("app", package = "mgcvUI")
  if (!nzchar(app_dir)) {
    stop("Could not find the mgcvUI app directory. ",
         "Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}
