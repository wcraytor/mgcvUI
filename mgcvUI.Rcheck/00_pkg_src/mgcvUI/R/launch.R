#' Launch the mgcvUI Shiny Application
#'
#' Opens the interactive GAM builder in your default web browser.
#'
#' @param port Integer port number. Defaults to 7880.
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return Called for its side effect (launches the app). Returns the
#'   value of [shiny::runApp()] invisibly.
#' @export
#' @examples
#' if (interactive()) {
#'   mgcvUI()
#' }
mgcvUI <- function(port = 7880L, ...) {
  app_dir <- system.file("app", package = "mgcvUI")
  if (!nzchar(app_dir)) {
    stop("Could not find the mgcvUI app directory. ",
         "Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, port = port, launch.browser = TRUE, ...)
}
