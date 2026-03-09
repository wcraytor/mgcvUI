#' Earth Import Module -- UI
#'
#' Compact widget for importing an earthUI result (.rds file).
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_earth_import_ui("earth1"))
#'   server <- function(input, output, session) {
#'     mod_earth_import_server("earth1")
#'   }
#'   shinyApp(ui, server)
#' }
mod_earth_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("earth_file"), "earthUI Result (.rds)",
              accept = ".rds"),
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_earth")),
      verbatimTextOutput(ns("earth_summary")),
      downloadButton(ns("download_knots"), "Export Knots CSV",
                     class = "btn-sm btn-outline-secondary",
                     style = "width: 100%;")
    )
  )
}


#' Earth Import Module -- Server
#'
#' @param id Shiny module namespace ID.
#' @return A reactive containing an `mgcvUI_earth_knots` object, or
#'   `NULL`.
#' @export
mod_earth_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    earth_knots <- reactiveVal(NULL)

    # Cache directory for persisting uploaded files across sessions
    cache_dir <- file.path(tools::R_user_dir("mgcvUI", "data"), "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    output$has_earth <- reactive(!is.null(earth_knots()))
    outputOptions(output, "has_earth", suspendWhenHidden = FALSE)

    # Load and cache helper
    load_earth_ <- function(path, name) {
      ek <- import_earth(path)
      earth_knots(ek)
      # Cache for next session
      cached <- file.path(cache_dir, name)
      tryCatch(file.copy(path, cached, overwrite = TRUE),
               error = function(e) NULL)
      last_file <- file.path(cache_dir, ".last_earth")
      tryCatch(writeLines(name, last_file), error = function(e) NULL)
      ek
    }

    # Auto-load last-used earth file on startup
    last_file_path <- file.path(cache_dir, ".last_earth")
    if (file.exists(last_file_path)) {
      last_name <- trimws(readLines(last_file_path, n = 1L, warn = FALSE))
      cached_path <- file.path(cache_dir, last_name)
      if (nzchar(last_name) && file.exists(cached_path)) {
        tryCatch({
          load_earth_(cached_path, last_name)
          message("mgcvUI: auto-loaded cached earth: ", last_name)
        }, error = function(e) {
          message("mgcvUI: failed to auto-load cached earth: ", e$message)
        })
      }
    }

    observeEvent(input$earth_file, {
      req(input$earth_file)
      tryCatch({
        ek <- load_earth_(input$earth_file$datapath, input$earth_file$name)
        n_knots <- sum(vapply(ek$knots, length, integer(1)))
        showNotification(
          paste("Imported:", length(ek$knots), "variables,",
                n_knots, "knots"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Import error:", e$message),
                         type = "error")
        earth_knots(NULL)
      })
    })

    output$earth_summary <- renderPrint({
      ek <- earth_knots()
      req(ek)
      cat("Target:", paste(ek$target, collapse = ", "), "\n")
      cat("R-sq:", round(ek$earth_summary$r_squared, 4), "\n")
      cat("Terms:", ek$earth_summary$n_terms, "\n")
      for (var in names(ek$knots)) {
        cat(var, ": ",
            paste(round(ek$knots[[var]], 2), collapse = ", "),
            "\n")
      }
    })

    output$download_knots <- downloadHandler(
      filename = function() {
        paste0("earth_knots_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        ek <- earth_knots()
        req(ek)
        export_knots_csv(ek, file)
      }
    )

    reactive(earth_knots())
  })
}
