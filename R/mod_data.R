#' Data Import Module -- UI
#'
#' Compact file upload widget for embedding in a sidebar.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_data_ui("data1"))
#'   server <- function(input, output, session) {
#'     mod_data_server("data1")
#'   }
#'   shinyApp(ui, server)
#' }
mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose CSV or Excel file",
              accept = c(".csv", ".xlsx", ".xls")),
    conditionalPanel(
      condition = sprintf("output['%s']", ns("is_excel")),
      numericInput(ns("sheet"), "Sheet number", value = 1L, min = 1L)
    ),
    textOutput(ns("data_info"))
  )
}


#' Data Import Module -- Server
#'
#' @param id Shiny module namespace ID.
#' @return A list with `data` (reactive data frame) and `filename`
#'   (reactive original file basename).
#' @export
mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    imported <- reactiveVal(NULL)
    orig_filename <- reactiveVal(NULL)

    # Cache directory for persisting uploaded files across sessions
    cache_dir <- file.path(tools::R_user_dir("mgcvUI", "data"), "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    output$is_excel <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      ext %in% c("xlsx", "xls")
    })
    outputOptions(output, "is_excel", suspendWhenHidden = FALSE)

    # Load and cache helper
    load_and_cache_ <- function(path, name, sheet = 1L) {
      df <- import_data(path, sheet = sheet)
      imported(df)
      orig_filename(name)
      # Cache a copy for next session
      cached <- file.path(cache_dir, name)
      tryCatch(file.copy(path, cached, overwrite = TRUE),
               error = function(e) NULL)
      # Remember last-used filename
      last_file <- file.path(cache_dir, ".last_data")
      tryCatch(writeLines(name, last_file), error = function(e) NULL)
      df
    }

    # Auto-load last-used data file on startup
    last_file_path <- file.path(cache_dir, ".last_data")
    if (file.exists(last_file_path)) {
      last_name <- trimws(readLines(last_file_path, n = 1L, warn = FALSE))
      cached_path <- file.path(cache_dir, last_name)
      if (nzchar(last_name) && file.exists(cached_path)) {
        tryCatch({
          load_and_cache_(cached_path, last_name)
          message("mgcvUI: auto-loaded cached data: ", last_name)
        }, error = function(e) {
          message("mgcvUI: failed to auto-load cached data: ", e$message)
        })
      }
    }

    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        load_and_cache_(input$file$datapath, input$file$name,
                        sheet = input$sheet %||% 1L)
        showNotification(
          paste("Loaded", nrow(imported()), "rows,", ncol(imported()), "columns"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Import error:", e$message), type = "error")
        imported(NULL)
        orig_filename(NULL)
      })
    })

    output$data_info <- renderText({
      df <- imported()
      if (is.null(df)) return("No data loaded.")
      paste(nrow(df), "rows,", ncol(df), "columns --", orig_filename())
    })

    list(
      data     = reactive(imported()),
      filename = reactive(orig_filename())
    )
  })
}
