#' Data Import Module -- UI
#'
#' Compact file upload widget for embedding in a sidebar, with a per-file
#' locale country selector for CSV separator and decimal conventions.
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
    tags$div(style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:4px;",
      tags$label(class = "control-label", "Choose CSV or Excel file"),
      tags$div(style = "display:flex; align-items:center; gap:6px;",
        tags$label(class = "control-label", style = "margin-bottom:0;", "Locale"),
        tags$div(style = "width:150px; margin-bottom:0;",
          selectInput(ns("locale_import"), NULL,
                      choices = locale_country_choices_(),
                      selected = "us", width = "100%")
        )
      )
    ),
    fileInput(ns("file_input"), NULL,
              accept = c(".csv", ".xlsx", ".xls")),
    uiOutput(ns("sheet_selector")),
    uiOutput(ns("data_preview_info"))
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
    file_path <- reactiveVal(NULL)
    sheets <- reactiveVal(NULL)

    # --- Restore saved import locale on startup ---
    locale_defaults <- settings_db_read_locale_()
    if (!is.null(locale_defaults) && length(locale_defaults) > 0L) {
      ld <- locale_defaults
      if (!is.null(ld$locale_import))
        updateSelectInput(session, "locale_import", selected = ld$locale_import)
      else if (!is.null(ld$locale_country))
        updateSelectInput(session, "locale_import", selected = ld$locale_country)
    }

    observeEvent(input$file_input, {
      req(input$file_input)
      path <- input$file_input$datapath
      name <- input$file_input$name
      ext <- tolower(tools::file_ext(name))

      import_country <- input$locale_import %||% "us"
      presets <- locale_country_presets_()
      preset <- presets[[import_country]] %||% presets[["us"]]

      tryCatch({
        df <- import_data(path, sheet = 1L,
                          sep = preset$csv_sep, dec = preset$csv_dec)
        imported(df)
        orig_filename(name)
        file_path(path)

        if (ext %in% c("xlsx", "xls")) {
          sheets(readxl::excel_sheets(path))
        } else {
          sheets(NULL)
        }

        showNotification(
          paste("Loaded", nrow(df), "rows,", ncol(df), "columns"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Import error:", e$message), type = "error")
        imported(NULL)
        orig_filename(NULL)
      })
    })

    output$sheet_selector <- renderUI({
      req(sheets())
      ns <- session$ns
      selectInput(ns("sheet"), "Sheet", choices = sheets(),
                  selected = sheets()[1])
    })

    observeEvent(input$sheet, {
      req(file_path(), input$sheet)
      import_country <- input$locale_import %||% "us"
      presets <- locale_country_presets_()
      preset <- presets[[import_country]] %||% presets[["us"]]
      tryCatch({
        imported(import_data(file_path(), sheet = input$sheet,
                             sep = preset$csv_sep, dec = preset$csv_dec))
      }, error = function(e) {
        showNotification(paste("Import error:", e$message),
                         type = "error", duration = 15)
      })
    })

    output$data_preview_info <- renderUI({
      df <- imported()
      if (is.null(df)) return(NULL)
      tags$div(
        class = "alert alert-info",
        style = "font-size: 0.85em; padding: 8px;",
        sprintf("%d rows, %d columns", nrow(df), ncol(df))
      )
    })

    list(
      data     = reactive(imported()),
      filename = reactive(orig_filename()),
      reset    = function() {
        imported(NULL)
        orig_filename(NULL)
        file_path(NULL)
        sheets(NULL)
      }
    )
  })
}
