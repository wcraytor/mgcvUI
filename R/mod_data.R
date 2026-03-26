#' Data Import Module -- UI
#'
#' Compact file browser widget for embedding in a sidebar, with a per-file
#' locale country selector for CSV separator and decimal conventions.
#' Reads files directly from disk via shinyFiles (no upload size limits).
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
    tags$div(
      class = "input-group",
      style = "margin-bottom:8px;",
      shinyFiles::shinyFilesButton(
        ns("file_browse"), "Browse...",
        title = "Select Data File (.csv, .xlsx, .xls)",
        multiple = FALSE,
        class = "btn-secondary"
      ),
      tags$span(
        class = "form-control",
        style = "font-size: 0.9em; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
        textOutput(ns("loaded_path"), inline = TRUE)
      )
    ),
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
    loaded_path <- reactiveVal(NULL)
    sheets <- reactiveVal(NULL)

    # Cache directory for persisting last-used path
    cache_dir <- file.path(tools::R_user_dir("mgcvUI", "data"), "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    # --- Restore saved import locale on startup ---
    locale_defaults <- settings_db_read_locale_()
    if (!is.null(locale_defaults) && length(locale_defaults) > 0L) {
      ld <- locale_defaults
      if (!is.null(ld$locale_import))
        updateSelectInput(session, "locale_import", selected = ld$locale_import)
      else if (!is.null(ld$locale_country))
        updateSelectInput(session, "locale_import", selected = ld$locale_country)
    }

    output$loaded_path <- renderText({
      f <- loaded_path()
      if (is.null(f)) "No file chosen" else basename(f)
    })

    # Load helper -- reads directly from disk, no copy needed
    load_data_ <- function(path, name, sheet = 1L) {
      import_country <- tryCatch(
        input$locale_import %||% "us",
        error = function(e) "us"
      )
      presets <- locale_country_presets_()
      preset <- presets[[import_country]] %||% presets[["us"]]
      sep <- preset$csv_sep
      dec <- preset$csv_dec
      df <- import_data(path, sheet = sheet, sep = sep, dec = dec)
      imported(df)
      orig_filename(name)
      loaded_path(normalizePath(path, mustWork = FALSE))

      # Detect sheets for Excel files
      ext <- tolower(tools::file_ext(path))
      if (ext %in% c("xlsx", "xls")) {
        sheets(readxl::excel_sheets(path))
      } else {
        sheets(NULL)
      }

      # Save name, directory, and full path for next session
      tryCatch({
        writeLines(name, file.path(cache_dir, ".last_data"))
        writeLines(dirname(path), file.path(cache_dir, ".last_data_dir"))
        writeLines(normalizePath(path, mustWork = FALSE),
                   file.path(cache_dir, ".last_data_path"))
      }, error = function(e) NULL)
      df
    }

    # Restore last-used directory for file browser
    last_dir <- path.expand("~")
    last_dir_file <- file.path(cache_dir, ".last_data_dir")
    if (file.exists(last_dir_file)) {
      saved_dir <- trimws(readLines(last_dir_file, n = 1L, warn = FALSE))
      if (nzchar(saved_dir) && dir.exists(saved_dir)) {
        last_dir <- saved_dir
      }
    }

    # Show last-used filename (but do NOT auto-load -- wait for Browse)
    last_name_file <- file.path(cache_dir, ".last_data")
    last_path_file <- file.path(cache_dir, ".last_data_path")
    if (file.exists(last_name_file)) {
      last_name <- trimws(readLines(last_name_file, n = 1L, warn = FALSE))
      if (nzchar(last_name)) {
        last_path <- if (file.exists(last_path_file))
          trimws(readLines(last_path_file, n = 1L, warn = FALSE)) else ""
        if (nzchar(last_path) && file.exists(last_path)) {
          loaded_path(last_path)
          orig_filename(last_name)
        }
      }
    }

    # File browser with last-used directory as default
    volumes <- c(Home = path.expand("~"), Root = "/")
    default_root <- "Home"
    default_path <- ""
    home <- path.expand("~")
    if (startsWith(last_dir, home) && last_dir != home) {
      default_path <- sub(paste0("^", home, "/?"), "", last_dir)
    } else if (last_dir != home) {
      volumes <- c("Last Folder" = last_dir, volumes)
      default_root <- "Last Folder"
    }
    shinyFiles::shinyFileChoose(input, "file_browse", roots = volumes,
                                defaultRoot = default_root,
                                defaultPath = default_path,
                                filetypes = c("csv", "xlsx", "xls"),
                                session = session)

    observeEvent(input$file_browse, {
      file_info <- shinyFiles::parseFilePaths(volumes, input$file_browse)
      if (nrow(file_info) == 0) return()
      path <- as.character(file_info$datapath)
      tryCatch({
        load_data_(path, basename(path), sheet = input$sheet %||% 1L)
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

    output$sheet_selector <- renderUI({
      req(sheets())
      ns <- session$ns
      selectInput(ns("sheet"), "Sheet", choices = sheets(),
                  selected = sheets()[1])
    })

    observeEvent(input$sheet, {
      req(loaded_path(), input$sheet)
      import_country <- input$locale_import %||% "us"
      presets <- locale_country_presets_()
      preset <- presets[[import_country]] %||% presets[["us"]]
      tryCatch({
        imported(import_data(loaded_path(), sheet = input$sheet,
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
        loaded_path(NULL)
        sheets(NULL)
      }
    )
  })
}
