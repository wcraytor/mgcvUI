library(mgcvUI)

# Allow uploads up to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

# Load Roboto Condensed for R graphics (ggplot2 + base R)
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
  showtext::showtext_auto()
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Roboto Condensed")
  )
} else {
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
}

# Enable thematic so ggplot2 auto-adapts to current theme
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny()
}

# Font family helper
mgcv_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}

# Nord Light theme
nord_light <- bslib::bs_theme(
  version = 5,
  bg = "#eceff4",
  fg = "#2e3440",

  primary = "#5e81ac",
  secondary = "#81a1c1",
  success = "#a3be8c",
  info = "#88c0d0",
  warning = "#ebcb8b",
  danger = "#bf616a",
  base_font = bslib::font_google("Roboto Condensed")
)

# Nord Dark theme
nord_dark <- bslib::bs_theme(
  version = 5,

  bg = "#2e3440",
  fg = "#d8dee9",
  primary = "#88c0d0",
  secondary = "#81a1c1",
  success = "#a3be8c",
  info = "#5e81ac",
  warning = "#ebcb8b",
  danger = "#bf616a",
  base_font = bslib::font_google("Roboto Condensed")
) |>
  bslib::bs_add_rules("
    .navbar { background-color: #242933 !important; }
    .card   { background-color: #3b4252 !important; border-color: #434c5e; }
  ")

ui <- fluidPage(
  theme = nord_light,
  withMathJax(),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$style(HTML("
    /* --- Full-width container (match earthUI) --- */
    .container-fluid { max-width: 100% !important; padding: 0 15px; }

    /* --- Initial window sizing --- */
    body { min-width: 1400px; }

    /* --- Variable table --- */
    .mgcv-var-row {
      display: flex; align-items: center;
      padding: 3px 0; border-bottom: 1px solid var(--bs-border-color);
    }
    .mgcv-var-header {
      font-weight: bold; border-bottom: 2px solid var(--bs-border-color);
      background: var(--bs-tertiary-bg); padding: 6px 0;
    }
    .mgcv-cell-inc    { width: 35px; text-align: center; }
    .mgcv-cell-name   { flex: 1; min-width: 80px; overflow: hidden;
                        text-overflow: ellipsis; white-space: nowrap;
                        padding: 0 6px;
                        font-family: 'Roboto Condensed', monospace;
                        font-size: 0.85em; }
    .mgcv-cell-type   { width: 85px; }
    .mgcv-cell-na     { width: 75px; text-align: right; padding-right: 4px;
                        font-size: 0.75em; white-space: nowrap;
                        font-family: 'Roboto Condensed', monospace; }
    .mgcv-cell-linear { width: 35px; text-align: center; }
    .mgcv-cell-special { width: 110px; }
    .mgcv-var-row select {
      width: 100%; padding: 1px 2px; font-size: 0.8em;
      border: 1px solid var(--bs-border-color); border-radius: 3px;
      appearance: auto; -webkit-appearance: auto;
      background-color: var(--bs-body-bg); color: var(--bs-body-color);
    }
    .mgcv-var-row input[type='checkbox'] {
      width: 15px; height: 15px; cursor: pointer;
    }

    /* --- DataTables --- */
    .dataTable td, .dataTable th { padding: 4px 8px !important; }
    .dataTables_wrapper { font-size: 0.9em; overflow-x: auto; }

    /* DT DataTables: adapt to current theme */
    .dataTables_wrapper { color: var(--bs-body-color); }
    table.dataTable { color: var(--bs-body-color); border-color: var(--bs-border-color); }
    table.dataTable thead th,
    table.dataTable thead td {
      background-color: var(--bs-tertiary-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important;
    }
    table.dataTable tbody td {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable tbody tr:hover td { background-color: var(--bs-tertiary-bg); }
    table.dataTable tbody tr.odd td { background-color: var(--bs-secondary-bg); }
    .dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate {
      color: var(--bs-body-color);
    }
    .dataTables_paginate .paginate_button {
      color: var(--bs-body-color) !important;
      background: var(--bs-body-bg); border-color: var(--bs-border-color);
    }
    .dataTables_paginate .paginate_button.current {
      color: var(--bs-body-color) !important;
      background: var(--bs-tertiary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_filter input, .dataTables_length select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }

    /* --- Collapsible sections --- */
    .mgcv-section > summary { cursor: pointer; list-style: none; }
    .mgcv-section > summary::-webkit-details-marker { display: none; }
    .mgcv-section > summary h4::before {
      content: '\\25B6  ';
      font-size: 0.7em;
      transition: transform 0.2s;
      display: inline-block;
    }
    .mgcv-section[open] > summary h4::before { transform: rotate(90deg); }

    /* --- Theme toggle button --- */
    #mgcv-theme-toggle {
      position: fixed; top: 12px; right: 20px; z-index: 10000;
      width: 38px; height: 38px; border-radius: 50%;
      border: 2px solid var(--bs-border-color);
      background: var(--bs-body-bg); color: var(--bs-body-color);
      font-size: 18px; cursor: pointer; display: flex;
      align-items: center; justify-content: center;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s;
    }
    #mgcv-theme-toggle:hover { box-shadow: 0 2px 10px rgba(0,0,0,0.25); }

    /* --- Dark mode --- */
    [data-bs-theme='dark'] #mgcv-theme-toggle {
      background: #3b4252; border-color: #434c5e;
    }
    [data-bs-theme='dark'] .mgcv-var-header { background: #3b4252; }
    [data-bs-theme='dark'] .mgcv-var-row { border-color: #434c5e; }
    [data-bs-theme='dark'] .mgcv-var-row select {
      background: #3b4252 !important; color: #d8dee9 !important;
      border-color: #434c5e !important;
    }
    [data-bs-theme='dark'] details > summary { color: #d8dee9 !important; }
    [data-bs-theme='dark'] .nav-tabs .nav-link.active {
      color: #d8dee9 !important; background-color: #2e3440 !important;
      border-color: #434c5e #434c5e #2e3440 !important;
    }
    [data-bs-theme='dark'] .nav-tabs .nav-link { color: #81a1c1; }
    [data-bs-theme='dark'] .nav-tabs .nav-link:hover {
      color: #d8dee9; border-color: #434c5e;
    }
  "))),

  # Dark/light toggle button
  tags$button(id = "mgcv-theme-toggle", HTML("&#9790;")),
  tags$script(HTML("
    var mgcvCurrentMode = 'light';

    function mgcvToggleTheme() {
      mgcvCurrentMode = (mgcvCurrentMode === 'dark') ? 'light' : 'dark';
      Shiny.setInputValue('dark_mode', mgcvCurrentMode, {priority: 'event'});
      var btn = document.getElementById('mgcv-theme-toggle');
      if (btn) btn.innerHTML = (mgcvCurrentMode === 'dark') ? '\\u2600' : '\\u263E';
      try { localStorage.setItem('mgcvUI_theme', mgcvCurrentMode); } catch(e) {}
    }

    document.getElementById('mgcv-theme-toggle').onclick = mgcvToggleTheme;

    $(document).on('shiny:connected', function() {
      var saved = null;
      try { saved = localStorage.getItem('mgcvUI_theme'); } catch(e) {}
      if (saved === 'dark') {
        mgcvCurrentMode = 'dark';
        var btn = document.getElementById('mgcv-theme-toggle');
        if (btn) btn.innerHTML = '\\u2600';
        Shiny.setInputValue('dark_mode', 'dark', {priority: 'event'});
      } else {
        Shiny.setInputValue('dark_mode', 'light', {priority: 'event'});
      }
    });
  ")),

  # Title with logo
  tags$div(
    style = "padding: 10px 15px;",
    tags$h2(
      tags$img(src = "logo.png", height = "32px",
               style = "margin-right: 8px; vertical-align: middle;"),
      "mgcvUI",
      tags$small(" - GAM Builder",
                 style = "font-size: 0.6em; color: var(--bs-secondary-color);")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Purpose Mode ---
      tags$div(
        style = "font-weight:bold;",
        radioButtons("purpose", "Purpose:",
                     choices = c("General" = "general",
                                 "For Appraisal" = "appraisal",
                                 "Market Area Analysis" = "market"),
                     selected = "general", inline = TRUE)
      ),
      hr(),

      # --- 1. Import Data ---
      tags$details(class = "mgcv-section",
        tags$summary(h4("1. Import Data", style = "display:inline;")),
        mod_data_ui("data")
      ),
      hr(),

      # --- 2. Import from earthUI (optional) ---
      tags$details(class = "mgcv-section",
        tags$summary(h4("2. Import from earthUI (optional)",
                        style = "display:inline;")),
        div(style = "padding-top: 6px;",
          mod_earth_import_ui("earth")
        )
      ),
      hr(),

      conditionalPanel(
        condition = "output.data_imported",

        # --- 3. Project Output Folder ---
        tags$details(class = "mgcv-section",
          tags$summary(h4("3. Project Output Folder",
                          style = "display:inline;")),
          textInput("output_folder", NULL,
                    value = path.expand("~/Downloads"))
        ),
        hr(),

        # --- 4. Variable Configuration ---
        tags$details(class = "mgcv-section",
          tags$summary(h4("4. Variable Configuration",
                          style = "display:inline;")),
          conditionalPanel(
            condition = "input.purpose !== 'general'",
            dateInput("effective_date", "Effective Date",
                      value = Sys.Date())
          ),
          mod_variables_ui("vars")
        ),
        hr(),

        # --- 5. Mgcv Call Parameters ---
        tags$details(class = "mgcv-section",
          tags$summary(h4("5. Mgcv Call Parameters",
                          style = "display:inline;")),
          mod_variables_params_ui("vars")
        ),
        hr(),

        # --- 6. Fit Mgcv GAM Model ---
        tags$details(class = "mgcv-section",
          tags$summary(h4("6. Fit Mgcv GAM Model",
                          style = "display:inline;")),
          mod_model_fit_ui("model")
        ),

        # --- 7. Download Estimated Sale Prices & Residuals ---
        conditionalPanel(
          condition = "output.model_fitted",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("download_heading", inline = TRUE)),
            conditionalPanel(
              condition = "input.purpose !== 'general'",
              actionButton("export_data", "Download Output (Excel)",
                           class = "btn-success",
                           style = "width: 100%;")
            ),
            conditionalPanel(
              condition = "input.purpose === 'general'",
              tags$p(
                tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 8. Calculate RCA Adjustments (Appraisal only) ---
        conditionalPanel(
          condition = "output.model_fitted",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(h4("8. Calculate RCA Adjustments & Download",
                            style = "display:inline;")),
            conditionalPanel(
              condition = "input.purpose === 'appraisal'",
              actionButton("rca_output_btn",
                           "Calculate RCA Adjustments & Download",
                           class = "btn-success",
                           style = "width: 100%;")
            ),
            conditionalPanel(
              condition = "input.purpose !== 'appraisal'",
              tags$p(
                tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 9. Download Report ---
        conditionalPanel(
          condition = "output.model_fitted",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("report_heading", inline = TRUE)),
            selectInput("export_format", "Format",
                        choices = c("Word" = "docx",
                                    "PDF" = "pdf")),
            actionButton("export_report_btn", "Download Report",
                         class = "btn-success",
                         style = "width: 100%;")
          )
        )
      )
    ),

    mainPanel(
      width = 8,
      conditionalPanel(
        condition = "!output.data_imported",
        div(
          class = "text-muted",
          style = "text-align: center; padding: 80px 20px;",
          h3("Welcome to mgcvUI"),
          p("Upload a CSV or Excel file, configure variables, ",
            "and fit a GAM model."),
          p("Designed for real estate appraisers who need transparent, ",
            "defensible nonlinear models.")
        )
      ),
      mod_model_results_ui("model")
    )
  ),

  # Footer
  tags$hr(style = "margin-top: 30px; margin-bottom: 10px;"),
  tags$footer(
    style = paste("text-align: center; padding: 10px 15px 15px;",
                  "font-size: 0.8em; color: var(--bs-secondary-color);"),
    tags$p(style = "margin: 2px 0;",
      HTML(paste0("mgcvUI v", utils::packageVersion("mgcvUI")))
    ),
    tags$p(style = "margin: 2px 0;",
      "Licensed under the ",
      tags$a(href = "https://www.gnu.org/licenses/agpl-3.0.html",
             target = "_blank",
             "GNU Affero General Public License v3.0"),
      " or later (AGPL-3)."
    )
  )
)

server <- function(input, output, session) {
  # --- Nord theme switching ---
  observe({
    mode <- input$dark_mode
    req(mode)
    tryCatch(
      session$setCurrentTheme(
        if (mode == "dark") nord_dark else nord_light
      ),
      error = function(e) {
        message("Theme switch error (non-fatal): ", conditionMessage(e))
      }
    )
  })

  # Data import - returns list(data, filename)
  data_mod <- mod_data_server("data")

  # --- Data imported flag ---
  output$data_imported <- reactive(!is.null(data_mod$data()))
  outputOptions(output, "data_imported", suspendWhenHidden = FALSE)

  # --- Restore app-level settings when data file changes ---
  observeEvent(data_mod$filename(), {
    fname <- data_mod$filename()
    if (is.null(fname)) return()
    saved <- mgcvUI:::settings_db_read_(fname)
    if (is.null(saved)) return()
    if (!is.null(saved$purpose)) {
      updateRadioButtons(session, "purpose", selected = saved$purpose)
    }
    if (!is.null(saved$output_folder)) {
      updateTextInput(session, "output_folder", value = saved$output_folder)
    }
    if (!is.null(saved$effective_date)) {
      updateDateInput(session, "effective_date",
                      value = as.Date(saved$effective_date))
    }
  })

  # --- Save app-level settings when they change ---
  observe({
    fname <- data_mod$filename()
    req(fname)
    input$purpose
    input$output_folder
    input$effective_date
    isolate({
      saved <- mgcvUI:::settings_db_read_(fname)
      config <- if (!is.null(saved)) saved else list()
      config$output_folder  <- input$output_folder %||% ""
      config$effective_date <- as.character(input$effective_date %||% "")
      config$purpose        <- input$purpose %||% "general"
      mgcvUI:::settings_db_write_(fname, config)
    })
  })

  output$data_preview_table <- DT::renderDT({
    req(data_mod$data())
    DT::datatable(data_mod$data(),
                  options = list(scrollX = TRUE, pageLength = 15),
                  rownames = FALSE)
  })

  # Optional earth import
  earth_knots_r <- mod_earth_import_server("earth")

  # Variable selection - returns config list
  var_config_r <- mod_variables_server("vars",
                                       data_r        = data_mod$data,
                                       filename_r    = data_mod$filename,
                                       earth_knots_r = earth_knots_r)

  # Model fitting + results display
  gam_result_r <- mod_model_server("model",
                                   data_r        = data_mod$data,
                                   var_config_r  = var_config_r,
                                   earth_knots_r = earth_knots_r)

  # Report export (existing module — kept for function export features)
  mod_report_server("report",
                    gam_result_r = gam_result_r,
                    data_r       = data_mod$data)

  # --- Model fitted flag for conditionalPanel ---
  output$model_fitted <- reactive(!is.null(gam_result_r()))
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  # --- RCA percentage data (stored after RCA export) ---
  rv_rca <- reactiveValues(pct_data = NULL)

  output$rca_computed <- reactive(!is.null(rv_rca$pct_data))
  outputOptions(output, "rca_computed", suspendWhenHidden = FALSE)

  # --- Dynamic step headings ---
  output$download_heading <- renderUI({
    label <- if (identical(input$purpose, "general")) {
      "7. Download Estimated Target Variable(s) & Residuals"
    } else {
      "7. Download Estimated Sale Prices & Residuals"
    }
    h4(label, style = "display:inline;")
  })

  output$report_heading <- renderUI({
    n <- if (identical(input$purpose, "appraisal")) "9" else "8"
    h4(paste0(n, ". Download Report"), style = "display:inline;")
  })

  # ---- Helper: find living_area column from variable specials ----
  find_living_area_ <- function() {
    cfg <- var_config_r()
    if (is.null(cfg) || is.null(cfg$specials)) return(NULL)
    sp <- cfg$specials
    la_idx <- which(sp == "living_area")
    if (length(la_idx) == 0) return(NULL)
    names(sp)[la_idx[1L]]
  }

  # ---- Helper: compute per-smooth-term contributions ----
  # Uses predict(model, type = "terms") which returns a matrix of
  # per-term contributions. For GAMs this is native and simple.
  compute_gam_contributions_ <- function(model, newdata) {
    # type = "terms" returns a matrix with one column per model term
    preds <- predict(model, newdata = newdata, type = "terms")
    # Convert to named list of numeric vectors
    contribs <- list()
    for (col in colnames(preds)) {
      contribs[[col]] <- as.numeric(preds[, col])
    }
    contribs
  }

  # --- 6. Download Output (Excel) ---
  observeEvent(input$export_data, {
    req(gam_result_r(), data_mod$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_mod$data()
      res       <- gam_result_r()
      model     <- res$model
      response  <- res$response
      n         <- nrow(export_df)

      # Predictions
      est_col   <- rep(NA_real_, n)
      resid_col <- rep(NA_real_, n)

      complete <- tryCatch({
        pv <- predict(model, newdata = export_df, type = "response")
        est_col <- as.numeric(pv)
        resid_col <- export_df[[response]] - est_col
        rep(TRUE, n)
      }, error = function(e) {
        message("Prediction error on some rows: ", e$message)
        # Try row by row
        ok <- logical(n)
        for (i in seq_len(n)) {
          tryCatch({
            pv <- predict(model, newdata = export_df[i, , drop = FALSE],
                          type = "response")
            est_col[i] <<- as.numeric(pv)
            resid_col[i] <<- export_df[[response]][i] - est_col[i]
            ok[i] <- TRUE
          }, error = function(e2) NULL)
        }
        ok
      })

      # Per-term contributions for complete rows
      if (any(complete)) {
        contribs <- compute_gam_contributions_(model,
                                                export_df[complete, , drop = FALSE])
        intercept <- stats::coef(model)[["(Intercept)"]]

        export_df[["basis"]] <- NA_real_
        export_df[["basis"]][complete] <- round(intercept, 1)

        for (tl in names(contribs)) {
          # Clean column name: s(var) -> var_contribution
          col_name <- gsub("^s\\((.+)\\)$", "\\1", tl)
          col_name <- gsub("^te\\((.+)\\)$", "\\1", col_name)
          col_name <- gsub("^ti\\((.+)\\)$", "\\1", col_name)
          col_name <- paste0(col_name, "_contribution")
          export_df[[col_name]] <- NA_real_
          export_df[[col_name]][complete] <- round(contribs[[tl]], 1)
        }

        # Verification column
        contrib_total <- intercept + Reduce(`+`, contribs)
        export_df[["calc_residual"]] <- NA_real_
        export_df[["calc_residual"]][complete] <-
          round(export_df[[response]][complete] - contrib_total, 1)
      }

      export_df[[paste0("est_", response)]] <- round(est_col, 1)
      export_df[["residual"]] <- round(resid_col, 1)

      # --- CQA scores ---
      la_col <- find_living_area_()
      comp_rows <- if (identical(input$purpose, "appraisal")) -1L else seq_len(n)
      comp_resid <- resid_col[comp_rows]
      comp_resid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid)
      if (n_comps > 0) {
        cqa <- vapply(resid_col, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
        export_df[["cqa"]] <- round(cqa, 2)

        if (!is.null(la_col) && la_col %in% names(export_df)) {
          resid_sf <- resid_col / export_df[[la_col]]
          export_df[["residual_sf"]] <- round(resid_sf, 4)
          comp_resid_sf <- resid_sf[comp_rows]
          comp_resid_sf <- comp_resid_sf[!is.na(comp_resid_sf)]
          n_sf <- length(comp_resid_sf)
          if (n_sf > 0) {
            cqa_sf <- vapply(resid_sf, function(r) {
              if (is.na(r)) return(NA_real_)
              sum(comp_resid_sf < r, na.rm = TRUE) / n_sf * 10
            }, numeric(1))
            export_df[["cqa_sf"]] <- round(cqa_sf, 2)
          }
        }
      }

      # In appraisal mode, set subject row (row 1) actual/residual to NA
      if (identical(input$purpose, "appraisal")) {
        export_df[["residual"]][1L] <- NA_real_
        if ("cqa" %in% names(export_df)) export_df[["cqa"]][1L] <- NA_real_
        if ("cqa_sf" %in% names(export_df)) export_df[["cqa_sf"]][1L] <- NA_real_
      }

      # Sort by residual_sf descending for appraisal/market
      if (input$purpose %in% c("appraisal", "market") &&
          "residual_sf" %in% names(export_df)) {
        ord <- order(export_df[["residual_sf"]], decreasing = TRUE,
                     na.last = TRUE)
        export_df <- export_df[ord, , drop = FALSE]
      }

      base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
      out_path <- file.path(folder, paste0(base, "_output_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      showNotification(paste0("Output saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Export error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- 7. Calculate RCA Adjustments & Download ---
  observeEvent(input$rca_output_btn, {
    req(gam_result_r(), data_mod$data())

    la_col <- find_living_area_()
    cqa_choices <- c("CQA" = "cqa")
    if (!is.null(la_col)) {
      cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")
    }

    showModal(modalDialog(
      title = "RCA Raw Output \u2014 Subject CQA Score",
      radioButtons("rca_cqa_type", "Score type:",
                   choices = cqa_choices, inline = TRUE),
      numericInput("rca_cqa_value", "Subject CQA Score:",
                   value = 5.00, min = 0, max = 9.99, step = 0.01),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("export_rca", "Generate", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$export_rca, {
    removeModal()
    req(gam_result_r(), data_mod$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_mod$data()
      res       <- gam_result_r()
      model     <- res$model
      response  <- res$response
      user_cqa  <- input$rca_cqa_value
      n         <- nrow(export_df)

      if (n < 2L) {
        showNotification("Need at least 2 rows (subject + 1 comp).",
                         type = "error")
        return()
      }

      # Predictions
      predicted <- as.numeric(predict(model, newdata = export_df,
                                      type = "response"))
      actual    <- export_df[[response]]
      residuals_val <- actual - predicted

      # Subject row (row 1): sale price treated as NA
      actual[1L] <- NA_real_
      residuals_val[1L] <- NA_real_

      export_df[[paste0("est_", response)]] <- round(predicted, 1)

      # --- CQA scores on comps (rows 2+) ---
      la_col <- find_living_area_()
      comp_resid <- residuals_val[-1L]
      comp_resid_valid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid_valid)

      cqa_col <- rep(NA_real_, n)
      if (n_comps > 0) {
        cqa_col <- vapply(residuals_val, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid_valid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
      }
      export_df[["residual"]] <- round(residuals_val, 1)
      export_df[["cqa"]] <- round(cqa_col, 2)

      # CQA_SF if living_area designated
      resid_sf <- NULL
      cqa_sf_col <- NULL
      if (!is.null(la_col) && la_col %in% names(export_df)) {
        resid_sf <- residuals_val / export_df[[la_col]]
        export_df[["residual_sf"]] <- round(resid_sf, 4)
        comp_resid_sf <- resid_sf[-1L]
        comp_resid_sf_valid <- comp_resid_sf[!is.na(comp_resid_sf)]
        n_sf <- length(comp_resid_sf_valid)
        if (n_sf > 0) {
          cqa_sf_col <- vapply(resid_sf, function(r) {
            if (is.na(r)) return(NA_real_)
            sum(comp_resid_sf_valid < r, na.rm = TRUE) / n_sf * 10
          }, numeric(1))
          export_df[["cqa_sf"]] <- round(cqa_sf_col, 2)
        }
      }

      # --- Interpolate subject residual from CQA ---
      use_sf <- (input$rca_cqa_type == "cqa_sf" && !is.null(la_col))
      if (use_sf) {
        comp_cqa_vals <- cqa_sf_col[-1L]
        comp_resid_for_interp <- resid_sf[-1L]
      } else {
        comp_cqa_vals <- cqa_col[-1L]
        comp_resid_for_interp <- residuals_val[-1L]
      }

      valid <- !is.na(comp_cqa_vals) & !is.na(comp_resid_for_interp)
      cqa_sorted   <- comp_cqa_vals[valid]
      resid_sorted <- comp_resid_for_interp[valid]
      ord <- order(cqa_sorted)
      cqa_sorted   <- cqa_sorted[ord]
      resid_sorted <- resid_sorted[ord]

      subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                     xout = user_cqa, rule = 2)$y

      # Convert per-SF back to total if needed
      if (use_sf) {
        subject_la <- export_df[[la_col]][1L]
        subject_resid_total <- subject_resid * subject_la
      } else {
        subject_resid_total <- subject_resid
      }

      # Subject value = model estimate + interpolated residual
      subject_est <- predicted[1L] + subject_resid_total
      residuals_val[1L] <- subject_resid_total
      export_df[["residual"]][1L] <- round(subject_resid_total, 1)
      export_df[["subject_value"]] <- NA_real_
      export_df[["subject_value"]][1L] <- round(subject_est, 1)
      export_df[["subject_cqa"]] <- NA_real_
      export_df[["subject_cqa"]][1L] <- user_cqa

      # --- Per-term contributions ---
      contribs <- compute_gam_contributions_(model, export_df)
      intercept <- stats::coef(model)[["(Intercept)"]]

      export_df[["basis"]] <- round(intercept, 1)

      # Clean term names for column naming
      clean_term_name_ <- function(tl) {
        nm <- gsub("^s\\((.+)\\)$", "\\1", tl)
        nm <- gsub("^te\\((.+)\\)$", "\\1", nm)
        nm <- gsub("^ti\\((.+)\\)$", "\\1", nm)
        nm
      }

      for (tl in names(contribs)) {
        col_name <- paste0(clean_term_name_(tl), "_contribution")
        export_df[[col_name]] <- round(contribs[[tl]], 1)
      }

      # --- RCA Adjustments ---
      adj_sum   <- rep(0, n)
      gross_sum <- rep(0, n)

      for (tl in names(contribs)) {
        adj_col_name <- paste0(clean_term_name_(tl), "_adjustment")
        subject_contrib <- contribs[[tl]][1L]
        adjustment <- subject_contrib - contribs[[tl]]
        export_df[[adj_col_name]] <- round(adjustment, 1)
        adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
        gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
      }

      # Residual adjustment
      resid_adj <- subject_resid_total - residuals_val
      export_df[["residual_adjustment"]] <- round(resid_adj, 1)
      adj_sum   <- adj_sum + ifelse(is.na(resid_adj), 0, resid_adj)
      gross_sum <- gross_sum + ifelse(is.na(resid_adj), 0, abs(resid_adj))

      export_df[["net_adjustments"]]   <- round(adj_sum, 1)
      export_df[["gross_adjustments"]] <- round(gross_sum, 1)

      # Adjustment percentages (adjustment / comparable sale price)
      sale_price <- export_df[[response]]
      export_df[["residual_adj_pct"]] <- round(resid_adj / sale_price * 100, 2)
      export_df[["net_adj_pct"]]      <- round(adj_sum / sale_price * 100, 2)
      export_df[["gross_adj_pct"]]    <- round(gross_sum / sale_price * 100, 2)

      export_df[["adjusted_sale_price"]] <- round(actual + adj_sum, 1)

      # Subject row: adjustments are zero (subject vs self)
      adj_cols <- grep(
        "_adjustment$|net_adjustments|gross_adjustments|adjusted_sale_price|_adj_pct$",
        names(export_df), value = TRUE)
      for (ac in adj_cols) {
        export_df[[ac]][1L] <- NA_real_
      }
      export_df[["adjusted_sale_price"]][1L] <- round(subject_est, 1)

      # Store pct data for RCA Analysis plots (comps only, exclude subject)
      rv_rca$pct_data <- data.frame(
        residual_adj_pct = export_df[["residual_adj_pct"]][-1L],
        net_adj_pct      = export_df[["net_adj_pct"]][-1L],
        gross_adj_pct    = export_df[["gross_adj_pct"]][-1L],
        stringsAsFactors = FALSE
      )

      base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
      out_path <- file.path(folder, paste0(base, "_adjusted_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      showNotification(paste0("RCA output saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("RCA error:", e$message),
                       type = "error", duration = 15)
    })
  })

  # --- RCA Analysis Plots ---
  rca_pct_histogram_ <- function(vals, title, xlab, fill_color) {
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)

    avg_val    <- mean(vals)
    median_val <- stats::median(vals)
    sd_val     <- stats::sd(vals)
    font_fam   <- mgcv_font_family_()

    # Bins in 20% increments
    lo <- floor(min(vals) / 20) * 20
    hi <- ceiling(max(vals) / 20) * 20
    breaks <- seq(lo, hi, by = 20)
    if (length(breaks) < 2) breaks <- c(lo, lo + 20)

    df <- data.frame(x = vals)
    subtitle <- sprintf("Mean: %.2f%%    Median: %.2f%%    Std Dev: %.2f%%",
                        avg_val, median_val, sd_val)

    ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
      ggplot2::geom_histogram(breaks = breaks, fill = fill_color,
                              color = "white", alpha = 0.85) +
      ggplot2::geom_vline(xintercept = avg_val, linetype = "dashed",
                          color = "#2e3440", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = median_val, linetype = "dotted",
                          color = "#5e81ac", linewidth = 0.8) +
      ggplot2::annotate("text", x = avg_val, y = Inf, label = "Mean",
                        vjust = 2, hjust = -0.15, size = 3.5,
                        color = "#2e3440", family = font_fam) +
      ggplot2::annotate("text", x = median_val, y = Inf, label = "Median",
                        vjust = 3.5, hjust = -0.15, size = 3.5,
                        color = "#5e81ac", family = font_fam) +
      ggplot2::scale_x_continuous(breaks = breaks,
                                  labels = paste0(breaks, "%")) +
      ggplot2::labs(title = title, subtitle = subtitle,
                    x = xlab, y = "Frequency") +
      ggplot2::theme_minimal(base_family = font_fam) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "#4c566a"),
        axis.text.x = ggplot2::element_text(angle = 0)
      )
  }

  output$rca_resid_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$residual_adj_pct,
                       "Residual Adjustment %",
                       "Residual Adj. %", "#88c0d0")
  })

  output$rca_net_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$net_adj_pct,
                       "Net Adjustment %",
                       "Net Adj. %", "#5e81ac")
  })

  output$rca_gross_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$gross_adj_pct,
                       "Gross Adjustment %",
                       "Gross Adj. %", "#a3be8c")
  })

  # --- 8. Download Report (to output folder) ---
  observeEvent(input$export_report_btn, {
    req(gam_result_r())

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    fmt <- input$export_format
    ext <- if (fmt == "docx") ".docx" else ".pdf"
    base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
    out_path <- file.path(folder, paste0(base, "_report_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ext))

    tryCatch({
      res   <- gam_result_r()
      model <- res$model

      if (fmt == "docx") {
        # Word document via officer
        font_fam <- mgcv_font_family_()
        tmpdir <- tempdir()

        # Smooth plots
        for (spec in res$smooth_specs) {
          if (spec$type == "linear") next
          var <- spec$vars[1]
          p <- tryCatch(
            plot_smooth_single(res, var, earth_knots = res$earth_knots),
            error = function(e) NULL
          )
          if (!is.null(p)) {
            ggplot2::ggsave(file.path(tmpdir, paste0("smooth_", var, ".png")),
                            p, width = 7, height = 4, dpi = 150)
          }
        }

        # Diagnostics
        p_diag <- tryCatch(plot_diagnostics(res), error = function(e) NULL)
        if (!is.null(p_diag)) {
          ggplot2::ggsave(file.path(tmpdir, "diagnostics.png"), p_diag,
                          width = 8, height = 6, dpi = 150)
        }

        # Actual vs predicted
        p_avp <- tryCatch(plot_actual_vs_predicted(res), error = function(e) NULL)
        if (!is.null(p_avp)) {
          ggplot2::ggsave(file.path(tmpdir, "actual_vs_predicted.png"), p_avp,
                          width = 7, height = 5, dpi = 150)
        }

        # Build Word document
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "mgcvUI GAM Report",
                                     style = "heading 1")
        doc <- officer::body_add_par(doc, paste("Date:",
                                     format(Sys.time(), "%Y-%m-%d %H:%M")))
        doc <- officer::body_add_par(doc, "")

        # Model summary
        doc <- officer::body_add_par(doc, "Model Summary", style = "heading 2")
        summ <- format_gam_summary(res)
        doc <- officer::body_add_par(doc, paste("R-squared:",
                                     round(summ$r_squared, 4)))
        doc <- officer::body_add_par(doc, paste("Deviance explained:",
                                     round(summ$dev_explained * 100, 1), "%"))
        doc <- officer::body_add_par(doc, paste("AIC:", round(summ$aic, 1)))
        doc <- officer::body_add_par(doc, paste("n:", summ$n_obs))
        if (!is.null(summ$cv_rsq)) {
          doc <- officer::body_add_par(doc, paste("CV R-squared:",
                                       round(summ$cv_rsq, 4)))
        }
        doc <- officer::body_add_par(doc, paste("Family:", summ$family))
        doc <- officer::body_add_par(doc, paste("Method:", summ$method))
        doc <- officer::body_add_par(doc, paste("Formula:",
                                     deparse(res$formula, width.cutoff = 500)))

        # Smooth terms table
        if (nrow(summ$smooth_table) > 0) {
          doc <- officer::body_add_par(doc, "Smooth Terms", style = "heading 2")
          doc <- officer::body_add_table(doc, value = summ$smooth_table,
                                         style = "table_template")
        }

        # Parametric terms table
        if (nrow(summ$parametric_table) > 0) {
          doc <- officer::body_add_par(doc, "Parametric Terms",
                                       style = "heading 2")
          doc <- officer::body_add_table(doc, value = summ$parametric_table,
                                         style = "table_template")
        }

        # Plots
        doc <- officer::body_add_par(doc, "Smooth Plots", style = "heading 2")
        for (spec in res$smooth_specs) {
          if (spec$type == "linear") next
          var <- spec$vars[1]
          fp <- file.path(tmpdir, paste0("smooth_", var, ".png"))
          if (file.exists(fp)) {
            doc <- officer::body_add_img(doc, src = fp, width = 6, height = 3.5)
            doc <- officer::body_add_par(doc, "")
          }
        }

        doc <- officer::body_add_par(doc, "Diagnostics", style = "heading 2")
        fp <- file.path(tmpdir, "diagnostics.png")
        if (file.exists(fp)) {
          doc <- officer::body_add_img(doc, src = fp, width = 7, height = 5.25)
        }

        doc <- officer::body_add_par(doc, "Actual vs Predicted",
                                     style = "heading 2")
        fp <- file.path(tmpdir, "actual_vs_predicted.png")
        if (file.exists(fp)) {
          doc <- officer::body_add_img(doc, src = fp, width = 6, height = 4.3)
        }

        print(doc, target = out_path)

      } else {
        # PDF via rmarkdown
        rmd_template <- system.file("rmd", "gam_report.Rmd",
                                     package = "mgcvUI")
        if (nzchar(rmd_template)) {
          tmpdir <- tempdir()
          rmd_copy <- file.path(tmpdir, "gam_report.Rmd")
          file.copy(rmd_template, rmd_copy, overwrite = TRUE)

          # Save result to temp RDS for template
          rds_path <- file.path(tmpdir, "gam_result.rds")
          saveRDS(res, rds_path)

          tmp_out <- file.path(tmpdir, paste0("gam_report", ext))
          rmarkdown::render(
            rmd_copy,
            output_file = tmp_out,
            params = list(result_path = rds_path,
                          title = "mgcvUI GAM Report"),
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
          file.copy(tmp_out, out_path, overwrite = TRUE)
        } else {
          stop("Report template not found.")
        }
      }

      showNotification(paste0("Report saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Report error:", e$message),
                       type = "error", duration = 10)
    })
  })
}

shinyApp(ui, server)
