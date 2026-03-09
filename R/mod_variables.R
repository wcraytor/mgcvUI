#' Variable Selection Module — UI
#'
#' Compact variable table with Inc, Type, and Linear checkboxes,
#' plus an Allowed Interactions matrix. Designed for sidebar embedding.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_variables_ui("vars1"))
#' }
mod_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("response"), "Target (response)",
                choices = NULL, width = "100%"),
    selectInput(ns("weights_col"), "Weights column (optional)",
                choices = c("(none)" = ""), width = "100%"),
    tags$p("Check Inc to include a predictor. Linear forces linear entry. Special designates column role.",
           style = "font-size: 0.8em; color: #888; margin-bottom: 4px;"),
    div(style = paste("max-height: 400px; overflow-y: auto;",
                      "border: 1px solid #ddd; border-radius: 4px;",
                      "padding: 4px;"),
      uiOutput(ns("var_table"))
    ),
    br(),
    tags$details(
      tags$summary(
        style = "cursor: pointer; font-weight: bold; font-size: 0.9em;",
        "Allowed Interactions"
      ),
      div(style = "padding-top: 6px;",
        fluidRow(
          column(6, actionButton(ns("allow_all_int"), "Allow All",
                                 class = "btn-sm btn-outline-success",
                                 style = "width:100%;")),
          column(6, actionButton(ns("clear_all_int"), "Clear All",
                                 class = "btn-sm btn-outline-danger",
                                 style = "width:100%;"))
        ),
        br(),
        uiOutput(ns("allowed_matrix_ui"))
      )
    )
  )
}


#' Mgcv Call Parameters — UI
#'
#' Model fitting parameters for the GAM call. Uses the same module
#' namespace as [mod_variables_ui()] so inputs are shared.
#'
#' @param id Shiny module namespace ID (must match [mod_variables_ui()]).
#' @return A [shiny::tagList].
#' @export
mod_variables_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("family"), "Family",
                choices = c("gaussian", "Gamma", "poisson",
                            "binomial", "inverse.gaussian"),
                selected = "gaussian", width = "100%"),
    selectInput(ns("method"), "Method",
                choices = c("REML", "GCV.Cp", "ML", "GACV.Cp"),
                selected = "REML", width = "100%"),
    numericInput(ns("gamma"), "Gamma (smoothing penalty)",
                 value = 1, min = 0.1, max = 10, step = 0.1,
                 width = "100%"),
    checkboxInput(ns("cv"), "Cross-validate (10-fold CV R\u00b2)",
                  value = TRUE),
    checkboxInput(ns("select"), "Variable selection penalty",
                  value = FALSE),
    selectInput(ns("default_basis"), "Default basis",
                choices = c("tp", "cr", "ps", "bs"),
                selected = "tp", width = "100%"),
    numericInput(ns("default_k"), "Default k (0 = auto)",
                 value = 0, min = 0, max = 100, step = 1,
                 width = "100%"),
    selectInput(ns("tensor_type"), "Tensor type for interactions",
                choices = c("ti (tensor interaction)" = "ti",
                            "te (tensor product)" = "te"),
                selected = "ti", width = "100%")
  )
}


#' Variable Selection Module — Server
#'
#' @param id Shiny module namespace ID.
#' @param data_r A reactive returning the imported data frame.
#' @param filename_r A reactive returning the original filename.
#' @param earth_knots_r A reactive returning an `mgcvUI_earth_knots`
#'   object (or `NULL`).
#' @return A reactive list with components: `response`, `predictors`,
#'   `smooth_specs`, `family`, `method`, `select`, `gamma`.
#' @export
mod_variables_server <- function(id, data_r, filename_r = reactive(NULL),
                                 earth_knots_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    var_state <- reactiveVal(list())

    # Update response choices when data changes
    observeEvent(data_r(), {
      df <- data_r()
      req(df)
      num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
      fname <- filename_r()
      saved <- if (!is.null(fname)) settings_db_read_(fname) else NULL

      if (!is.null(saved) && !is.null(saved$response) &&
            saved$response %in% num_vars) {
        updateSelectInput(session, "response", choices = num_vars,
                          selected = saved$response)
        if (!is.null(saved$family)) {
          updateSelectInput(session, "family", selected = saved$family)
        }
        if (!is.null(saved$method)) {
          updateSelectInput(session, "method", selected = saved$method)
        }
        if (!is.null(saved$select)) {
          updateCheckboxInput(session, "select", value = saved$select)
        }
        if (!is.null(saved$gamma)) {
          updateNumericInput(session, "gamma", value = saved$gamma)
        }
      } else {
        updateSelectInput(session, "response", choices = num_vars)
      }

      # Weights column: any numeric column, plus "none"
      wt_choices <- c("(none)" = "", num_vars)
      wt_selected <- if (!is.null(saved) && !is.null(saved$weights_col) &&
                          saved$weights_col %in% num_vars) {
        saved$weights_col
      } else {
        ""
      }
      updateSelectInput(session, "weights_col", choices = wt_choices,
                        selected = wt_selected)
    })

    # Render the variable table
    output$var_table <- renderUI({
      df <- data_r()
      req(df)
      cols <- names(df)
      detected <- detect_column_types(df)

      fname <- filename_r()
      saved <- if (!is.null(fname)) settings_db_read_(fname) else NULL
      ek <- earth_knots_r()

      type_choices <- c("numeric", "integer", "character", "factor",
                        "logical", "Date", "POSIXct")
      special_choices <- c("no", "contract_date", "latitude", "longitude",
                           "living_area", "display_only")

      rows <- lapply(seq_along(cols), function(i) {
        var <- cols[i]
        det_type <- unname(detected[i])

        # Defaults: unchecked, auto-detected type, not linear, no special
        inc_val     <- FALSE
        type_val    <- det_type
        lin_val     <- FALSE
        special_val <- "no"

        # Restore from saved settings
        if (!is.null(saved) && !is.null(saved$variables[[var]])) {
          sv <- saved$variables[[var]]
          if (!is.null(sv$inc)) inc_val <- isTRUE(sv$inc)
          if (!is.null(sv$type) && sv$type %in% type_choices) {
            type_val <- sv$type
          }
          if (!is.null(sv$linear)) lin_val <- isTRUE(sv$linear)
          if (!is.null(sv$special) && sv$special %in% special_choices) {
            special_val <- sv$special
          }
        }

        # Earth import overrides (always applied when earth knots present)
        if (!is.null(ek)) {
          if (var %in% ek$predictors) {
            inc_val <- TRUE
          }
          if (var %in% ek$linpreds) {
            lin_val <- TRUE
          }
          if (var %in% ek$categoricals) {
            type_val <- "factor"
          }
        }

        # Auto-exclude variables with > 50% NAs
        pct_na_var <- sum(is.na(df[[var]])) / nrow(df)
        if (pct_na_var > 0.5) {
          inc_val <- FALSE
        }

        type_opts <- paste(vapply(type_choices, function(t) {
          sel <- if (t == type_val) " selected" else ""
          sprintf("<option value=\"%s\"%s>%s</option>", t, sel, t)
        }, character(1)), collapse = "")

        special_opts <- paste(vapply(special_choices, function(s) {
          sel <- if (s == special_val) " selected" else ""
          sprintf("<option value=\"%s\"%s>%s</option>", s, sel, s)
        }, character(1)), collapse = "")

        inc_chk <- if (inc_val) " checked" else ""
        lin_chk <- if (lin_val) " checked" else ""

        # NA count display
        n_na <- sum(is.na(df[[var]]))
        pct_na <- round(n_na / nrow(df) * 100, 0)
        na_color <- if (pct_na >= 50) "#bf616a"
                    else if (pct_na >= 20) "#d08770"
                    else if (n_na > 0) "#888"
                    else "#aaa"
        na_label <- if (n_na == 0) "\u2014"
                    else paste0(n_na, " (", pct_na, "%)")
        na_html <- sprintf(
          "<span style='color:%s;' title='%d of %d rows are NA'>%s</span>",
          na_color, n_na, nrow(df), na_label)

        tags$div(
          class = "mgcv-var-row", `data-var` = var,
          tags$div(class = "mgcv-cell mgcv-cell-name", title = var, var),
          tags$div(class = "mgcv-cell mgcv-cell-na", HTML(na_html)),
          tags$div(
            class = "mgcv-cell mgcv-cell-inc",
            HTML(paste0("<input type='checkbox'",
                        " class='mgcv-inc'",
                        " data-var='", var, "'",
                        inc_chk, ">"))
          ),
          tags$div(
            class = "mgcv-cell mgcv-cell-type",
            HTML(paste0("<select class='mgcv-type'",
                        " data-var='", var, "'>",
                        type_opts, "</select>"))
          ),
          tags$div(
            class = "mgcv-cell mgcv-cell-linear",
            HTML(paste0("<input type='checkbox'",
                        " class='mgcv-linear'",
                        " data-var='", var, "'",
                        lin_chk, ">"))
          ),
          tags$div(
            class = "mgcv-cell mgcv-cell-special",
            HTML(paste0("<select class='mgcv-special'",
                        " data-var='", var, "'>",
                        special_opts, "</select>"))
          )
        )
      })

      header <- tags$div(
        class = "mgcv-var-row mgcv-var-header",
        tags$div(class = "mgcv-cell mgcv-cell-name", "Variable"),
        tags$div(class = "mgcv-cell mgcv-cell-na", "NAs"),
        tags$div(class = "mgcv-cell mgcv-cell-inc", "Inc"),
        tags$div(class = "mgcv-cell mgcv-cell-type", "Type"),
        tags$div(class = "mgcv-cell mgcv-cell-linear", "Lin"),
        tags$div(class = "mgcv-cell mgcv-cell-special", "Special")
      )

      js <- tags$script(HTML(sprintf("
        (function() {
          var ns = '%s';
          function gatherState() {
            var state = {};
            document.querySelectorAll('.mgcv-var-row[data-var]').forEach(
              function(row) {
                var v = row.getAttribute('data-var');
                state[v] = {
                  inc:     row.querySelector('.mgcv-inc').checked,
                  type:    row.querySelector('.mgcv-type').value,
                  linear:  row.querySelector('.mgcv-linear').checked,
                  special: row.querySelector('.mgcv-special').value
                };
              }
            );
            Shiny.setInputValue(ns + 'var_state', state, {priority:'event'});
          }
          $(document).off('change.mgcvVars')
            .on('change.mgcvVars',
                '.mgcv-inc, .mgcv-type, .mgcv-linear, .mgcv-special',
                gatherState);
          setTimeout(gatherState, 200);
        })();
      ", ns(""))))

      tagList(header, rows, js)
    })

    # When earth knots arrive, re-set response
    observeEvent(earth_knots_r(), {
      ek <- earth_knots_r()
      if (!is.null(ek) && length(ek$target) == 1L) {
        df <- data_r()
        if (!is.null(df) && ek$target %in% names(df)) {
          updateSelectInput(session, "response", selected = ek$target)
        }
      }
    })

    # --- Allowed Interactions Matrix ---
    output$allowed_matrix_ui <- renderUI({
      st <- var_state()
      resp <- input$response
      if (length(st) == 0L) return(NULL)

      # Included smooth (non-linear numeric) variables
      included <- character(0)
      for (var in names(st)) {
        s <- st[[var]]
        if (isTRUE(s$inc) && !identical(var, resp)) {
          typ <- s$type %||% "numeric"
          if (typ %in% c("numeric", "integer", "Date", "POSIXct") &&
              !isTRUE(s$linear)) {
            included <- c(included, var)
          }
        }
      }

      if (length(included) < 2L) {
        return(tags$p("Need 2+ smooth predictors for interactions.",
                      style = "color: #888; font-size: 0.85em;"))
      }

      n <- length(included)
      ek <- earth_knots_r()

      # Info message when earth used degree == 1
      earth_deg1_msg <- NULL
      if (!is.null(ek) && identical(ek$degree, 1L) &&
            length(ek$interactions) == 0L) {
        earth_deg1_msg <- tags$p(
          "Earth model used degree = 1 (no interactions detected).",
          style = "color: #b8860b; font-size: 0.85em; margin-bottom: 6px;"
        )
      }

      # Header row with rotated column names (clickable to toggle)
      header_cells <- list(tags$th(""))
      for (j in seq_len(n)) {
        header_cells[[j + 1L]] <- tags$th(
          tags$div(included[j],
            class = "mgcv-int-var-toggle",
            `data-var` = included[j],
            style = paste("writing-mode: vertical-rl;",
                          "transform: rotate(180deg);",
                          "font-size: 0.75em;",
                          "white-space: nowrap;",
                          "padding: 4px 2px;",
                          "cursor: pointer;")),
          style = "text-align: center; vertical-align: bottom;"
        )
      }

      # Body rows
      body_rows <- list()
      for (i in seq_len(n)) {
        cells <- list(
          tags$td(
            tags$span(included[i],
              class = "mgcv-int-var-toggle",
              `data-var` = included[i]),
            style = paste("font-size: 0.8em; padding: 2px 6px;",
                          "white-space: nowrap; font-weight: 500;",
                          "cursor: pointer;"))
        )
        for (j in seq_len(n)) {
          if (j <= i) {
            cells[[j + 1L]] <- tags$td(
              if (i == j) "\u00b7" else "",
              style = "text-align: center; padding: 2px; color: #aaa;"
            )
          } else {
            # Check if earth detected this interaction or allowed it
            pair <- sort(c(included[i], included[j]))
            key <- paste(pair, collapse = ":")
            earth_detected <- !is.null(ek) &&
              !is.null(ek$interactions) &&
              key %in% names(ek$interactions)
            # Also check earthUI allowed_matrix
            earth_allowed <- FALSE
            if (!is.null(ek) && !is.null(ek$allowed_matrix)) {
              am <- ek$allowed_matrix
              cn <- colnames(am)
              if (all(pair %in% cn)) {
                earth_allowed <- isTRUE(
                  am[pair[1L], pair[2L]] == 1 ||
                  am[pair[2L], pair[1L]] == 1
                )
              }
            }
            chk <- if (earth_detected || earth_allowed) " checked" else ""

            cells[[j + 1L]] <- tags$td(
              style = "text-align: center; padding: 2px;",
              HTML(paste0("<input type='checkbox' class='mgcv-int-cb'",
                          " data-var1='", included[i], "'",
                          " data-var2='", included[j], "'",
                          chk, ">"))
            )
          }
        }
        body_rows[[i]] <- do.call(tags$tr, cells)
      }

      # JavaScript to gather checkbox state
      js <- tags$script(HTML(sprintf("
        (function() {
          var ns = '%s';
          function gatherInteractions() {
            var state = {};
            document.querySelectorAll('.mgcv-int-cb').forEach(function(cb) {
              var v1 = cb.getAttribute('data-var1');
              var v2 = cb.getAttribute('data-var2');
              var pair = [v1, v2].sort();
              state[pair.join(':')] = cb.checked;
            });
            Shiny.setInputValue(ns + 'interaction_matrix',
                                state, {priority:'event'});
          }
          Shiny.addCustomMessageHandler('mgcv_toggle_all_int',
            function(checked) {
              document.querySelectorAll('.mgcv-int-cb').forEach(
                function(cb) { cb.checked = checked; });
              gatherInteractions();
            });
          $(document).off('change.mgcvInts')
            .on('change.mgcvInts', '.mgcv-int-cb', gatherInteractions);
          $(document).off('click.mgcvVarToggle')
            .on('click.mgcvVarToggle', '.mgcv-int-var-toggle', function() {
              var varName = $(this).attr('data-var');
              var cbs = document.querySelectorAll(
                '.mgcv-int-cb[data-var1=\"' + varName + '\"],' +
                '.mgcv-int-cb[data-var2=\"' + varName + '\"]'
              );
              // If any are unchecked, check all; otherwise uncheck all
              var anyUnchecked = false;
              cbs.forEach(function(cb) { if (!cb.checked) anyUnchecked = true; });
              cbs.forEach(function(cb) { cb.checked = anyUnchecked; });
              gatherInteractions();
            });
          setTimeout(gatherInteractions, 300);
        })();
      ", ns(""))))

      tagList(
        earth_deg1_msg,
        tags$div(
          style = paste("overflow-x: auto; max-height: 300px;",
                        "overflow-y: auto; border: 1px solid #ddd;",
                        "border-radius: 4px; padding: 4px;"),
          tags$table(
            style = "border-collapse: collapse;",
            tags$thead(do.call(tags$tr, header_cells)),
            tags$tbody(body_rows)
          )
        ),
        js
      )
    })

    # Allow All / Clear All buttons
    observeEvent(input$allow_all_int, {
      session$sendCustomMessage("mgcv_toggle_all_int", TRUE)
    })
    observeEvent(input$clear_all_int, {
      session$sendCustomMessage("mgcv_toggle_all_int", FALSE)
    })

    # Save settings (merge with existing to preserve app-level fields)
    save_settings_ <- function(fname, st) {
      existing <- settings_db_read_(fname)
      config <- if (!is.null(existing)) existing else list()
      config$response    <- input$response
      config$variables   <- st
      config$family      <- input$family
      config$method      <- input$method
      config$select      <- input$select
      config$gamma       <- input$gamma
      config$weights_col <- input$weights_col %||% ""
      settings_db_write_(fname, config)
    }

    observeEvent(input$var_state, {
      var_state(input$var_state)
      fname <- filename_r()
      if (!is.null(fname)) {
        save_settings_(fname, input$var_state)
      }
    })

    observe({
      input$response
      input$family
      input$method
      input$select
      input$gamma
      input$weights_col
      fname <- isolate(filename_r())
      st <- isolate(var_state())
      if (!is.null(fname) && length(st) > 0L) {
        save_settings_(fname, st)
      }
    })

    # Return configuration
    reactive({
      st <- var_state()
      resp <- input$response
      default_bs <- input$default_basis %||% "tp"
      default_k_raw <- input$default_k %||% 0
      default_k <- if (default_k_raw == 0) NULL else as.integer(default_k_raw)

      if (length(st) == 0L) {
        return(list(
          response = resp, predictors = character(0),
          smooth_specs = list(), family = input$family,
          method = input$method, select = input$select,
          gamma = input$gamma, cv = input$cv
        ))
      }

      ek <- earth_knots_r()
      included <- character(0)
      specials <- character(0)
      for (var in names(st)) {
        if (isTRUE(st[[var]]$inc) && !identical(var, resp)) {
          included <- c(included, var)
        }
        sp <- st[[var]]$special %||% "no"
        if (sp != "no") {
          specials <- c(specials, stats::setNames(sp, var))
        }
      }

      specs <- lapply(included, function(var) {
        s <- st[[var]]
        typ <- s$type %||% "numeric"
        is_numeric <- typ %in% c("numeric", "integer", "Date", "POSIXct")

        if (!is_numeric || isTRUE(s$linear)) {
          list(vars = var, type = "linear", bs = NULL, k = NULL)
        } else {
          # Use earth knots basis if available
          has_knots <- !is.null(ek) && var %in% names(ek$knots)
          bs <- if (has_knots) "cr" else default_bs
          k <- if (has_knots) {
            length(ek$knots[[var]])
          } else {
            default_k
          }
          list(vars = var, type = "s", bs = bs, k = k)
        }
      })

      # Add interaction specs from allowed matrix
      int_matrix <- input$interaction_matrix
      tensor_type <- input$tensor_type %||% "ti"
      if (!is.null(int_matrix) && length(int_matrix) > 0L) {
        for (key in names(int_matrix)) {
          if (isTRUE(int_matrix[[key]])) {
            pair <- strsplit(key, ":")[[1L]]
            if (all(pair %in% included)) {
              specs <- c(specs, list(
                list(vars = pair, type = tensor_type, bs = NULL, k = NULL)
              ))
            }
          }
        }
      }

      wt_col <- input$weights_col
      if (is.null(wt_col) || !nzchar(wt_col)) wt_col <- NULL

      list(
        response     = resp,
        predictors   = included,
        smooth_specs = specs,
        family       = input$family,
        method       = input$method,
        select       = input$select,
        gamma        = input$gamma,
        cv           = input$cv,
        specials     = specials,
        weights_col  = wt_col
      )
    })
  })
}
