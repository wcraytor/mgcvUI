#' Model Fit Button — UI (sidebar)
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_model_fit_ui("model1"))
#' }
mod_model_fit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("fit"), "Fit Model",
                 class = "btn-success btn-lg",
                 style = "width: 100%;"),
    textOutput(ns("fit_status"))
  )
}


#' Model Results — UI (main panel)
#'
#' Tabbed output panel for model results, plots, and diagnostics.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
mod_model_results_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    condition = "output.data_imported",
    tabsetPanel(
      id = ns("results_tabs"),
      tabPanel("Data",
        br(),
        DT::DTOutput("data_preview_table")
      ),
      tabPanel("Equation",
        br(),
        div(style = "overflow-x: auto; padding: 10px 10px 10px 0;",
            uiOutput(ns("equation_display"))),
        hr(),
        h5("Smooth Function Definitions"),
        DT::DTOutput(ns("smooth_specs_table"))
      ),
      tabPanel("Summary",
        br(),
        uiOutput(ns("summary_metrics")),
        hr(),
        h5("Smooth Terms"),
        DT::DTOutput(ns("smooth_table")),
        h5("Parametric Terms"),
        DT::DTOutput(ns("param_table"))
      ),
      tabPanel("Variable Importance",
        br(),
        plotOutput(ns("importance_plot"), height = "400px"),
        br(),
        DT::DTOutput(ns("importance_table"))
      ),
      tabPanel("Contribution",
        br(),
        uiOutput(ns("smooth_plots_container"))
      ),
      tabPanel("Correlation",
        br(),
        uiOutput(ns("correlation_plot_ui"))
      ),
      tabPanel("Diagnostics",
        br(),
        plotOutput(ns("diagnostics_plot"), height = "550px"),
        hr(),
        plotOutput(ns("avp_plot"), height = "400px")
      ),
      tabPanel("RCA Adjustments",
        br(),
        conditionalPanel(
          condition = "!output.rca_computed",
          tags$div(
            style = "text-align:center; padding:40px 20px;",
            tags$p("Run RCA Adjustments (Step 8) to see analysis.",
                   style = "color: var(--bs-secondary-color);")
          )
        ),
        conditionalPanel(
          condition = "output.rca_computed",
          plotOutput("rca_resid_pct_plot", height = "350px"),
          hr(),
          plotOutput("rca_net_pct_plot", height = "350px"),
          hr(),
          plotOutput("rca_gross_pct_plot", height = "350px")
        )
      ),
      tabPanel("Anova",
        br(),
        DT::DTOutput(ns("anova_table"))
      ),
      tabPanel("Mgcv Output",
        br(),
        div(style = "overflow-x: auto;",
            verbatimTextOutput(ns("mgcv_output")))
      ),
      tabPanel("Sign Check",
        br(),
        DT::DTOutput(ns("sign_table")),
        textOutput(ns("sign_note"))
      ),
      tabPanel("Basis Check",
        br(),
        verbatimTextOutput(ns("gam_check"))
      )
    )
  )
}


#' Model Module — Server (shared by fit button and results)
#'
#' @param id Shiny module namespace ID.
#' @param data_r A reactive returning the data frame.
#' @param var_config_r A reactive returning the variable configuration.
#' @param earth_knots_r A reactive returning earth knots (or `NULL`).
#' @return A reactive containing the `mgcvUI_result`, or `NULL`.
#' @export
mod_model_server <- function(id, data_r, var_config_r,
                             earth_knots_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- reactiveVal(NULL)

    output$has_model <- reactive(!is.null(result()))
    outputOptions(output, "has_model", suspendWhenHidden = FALSE)

    observeEvent(input$fit, {
      df <- data_r()
      cfg <- var_config_r()
      req(df, cfg$response, cfg$smooth_specs)

      if (length(cfg$smooth_specs) == 0L) {
        showNotification("Select at least one predictor (check Inc).",
                         type = "warning")
        return()
      }

      # Pre-fit check: how many complete cases?
      all_vars <- unique(c(cfg$response,
        unlist(lapply(cfg$smooth_specs, `[[`, "vars"))))
      n_total <- nrow(df)
      n_complete <- sum(stats::complete.cases(df[, all_vars, drop = FALSE]))
      pct <- round(n_complete / n_total * 100, 1)

      if (n_complete < 10L) {
        showNotification(
          paste0("Only ", n_complete, " complete rows (", pct,
                 "%). Too few to fit. Exclude variables with many NAs."),
          type = "error", duration = 15)
        return()
      }

      if (pct < 50) {
        # Identify which variables cause the most row loss
        na_counts <- vapply(all_vars, function(v) sum(is.na(df[[v]])),
                            integer(1))
        na_counts <- sort(na_counts[na_counts > 0], decreasing = TRUE)
        culprits <- paste(names(na_counts), " (", na_counts, " NAs)",
                          sep = "", collapse = ", ")
        showNotification(
          paste0("Warning: only ", n_complete, " of ", n_total,
                 " rows (", pct, "%) are complete. ",
                 "NA culprits: ", culprits,
                 ". Consider excluding or unchecking these variables."),
          type = "warning", duration = 20)
      }

      tryCatch({
        withProgress(message = "Fitting GAM...", {
          # Log specs to console for debugging
          message("mgcvUI: fitting with ", length(cfg$smooth_specs),
                  " terms, response='", cfg$response, "'",
                  ", family='", cfg$family, "'",
                  ", method='", cfg$method, "'")
          for (si in seq_along(cfg$smooth_specs)) {
            sp <- cfg$smooth_specs[[si]]
            message("  spec[", si, "]: vars=",
                    paste(sp$vars, collapse = ","),
                    " type=", sp$type,
                    " bs=", sp$bs %||% "NULL",
                    " k=", sp$k %||% "NULL")
          }

          # Extract weights vector if specified
          wt <- NULL
          if (!is.null(cfg$weights_col) && cfg$weights_col %in% names(df)) {
            wt <- df[[cfg$weights_col]]
          }

          res <- fit_gam(
            data         = df,
            response     = cfg$response,
            smooth_specs = cfg$smooth_specs,
            family       = cfg$family,
            method       = cfg$method,
            earth_knots  = earth_knots_r(),
            select       = cfg$select,
            gamma        = cfg$gamma,
            cv_folds     = if (isTRUE(cfg$cv)) 10L else 0L,
            weights      = wt
          )
        })
        result(res)

        smooth_vars <- vapply(res$smooth_specs, function(s) {
          if (s$type != "linear") s$vars[1] else NA_character_
        }, character(1))
        smooth_vars <- smooth_vars[!is.na(smooth_vars)]

        showNotification(
          paste("Model fitted in", round(res$elapsed, 2), "seconds"),
          type = "message"
        )
      }, error = function(e) {
        message("mgcvUI fitting error: ", e$message)
        message("  traceback: ", paste(deparse(e$call), collapse = " "))
        showNotification(paste("Fitting error:", e$message),
                         type = "error", duration = 10)
      })
    })

    output$fit_status <- renderText({
      res <- result()
      if (is.null(res)) return("")
      tryCatch({
        summ <- format_gam_summary(res)
        cv_part <- if (!is.null(summ$cv_rsq)) {
          paste0("  CVR\u00b2=", round(summ$cv_rsq, 4))
        } else {
          ""
        }
        paste0("R\u00b2=", round(summ$r_squared, 4),
               cv_part,
               "  Dev=", round(summ$dev_explained * 100, 1), "%",
               "  AIC=", round(summ$aic, 1),
               "  n=", summ$n_obs)
      }, error = function(e) {
        message("mgcvUI format_gam_summary error: ", e$message)
        paste("Summary error:", e$message)
      })
    })

    # --- Equation tab ---
    output$equation_display <- renderUI({
      res <- result()
      req(res)
      model <- res$model
      response <- res$response

      fam <- model$family
      link_name <- fam$link

      # No underscore escaping needed inside \text{} in MathJax
      tex_esc <- function(x) x

      # LHS with link function
      resp_tex <- tex_esc(response)
      if (link_name == "identity") {
        lhs <- paste0("\\widehat{\\text{", resp_tex, "}}")
      } else if (link_name == "log") {
        lhs <- paste0("\\log\\bigl(\\widehat{\\text{", resp_tex, "}}\\bigr)")
      } else if (link_name == "inverse") {
        lhs <- paste0("\\frac{1}{\\widehat{\\text{", resp_tex, "}}}")
      } else {
        lhs <- paste0("\\text{", link_name, "}\\bigl(\\widehat{\\text{",
                       resp_tex, "}}\\bigr)")
      }

      # Intercept
      intercept <- stats::coef(model)[["(Intercept)"]]
      parts <- paste0(lhs, " = ", round(intercept, 4))

      # Smooth terms — use f_i notation
      smooth_defs <- list()
      for (idx in seq_along(model$smooth)) {
        sm <- model$smooth[[idx]]
        var_names <- tex_esc(sm$term)
        fn_label <- paste0("f_{", idx, "}")
        parts <- paste0(parts, " + ", fn_label,
                        "(\\text{", paste(var_names, collapse = ", "), "})")

        # Build definition string
        bs_label <- sm$bs.dim
        sm_class <- class(sm)[1L]
        bs_type <- if (grepl("tp", sm_class)) {
          "thin plate regression spline"
        } else if (grepl("cr", sm_class)) {
          "cubic regression spline"
        } else if (grepl("ps", sm_class)) {
          "P-spline"
        } else if (grepl("tensor", sm_class) || grepl("t2", sm_class)) {
          "tensor product smooth"
        } else {
          sm_class
        }
        var_plain <- paste(sm$term, collapse = ", ")
        k_info <- paste0("k = ", sm$bs.dim)
        smooth_defs[[idx]] <- list(
          fn = paste0("f_", idx),
          label = sm$label,
          desc = paste0(bs_type, ", ", k_info),
          vars = var_plain
        )
      }

      # Parametric terms (excluding intercept)
      # Group factor levels under one term per factor variable
      sm_obj <- summary(model)
      if (!is.null(sm_obj$p.table) && nrow(sm_obj$p.table) > 1L) {
        param_names <- rownames(sm_obj$p.table)
        param_names <- param_names[param_names != "(Intercept)"]
        all_coefs <- stats::coef(model)

        # Identify factor variables from model frame
        mf <- stats::model.frame(model)
        factor_vars <- names(mf)[vapply(mf, is.factor, logical(1))]

        # Track which factor variables we've already added
        factors_shown <- character(0)

        for (pn in param_names) {
          # Check if this param belongs to a factor variable
          matched_factor <- NULL
          for (fv in factor_vars) {
            if (startsWith(pn, fv)) {
              matched_factor <- fv
              break
            }
          }

          if (!is.null(matched_factor)) {
            if (matched_factor %in% factors_shown) next
            factors_shown <- c(factors_shown, matched_factor)
            n_levels <- nlevels(mf[[matched_factor]])
            parts <- paste0(parts,
              " + \\beta_{\\text{", tex_esc(matched_factor),
              "}} \\text{ (", n_levels, " levels)}")
          } else {
            coef_val <- all_coefs[[pn]]
            pn_tex <- tex_esc(pn)
            if (coef_val >= 0) {
              parts <- paste0(parts, " + ", round(coef_val, 4),
                              " \\cdot \\text{", pn_tex, "}")
            } else {
              parts <- paste0(parts, " - ", round(abs(coef_val), 4),
                              " \\cdot \\text{", pn_tex, "}")
            }
          }
        }
      }

      latex <- paste0("$$", parts, "$$")

      family_info <- paste0("Family: ", fam$family,
                            "(link = \"", link_name, "\")")
      method_info <- paste0("Method: ", model$method)

      # Build definitions section
      def_items <- lapply(smooth_defs, function(d) {
        tags$li(
          style = "margin-bottom: 4px; font-size: 0.9em;",
          withMathJax(HTML(paste0("\\(", d$fn, "\\)"))),
          HTML(paste0(" &rarr; <code>", htmltools::htmlEscape(d$label),
                      "</code> &mdash; ", htmltools::htmlEscape(d$desc)))
        )
      })

      tagList(
        tags$p(
          tags$span(family_info, style = "margin-right: 20px;"),
          tags$span(method_info),
          style = "color: var(--bs-secondary-color); margin-bottom: 10px;"
        ),
        withMathJax(HTML(latex)),
        if (length(def_items) > 0L) {
          tagList(
            tags$h6("Smooth function definitions:",
                    style = "margin-top: 16px; margin-bottom: 6px;"),
            tags$ul(style = "list-style: none; padding-left: 8px;", def_items)
          )
        }
      )
    })

    # --- Equation tab: smooth function definitions table ---
    output$smooth_specs_table <- DT::renderDT({
      res <- result()
      req(res)
      model <- res$model
      specs <- res$smooth_specs

      rows <- lapply(specs, function(sp) {
        var_name <- paste(sp$vars, collapse = ", ")
        term_type <- sp$type  # "s", "te", "ti", "linear"

        # Basis type
        bs <- if (!is.null(sp$bs)) sp$bs else "tp"

        # k value
        k_val <- if (!is.null(sp$k)) as.character(sp$k) else "default"

        # Knot positions from earth (if any)
        knots <- ""
        if (!is.null(res$earth_knots) &&
            sp$vars[1] %in% names(res$earth_knots$knots)) {
          kv <- res$earth_knots$knots[[sp$vars[1]]]
          knots <- paste(round(kv, 4), collapse = ", ")
        }

        # Build the R formula term
        if (term_type == "linear") {
          formula_term <- var_name
        } else {
          args <- var_name
          if (bs != "tp") args <- paste0(args, ", bs=\"", bs, "\"")
          if (k_val != "default") args <- paste0(args, ", k=", k_val)
          formula_term <- paste0(term_type, "(", args, ")")
        }

        data.frame(
          Term = formula_term,
          Variable = var_name,
          Type = term_type,
          Basis = if (term_type == "linear") "-" else bs,
          k = k_val,
          Knots = if (nzchar(knots)) knots else "-",
          stringsAsFactors = FALSE
        )
      })

      spec_df <- do.call(rbind, rows)

      DT::datatable(spec_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE,
                                  pageLength = 50),
                    class = "compact stripe")
    })

    # --- Summary tab: metric cards ---
    output$summary_metrics <- renderUI({
      res <- result()
      req(res)
      summ <- format_gam_summary(res)

      cv_text <- if (!is.null(summ$cv_rsq)) {
        sprintf("%.4f", summ$cv_rsq)
      } else {
        "N/A"
      }

      fluidRow(
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("R\u00b2"), tags$h4(sprintf("%.4f", summ$r_squared))
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("CV R\u00b2"), tags$h4(cv_text)
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("Dev. Expl."),
          tags$h4(paste0(round(summ$dev_explained * 100, 1), "%"))
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("AIC"), tags$h4(round(summ$aic, 1))
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("n"), tags$h4(summ$n_obs)
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("Smooths"), tags$h4(summ$n_smooths)
        ))
      )
    })

    output$smooth_table <- DT::renderDT({
      req(result())
      summ <- format_gam_summary(result())
      if (nrow(summ$smooth_table) == 0L) return(NULL)
      st <- summ$smooth_table
      st$p_value <- format.pval(st$p_value, digits = 3)
      DT::datatable(st, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })

    output$param_table <- DT::renderDT({
      req(result())
      summ <- format_gam_summary(result())
      if (nrow(summ$parametric_table) == 0L) return(NULL)
      pt <- summ$parametric_table
      pt$p_value <- format.pval(pt$p_value, digits = 3)
      DT::datatable(pt, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })

    smooth_vars_r <- reactive({
      res <- result()
      req(res)
      sv <- vapply(res$smooth_specs, function(s) {
        if (s$type != "linear") s$vars[1] else NA_character_
      }, character(1))
      sv[!is.na(sv)]
    })

    output$smooth_plots_container <- renderUI({
      sv <- smooth_vars_r()
      if (length(sv) == 0L) return(tags$p("No smooth terms."))
      plot_outputs <- lapply(sv, function(var) {
        id <- ns(paste0("plotly_", var))
        plotly::plotlyOutput(id, height = "400px")
      })
      do.call(tagList, plot_outputs)
    })

    observe({
      res <- result()
      req(res)
      sv <- smooth_vars_r()

      lapply(sv, function(var) {
        local({
          v <- var
          output[[paste0("plotly_", v)]] <- plotly::renderPlotly({
            tryCatch(
              plot_smooth_interactive(result(), v,
                                      earth_knots = earth_knots_r()),
              error = function(e) {
                plotly::plot_ly() |>
                  plotly::add_annotations(
                    text = paste("Error:", e$message),
                    x = 0.5, y = 0.5, showarrow = FALSE
                  )
              }
            )
          })
        })
      })
    })

    output$diagnostics_plot <- renderPlot({
      req(result())
      plot_diagnostics(result())
    })

    output$avp_plot <- renderPlot({
      req(result())
      plot_actual_vs_predicted(result())
    })

    output$sign_table <- DT::renderDT({
      req(result())
      signs <- check_sign_consistency(result())
      if (is.null(signs)) {
        return(DT::datatable(
          data.frame(Note = "No earth model imported."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(signs, rownames = FALSE,
                    options = list(dom = "t"))
    })

    output$sign_note <- renderText({
      req(result())
      if (is.null(earth_knots_r())) {
        return("Import an earthUI result to enable sign checks.")
      }
      signs <- check_sign_consistency(result())
      if (is.null(signs)) return("")
      n_warn <- sum(!signs$consistent)
      if (n_warn == 0L) {
        "All directions consistent."
      } else {
        paste(n_warn, "variable(s) show direction inconsistency.")
      }
    })

    output$gam_check <- renderPrint({
      req(result())
      mgcv::gam.check(result()$model)
    })

    # --- Variable Importance tab ---
    output$importance_plot <- renderPlot({
      res <- result()
      req(res)
      summ <- format_gam_summary(res)

      imp_df <- data.frame(
        Term = character(0), Statistic = numeric(0),
        Type = character(0), stringsAsFactors = FALSE
      )

      if (nrow(summ$smooth_table) > 0) {
        imp_df <- rbind(imp_df, data.frame(
          Term = summ$smooth_table$Term,
          Statistic = summ$smooth_table$F,
          Type = "Smooth",
          stringsAsFactors = FALSE
        ))
      }

      if (nrow(summ$parametric_table) > 0) {
        pt <- summ$parametric_table[
          summ$parametric_table$Term != "(Intercept)", , drop = FALSE
        ]
        if (nrow(pt) > 0) {
          imp_df <- rbind(imp_df, data.frame(
            Term = pt$Term,
            Statistic = abs(pt$t_value),
            Type = "Parametric",
            stringsAsFactors = FALSE
          ))
        }
      }

      if (nrow(imp_df) == 0) return(NULL)

      imp_df$Term <- factor(imp_df$Term,
                            levels = imp_df$Term[order(imp_df$Statistic)])

      ggplot2::ggplot(imp_df,
                      ggplot2::aes(x = .data$Statistic, y = .data$Term,
                                   fill = .data$Type)) +
        ggplot2::geom_col(alpha = 0.85) +
        ggplot2::scale_fill_manual(
          values = c("Smooth" = "#5e81ac", "Parametric" = "#a3be8c")
        ) +
        ggplot2::labs(title = "Variable Importance (F / |t| statistic)",
                      x = "Statistic", y = NULL) +
        ggplot2::theme_minimal(base_size = 13)
    })

    output$importance_table <- DT::renderDT({
      res <- result()
      req(res)
      summ <- format_gam_summary(res)

      imp_rows <- list()

      if (nrow(summ$smooth_table) > 0) {
        for (i in seq_len(nrow(summ$smooth_table))) {
          imp_rows[[length(imp_rows) + 1L]] <- data.frame(
            Term = summ$smooth_table$Term[i],
            Type = "Smooth",
            EDF = round(summ$smooth_table$EDF[i], 2),
            Statistic = round(summ$smooth_table$F[i], 4),
            p_value = summ$smooth_table$p_value[i],
            stringsAsFactors = FALSE
          )
        }
      }

      if (nrow(summ$parametric_table) > 0) {
        pt <- summ$parametric_table[
          summ$parametric_table$Term != "(Intercept)", , drop = FALSE
        ]
        if (nrow(pt) > 0) {
          for (i in seq_len(nrow(pt))) {
            imp_rows[[length(imp_rows) + 1L]] <- data.frame(
              Term = pt$Term[i],
              Type = "Parametric",
              EDF = 1.0,
              Statistic = round(abs(pt$t_value[i]), 4),
              p_value = pt$p_value[i],
              stringsAsFactors = FALSE
            )
          }
        }
      }

      if (length(imp_rows) == 0) return(NULL)

      imp_df <- do.call(rbind, imp_rows)
      imp_df <- imp_df[order(imp_df$Statistic, decreasing = TRUE), ]
      imp_df$p_value <- format.pval(imp_df$p_value, digits = 3)

      DT::datatable(imp_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE,
                                  pageLength = 50),
                    class = "compact stripe")
    })

    # --- Correlation tab ---
    output$correlation_plot_ui <- renderUI({
      req(result())
      plotOutput(ns("correlation_plot"), height = "700px", width = "700px")
    })

    output$correlation_plot <- renderPlot({
      res <- result()
      req(res)
      model <- res$model

      # Get numeric predictors from model frame
      mf <- model$model
      pred_data <- mf[, setdiff(names(mf), res$response), drop = FALSE]

      num_cols <- vapply(pred_data, is.numeric, logical(1))
      pred_data <- pred_data[, num_cols, drop = FALSE]

      if (ncol(pred_data) < 2L) {
        plot.new()
        graphics::text(0.5, 0.5,
                       "Need at least 2 numeric predictors for correlation.",
                       cex = 1.2)
        return()
      }

      cor_mat <- stats::cor(pred_data, use = "pairwise.complete.obs")

      vars <- colnames(cor_mat)
      cor_long <- expand.grid(Var1 = vars, Var2 = vars,
                              stringsAsFactors = FALSE)
      cor_long$value <- as.numeric(cor_mat)
      cor_long$Var1 <- factor(cor_long$Var1, levels = rev(vars))
      cor_long$Var2 <- factor(cor_long$Var2, levels = vars)

      n_vars <- length(vars)
      txt_size <- max(2.5, 5 - n_vars * 0.15)

      ggplot2::ggplot(cor_long,
                      ggplot2::aes(x = .data$Var2, y = .data$Var1,
                                   fill = .data$value)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", .data$value)),
          size = txt_size
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#2166AC", mid = "white", high = "#B2182B",
          midpoint = 0, limits = c(-1, 1), name = "Correlation"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = "Correlation Matrix", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )
    }, res = 120)

    # --- Anova tab ---
    output$anova_table <- DT::renderDT({
      res <- result()
      req(res)

      aov <- summary(res$model)
      parts <- list()

      # Parametric terms
      if (!is.null(aov$p.table) && nrow(aov$p.table) > 0) {
        pt <- as.data.frame(aov$p.table)
        pt$Term <- rownames(pt)
        pt$Type <- "parametric"
        parts <- c(parts, list(pt))
      }

      # Smooth terms
      if (!is.null(aov$s.table) && nrow(aov$s.table) > 0) {
        st <- as.data.frame(aov$s.table)
        st$Term <- rownames(st)
        st$Type <- "smooth"
        parts <- c(parts, list(st))
      }

      if (length(parts) == 0) return(NULL)

      # Merge parametric & smooth (different columns — fill NA)
      all_cols <- unique(unlist(lapply(parts, names)))
      aov_df <- do.call(rbind, lapply(parts, function(p) {
        missing <- setdiff(all_cols, names(p))
        for (m in missing) p[[m]] <- NA
        p[, all_cols, drop = FALSE]
      }))

      # Move Term and Type to front
      front <- c("Term", "Type")
      aov_df <- aov_df[, c(front, setdiff(names(aov_df), front)),
                        drop = FALSE]
      rownames(aov_df) <- NULL

      # Format p-values if present
      pcol <- grep("p-value|Pr\\(", names(aov_df), value = TRUE)
      for (pc in pcol) {
        aov_df[[pc]] <- format.pval(aov_df[[pc]], digits = 3)
      }

      DT::datatable(aov_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE,
                                  pageLength = 50),
                    class = "compact stripe")
    })

    # --- Mgcv Output tab ---
    output$mgcv_output <- renderPrint({
      res <- result()
      req(res)

      cat(sprintf("== Timing: %.2f seconds ==\n\n", res$elapsed))

      cat("== Formula ==\n\n")
      print(res$formula)

      cat("\n\n== Model ==\n\n")
      print(res$model)

      cat("\n\n== Summary ==\n\n")
      print(summary(res$model))

      cat("\n\n== Family ==\n\n")
      print(res$model$family)
    })

    output$data_preview <- DT::renderDT({
      req(data_r())
      DT::datatable(
        utils::head(data_r(), 100L),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10L)
      )
    })

    reactive(result())
  })
}
