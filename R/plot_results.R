#' Plot GAM Smooth Terms
#'
#' Produces ggplot2-based smooth plots for each term in the fitted GAM,
#' using the \pkg{gratia} package for tidy extraction.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param residuals Logical. If `TRUE`, overlay partial residuals on
#'   smooth plots. Default `TRUE`.
#' @return A ggplot object showing all smooth terms in a faceted layout.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_smooths(res)
plot_smooths <- function(gam_result, residuals = TRUE) {
  gratia::draw(gam_result$model, residuals = residuals) +
    ggplot2::theme_minimal(base_size = 12)
}


#' Plot a Single GAM Smooth Term
#'
#' Plots one smooth term from the fitted GAM with confidence band and
#' optional partial residuals.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param variable Character name of the variable to plot.
#' @param residuals Logical. Overlay partial residuals? Default `TRUE`.
#' @param earth_knots Optional `mgcvUI_earth_knots` object. If provided,
#'   earth knot positions are marked on the plot as vertical dashed
#'   lines.
#' @return A ggplot object.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_smooth_single(res, "wt")
plot_smooth_single <- function(gam_result, variable, residuals = TRUE,
                               earth_knots = NULL) {
  model <- gam_result$model

  # Find the smooth index for this variable
  smooth_idx <- NULL
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    # Check $term (variable names) first, fall back to $vn for older mgcv
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (variable %in% sm_vars && sm$dim == 1L && is.null(sm$by.level)) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No smooth term found for variable: ", variable, call. = FALSE)
  }

  sm_sel <- model$smooth[[smooth_idx]]$label
  sm_data <- gratia::smooth_estimates(model, select = sm_sel)

  # Comma-formatted number labels
  comma_fmt <- function(x) {
    formatC(round(x), format = "f", big.mark = ",", digits = 0)
  }

  p <- ggplot2::ggplot(
    sm_data,
    ggplot2::aes(x = .data[[variable]], y = .data$.estimate)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$.estimate - 1.96 * .data$.se,
                   ymax = .data$.estimate + 1.96 * .data$.se),
      fill = "steelblue", alpha = 0.2
    ) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1.2) +
    ggplot2::scale_y_continuous(labels = comma_fmt) +
    ggplot2::labs(y = paste0("Contribution to ", gam_result$response),
                  x = variable,
                  title = variable) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      axis.title = ggplot2::element_text(size = 13),
      axis.text = ggplot2::element_text(size = 11)
    )

  # Add partial residuals
  if (residuals) {
    pres <- gratia::partial_residuals(model)
    smooth_label <- paste0("s(", variable, ")")
    if (smooth_label %in% names(pres)) {
      res_df <- data.frame(
        x = model$model[[variable]],
        y = pres[[smooth_label]]
      )
      p <- p + ggplot2::geom_point(
        data = res_df,
        ggplot2::aes(x = .data$x, y = .data$y),
        alpha = 0.3, size = 1.5, color = "grey40"
      )
    }
  }

  # Add earth knot positions
  if (!is.null(earth_knots) && variable %in% names(earth_knots$knots)) {
    knot_df <- data.frame(xintercept = earth_knots$knots[[variable]])
    p <- p + ggplot2::geom_vline(
      data = knot_df,
      ggplot2::aes(xintercept = .data$xintercept),
      linetype = "dashed", color = "red", alpha = 0.6
    )
  }

  p
}


#' Interactive Smooth Plot with Slope Tooltip
#'
#' Creates a plotly smooth plot showing contribution to value with
#' hover tooltips displaying the slope (rate of change per unit).
#' For latitude/longitude variables, slope is computed per 0.001 degrees.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param variable Character name of the variable to plot.
#' @param earth_knots Optional `mgcvUI_earth_knots` object.
#' @return A plotly htmlwidget.
#' @export
plot_smooth_interactive <- function(
    gam_result, variable, earth_knots = NULL, dark_mode = FALSE) {
  model <- gam_result$model
  resp <- gam_result$response
  xform <- gam_result$response_transform %||% "none"

  # Check smooth exists
  smooth_idx <- NULL
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (variable %in% sm_vars && sm$dim == 1L && is.null(sm$by.level)) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No smooth term found for variable: ", variable, call. = FALSE)
  }

  sm_sel <- model$smooth[[smooth_idx]]$label
  sm_data <- gratia::smooth_estimates(model, select = sm_sel)

  x <- sm_data[[variable]]
  y <- sm_data$.estimate
  se_val <- sm_data$.se

  # Diagnostic logging for unexpected X ranges
  message("  plot_smooth_interactive '", variable, "': x range = [",
          round(min(x), 2), ", ", round(max(x), 2), "], y range = [",
          round(min(y), 4), ", ", round(max(y), 4), "]")

  # Compute slope (dy/dx)
  dx <- diff(x)
  dy <- diff(y)
  raw_slope <- dy / dx

  # Smooth the slope slightly for display
  slope <- c(raw_slope[1], (raw_slope[-length(raw_slope)] + raw_slope[-1]) / 2,
             raw_slope[length(raw_slope)])

  # Determine slope unit
  is_coord <- grepl("lat|lon", variable, ignore.case = TRUE)
  if (is_coord) {
    slope_display <- slope * 0.001
    unit_label <- "/ 0.001\u00b0"
  } else {
    slope_display <- slope
    unit_label <- paste0("/ ", variable, " unit")
  }

  comma_fmt <- function(x) {
    formatC(round(x), format = "f", big.mark = ",", digits = 0)
  }

  # Back-transform contributions to dollar values when log transform is used.
  # The smooth contribution c is on log scale; the dollar effect relative to
  # the mean predicted price is: mean_price * (10^c - 1) for log10.
  if (xform == "log10") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- 10^mean_resp
    y_display <- mean_price * (10^y - 1)
    ymin_display <- mean_price * (10^(y - 1.96 * se_val) - 1)
    ymax_display <- mean_price * (10^(y + 1.96 * se_val) - 1)
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
    y_ticksuffix <- ""
  } else if (xform == "log") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- exp(mean_resp)
    y_display <- mean_price * (exp(y) - 1)
    ymin_display <- mean_price * (exp(y - 1.96 * se_val) - 1)
    ymax_display <- mean_price * (exp(y + 1.96 * se_val) - 1)
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
    y_ticksuffix <- ""
  } else {
    y_display <- y
    ymin_display <- y - 1.96 * se_val
    ymax_display <- y + 1.96 * se_val
    y_title <- paste0("Contribution to ", resp)
    y_tickfmt <- ",.0f"
    y_ticksuffix <- ""
  }

  # Build hover text
  hover_text <- paste0(
    variable, ": ", round(x, 4), "\n",
    "Contribution: $", comma_fmt(y_display), "\n",
    "95% CI: [$", comma_fmt(ymin_display), " , $",
    comma_fmt(ymax_display), "]\n",
    "Rate: $", comma_fmt(slope_display), " ", unit_label
  )

  fig <- plotly::plot_ly() |>
    plotly::add_ribbons(
      x = x, ymin = ymin_display, ymax = ymax_display,
      fillcolor = "rgba(70,130,180,0.2)",
      line = list(color = "transparent"),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    plotly::add_lines(
      x = x, y = y_display,
      line = list(color = "steelblue", width = 2.5),
      text = hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    )

  # Add partial residuals (only when no transform — back-transformed
  # residuals produce extreme outliers that dominate the axis)
  if (xform == "none") {
    pres <- gratia::partial_residuals(model)
    smooth_label <- paste0("s(", variable, ")")
    if (smooth_label %in% names(pres)) {
      scatter_x <- model$model[[variable]]
      scatter_y <- pres[[smooth_label]]
      message("  DEBUG scatter '", variable, "': x class=",
              paste(class(scatter_x), collapse="/"),
              " range=[", min(scatter_x, na.rm=TRUE), ",",
              max(scatter_x, na.rm=TRUE), "]",
              " y range=[", round(min(scatter_y, na.rm=TRUE)),
              ",", round(max(scatter_y, na.rm=TRUE)), "]")
      fig <- fig |>
        plotly::add_markers(
          x = scatter_x,
          y = scatter_y,
          marker = list(color = "grey", opacity = 0.3, size = 4),
          showlegend = FALSE,
          hoverinfo = "text",
          text = paste0(variable, "=", round(scatter_x, 1),
                        " resid=", round(scatter_y, 0))
        )
    }
  }

  # Add earth knot lines
  if (!is.null(earth_knots) && variable %in% names(earth_knots$knots)) {
    for (kv in earth_knots$knots[[variable]]) {
      fig <- fig |>
        plotly::add_segments(
          x = kv, xend = kv,
          y = min(ymin_display), yend = max(ymax_display),
          line = list(color = "red", dash = "dash", width = 1),
          opacity = 0.6, showlegend = FALSE,
          hoverinfo = "skip"
        )
    }
  }

  font_color <- if (dark_mode) "#d8dee9" else "#2e3440"
  grid_color <- if (dark_mode) "rgba(67,76,94,0.6)" else "rgba(0,0,0,0.1)"

  fig |>
    plotly::layout(
      title = list(text = variable,
                   font = list(size = 16, color = font_color)),
      xaxis = list(title = variable,
                   color = font_color, gridcolor = grid_color),
      yaxis = list(
        title = y_title,
        tickformat = y_tickfmt,
        ticksuffix = y_ticksuffix,
        color = font_color, gridcolor = grid_color
      ),
      font = list(color = font_color),
      hovermode = "x unified",
      margin = list(t = 50, b = 50),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}


#' Plot GAM Residual Diagnostics
#'
#' Produces a 2x2 panel of diagnostic plots: residuals vs fitted,
#' Q-Q plot, histogram of residuals, and response vs fitted.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A ggplot object (patchwork-assembled if available, otherwise
#'   individual plots returned as a list).
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_diagnostics(res)
plot_diagnostics <- function(gam_result) {
  gratia::appraise(gam_result$model) +
    ggplot2::theme_minimal(base_size = 11)
}


#' Plot Actual vs Predicted
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A ggplot object.
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_actual_vs_predicted(res)
plot_actual_vs_predicted <- function(gam_result) {
  model <- gam_result$model
  xform <- gam_result$response_transform %||% "none"

  actual_raw <- model$model[[gam_result$response]]
  predicted_raw <- fitted(model)

  # Back-transform to original scale for display
  df <- data.frame(
    actual    = back_transform_(actual_raw, xform),
    predicted = back_transform_(predicted_raw, xform)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$predicted,
                                   y = .data$actual)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "grey40") +
    ggplot2::labs(x = "Predicted", y = "Actual",
                  title = "Actual vs Predicted (original scale)") +
    ggplot2::theme_minimal(base_size = 12)
}
