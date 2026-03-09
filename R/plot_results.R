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
    if (variable %in% model$smooth[[i]]$vn) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No smooth term found for variable: ", variable, call. = FALSE)
  }

  sm_sel <- paste0("s(", variable, ")")
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
    gam_result, variable, earth_knots = NULL) {
  model <- gam_result$model
  resp <- gam_result$response

  # Check smooth exists
  has_smooth <- FALSE
  for (i in seq_along(model$smooth)) {
    if (variable %in% model$smooth[[i]]$vn) {
      has_smooth <- TRUE
      break
    }
  }
  if (!has_smooth) {
    stop("No smooth term found for variable: ", variable, call. = FALSE)
  }

  sm_sel <- paste0("s(", variable, ")")
  sm_data <- gratia::smooth_estimates(model, select = sm_sel)

  x <- sm_data[[variable]]
  y <- sm_data$.estimate
  se <- sm_data$.se

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

  # Confidence band
  ymin <- y - 1.96 * se
  ymax <- y + 1.96 * se

  # Build hover text
  hover_text <- paste0(
    variable, ": ", round(x, 4), "\n",
    "Contribution: $", comma_fmt(y), "\n",
    "95% CI: [$", comma_fmt(ymin), " , $", comma_fmt(ymax), "]\n",
    "Rate: $", comma_fmt(slope_display), " ", unit_label
  )

  fig <- plotly::plot_ly() |>
    plotly::add_ribbons(
      x = x, ymin = ymin, ymax = ymax,
      fillcolor = "rgba(70,130,180,0.2)",
      line = list(color = "transparent"),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    plotly::add_lines(
      x = x, y = y,
      line = list(color = "steelblue", width = 2.5),
      text = hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    )

  # Add partial residuals
  pres <- gratia::partial_residuals(model)
  smooth_label <- paste0("s(", variable, ")")
  if (smooth_label %in% names(pres)) {
    fig <- fig |>
      plotly::add_markers(
        x = model$model[[variable]],
        y = pres[[smooth_label]],
        marker = list(color = "grey", opacity = 0.3, size = 4),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }

  # Add earth knot lines
  if (!is.null(earth_knots) && variable %in% names(earth_knots$knots)) {
    for (kv in earth_knots$knots[[variable]]) {
      fig <- fig |>
        plotly::add_segments(
          x = kv, xend = kv,
          y = min(ymin), yend = max(ymax),
          line = list(color = "red", dash = "dash", width = 1),
          opacity = 0.6, showlegend = FALSE,
          hoverinfo = "skip"
        )
    }
  }

  fig |>
    plotly::layout(
      title = list(text = variable, font = list(size = 16)),
      xaxis = list(title = variable),
      yaxis = list(
        title = paste0("Contribution to ", resp),
        tickformat = ",.0f"
      ),
      hovermode = "x unified",
      margin = list(t = 50, b = 50)
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
  df <- data.frame(
    actual    = model$model[[gam_result$response]],
    predicted = fitted(model)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$predicted,
                                   y = .data$actual)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "grey40") +
    ggplot2::labs(x = "Predicted", y = "Actual",
                  title = "Actual vs Predicted") +
    ggplot2::theme_minimal(base_size = 12)
}
