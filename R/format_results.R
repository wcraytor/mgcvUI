#' Format GAM Summary Statistics
#'
#' Extracts key statistics from a fitted GAM for display in the UI.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A list with components: `r_squared`, `dev_explained`,
#'   `aic`, `bic`, `n_obs`, `n_smooths`, `method`, `family`,
#'   `smooth_table`, `parametric_table`.
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' format_gam_summary(res)
format_gam_summary <- function(gam_result) {
  model <- gam_result$model
  sm <- summary(model)

  # Smooth terms table
  if (!is.null(sm$s.table) && nrow(sm$s.table) > 0L) {
    smooth_tbl <- as.data.frame(sm$s.table, check.names = FALSE)
    smooth_tbl$Term <- rownames(sm$s.table)
    # Handle both Gaussian ("F") and non-Gaussian ("Chi.sq") families
    stat_col <- if ("F" %in% names(smooth_tbl)) "F" else "Chi.sq"
    smooth_tbl <- data.frame(
      Term   = smooth_tbl$Term,
      EDF    = smooth_tbl[["edf"]],
      Ref.df = smooth_tbl[["Ref.df"]],
      F      = smooth_tbl[[stat_col]],
      p_value = smooth_tbl[["p-value"]],
      stringsAsFactors = FALSE
    )
  } else {
    smooth_tbl <- data.frame(
      Term = character(0), EDF = numeric(0), Ref.df = numeric(0),
      F = numeric(0), p_value = numeric(0)
    )
  }

  # Parametric terms table
  if (!is.null(sm$p.table) && nrow(sm$p.table) > 0L) {
    param_tbl <- as.data.frame(sm$p.table, check.names = FALSE)
    param_tbl$Term <- rownames(sm$p.table)
    # Handle both Gaussian ("t value") and non-Gaussian ("z value")
    stat_col <- if ("t value" %in% names(param_tbl)) "t value" else "z value"
    p_col <- if ("Pr(>|t|)" %in% names(param_tbl)) "Pr(>|t|)" else "Pr(>|z|)"
    param_tbl <- data.frame(
      Term      = param_tbl$Term,
      Estimate  = param_tbl[["Estimate"]],
      Std_Error = param_tbl[["Std. Error"]],
      t_value   = param_tbl[[stat_col]],
      p_value   = param_tbl[[p_col]],
      stringsAsFactors = FALSE
    )
  } else {
    param_tbl <- data.frame(
      Term = character(0), Estimate = numeric(0), Std_Error = numeric(0),
      t_value = numeric(0), p_value = numeric(0)
    )
  }

  list(
    r_squared     = sm$r.sq,
    cv_rsq        = gam_result$cv_rsq,
    dev_explained = sm$dev.expl,
    aic           = if (is.finite(AIC(model))) AIC(model) else model$aic,
    bic           = if (is.finite(BIC(model))) BIC(model) else NA_real_,
    n_obs         = nrow(model$model),
    n_smooths     = length(model$smooth),
    method        = model$method,
    family        = model$family$family,
    smooth_table  = smooth_tbl,
    parametric_table = param_tbl
  )
}


#' Tidy GAM Results with broom
#'
#' Returns a tidy data frame of model terms using [broom::tidy()].
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A data frame (tibble) of tidied model terms.
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' tidy_gam(res)
tidy_gam <- function(gam_result) {
  broom::tidy(gam_result$model)
}


#' Glance at GAM Model Fit
#'
#' Returns a one-row data frame of model-level statistics using
#' [broom::glance()].
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A one-row data frame (tibble).
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' glance_gam(res)
glance_gam <- function(gam_result) {
  broom::glance(gam_result$model)
}
