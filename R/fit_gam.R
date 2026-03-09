#' Fit a GAM Model
#'
#' Wrapper around [mgcv::gam()] that accepts the specification objects
#' produced by the mgcvUI interface and returns a structured result.
#'
#' @param data Data frame of observations.
#' @param response Character name of the response variable.
#' @param smooth_specs List of smooth-term specifications (see
#'   [build_gam_formula()]).
#' @param family A family object or character string (e.g. `"gaussian"`,
#'   `"Gamma"`). Default is `gaussian()`.
#' @param method Character fitting method. Default `"REML"`.
#' @param earth_knots Optional `mgcvUI_earth_knots` object from
#'   [import_earth()]. When provided, knot positions are passed to
#'   [mgcv::gam()].
#' @param select Logical. If `TRUE`, adds an extra penalty to each
#'   smooth term so that it can be penalised to zero (variable
#'   selection). Default `FALSE`.
#' @param gamma Numeric inflation factor for the effective degrees of
#'   freedom in the GCV/UBRE score. Values greater than 1 produce
#'   smoother fits. Default `1`.
#' @param cv_folds Integer number of cross-validation folds. Set to 0
#'   to skip CV. Default `10`.
#' @param weights Optional numeric vector of prior weights (one per
#'   observation). Passed to [mgcv::gam()].
#' @return An `mgcvUI_result` object (a list) with components:
#'   \describe{
#'     \item{model}{The fitted [mgcv::gam] object.}
#'     \item{formula}{The formula used.}
#'     \item{response}{Response variable name.}
#'     \item{smooth_specs}{The smooth-term specs used.}
#'     \item{earth_knots}{The earth knots used (or `NULL`).}
#'     \item{elapsed}{Fitting time in seconds.}
#'     \item{cv_rsq}{Cross-validated R-squared (or `NULL` if CV skipped).}
#'   }
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' result <- fit_gam(mtcars, "mpg", specs)
#' summary(result$model)
fit_gam <- function(data, response, smooth_specs, family = gaussian(),
                    method = "REML", earth_knots = NULL,
                    select = FALSE, gamma = 1, cv_folds = 10L,
                    weights = NULL) {
  # Defensive defaults for NULL parameters (Shiny inputs may be NULL)
  if (is.null(method)) method <- "REML"
  if (is.null(select)) select <- FALSE
  if (is.null(gamma) || is.na(gamma)) gamma <- 1
  if (is.null(family)) family <- gaussian()

  # Ensure plain data.frame with types mgcv can handle
  data <- as.data.frame(data)
  for (col in names(data)) {
    if (inherits(data[[col]], "POSIXct") || inherits(data[[col]], "POSIXlt")) {
      data[[col]] <- as.numeric(data[[col]])
    } else if (inherits(data[[col]], "Date")) {
      data[[col]] <- as.numeric(data[[col]])
    } else if (is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  # Validate smooth_specs structure
  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (is.null(spec$vars) || length(spec$vars) == 0L) {
      stop("smooth_specs[[", i, "]] has no 'vars'.", call. = FALSE)
    }
    if (!all(spec$vars %in% names(data))) {
      missing <- setdiff(spec$vars, names(data))
      stop("Variable(s) not in data: ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    if (!response %in% names(data)) {
      stop("Response '", response, "' not found in data.", call. = FALSE)
    }
  }

  # Handle low-cardinality variables
  drop_idx <- integer(0)
  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next
    n_unique <- length(unique(stats::na.omit(data[[spec$vars]])))
    if (n_unique <= 1L) {
      # Constant -- drop from model
      message("  dropping '", spec$vars, "': only ", n_unique,
              " unique value(s)")
      drop_idx <- c(drop_idx, i)
    } else if (n_unique == 2L) {
      # Binary -- treat as factor
      message("  '", spec$vars, "': 2 unique values, converting to factor")
      data[[spec$vars]] <- as.factor(data[[spec$vars]])
      smooth_specs[[i]]$type <- "linear"
      smooth_specs[[i]]$bs <- NULL
      smooth_specs[[i]]$k <- NULL
    } else {
      k <- spec$k
      if (is.null(k)) k <- 10L  # mgcv default
      if (k >= n_unique) {
        smooth_specs[[i]]$k <- max(n_unique - 1L, 3L)
      }
    }
  }
  if (length(drop_idx) > 0L) {
    smooth_specs <- smooth_specs[-drop_idx]
  }
  if (length(smooth_specs) == 0L) {
    stop("No usable predictors remain after filtering.", call. = FALSE)
  }

  # Reconcile k with earth knot counts for cr/ps bases
  smooth_specs <- reconcile_knots_(smooth_specs, earth_knots)

  # Final safety: cap k to unique covariate combinations
  # Must use complete cases across ALL model variables (mgcv uses na.action)
  all_model_vars <- unique(c(response,
    unlist(lapply(smooth_specs, `[[`, "vars"))))
  complete_rows <- stats::complete.cases(data[, all_model_vars, drop = FALSE])
  complete_data <- data[complete_rows, , drop = FALSE]
  n_complete <- nrow(complete_data)
  message("  complete cases: ", n_complete, " of ", nrow(data))

  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (spec$type == "linear") next
    k_eff <- spec$k %||% 10L  # mgcv default is 10
    if (length(spec$vars) == 1L) {
      n_unique <- length(unique(complete_data[[spec$vars]]))
    } else {
      # Multi-variable: count unique combinations
      sub <- complete_data[, spec$vars, drop = FALSE]
      n_unique <- nrow(unique(sub))
    }
    if (k_eff >= n_unique) {
      new_k <- max(n_unique - 1L, 3L)
      message("  capping k for ", paste(spec$vars, collapse = ","),
              ": ", k_eff, " -> ", new_k, " (unique=", n_unique, ")")
      smooth_specs[[i]]$k <- new_k
    }
  }

  formula <- build_gam_formula(response, smooth_specs)
  knots  <- build_gam_knots(smooth_specs, earth_knots)

  if (is.character(family)) {
    family <- get(family, mode = "function")()
  }

  # Coerce predictor columns: factors/characters need to be factors for gam()
  for (spec in smooth_specs) {
    if (spec$type == "linear") {
      for (v in spec$vars) {
        if (is.character(data[[v]])) {
          data[[v]] <- as.factor(data[[v]])
        }
      }
    }
  }

  # Log column classes for debugging
  all_vars <- unique(c(response, unlist(lapply(smooth_specs, `[[`, "vars"))))
  for (v in all_vars) {
    message("  col '", v, "': class=", paste(class(data[[v]]), collapse = "/"),
            " length=", length(data[[v]]),
            " NAs=", sum(is.na(data[[v]])))
  }
  message("  formula: ", deparse(formula))

  # Debug: log k vs unique for each smooth term (using complete cases)
  for (spec in smooth_specs) {
    if (spec$type == "linear") next
    k_val <- spec$k %||% "default(10)"
    if (length(spec$vars) == 1L) {
      nu <- length(unique(complete_data[[spec$vars]]))
    } else {
      sub <- complete_data[, spec$vars, drop = FALSE]
      nu <- nrow(unique(sub))
    }
    message("  term: ", spec$type, "(", paste(spec$vars, collapse = ","),
            ") bs=", spec$bs %||% "NULL", " k=", k_val,
            " unique(complete)=", nu)
  }

  # Subset data to only model-relevant columns to prevent na.action

  # from dropping rows due to NAs in unrelated columns
  model_data <- data[, all_vars, drop = FALSE]

  t0 <- proc.time()
  gam_args <- list(
    formula = formula,
    data    = model_data,
    family  = family,
    method  = method,
    knots   = knots,
    select  = select,
    gamma   = gamma
  )
  if (!is.null(weights)) {
    gam_args$weights <- weights
  }
  model <- do.call(mgcv::gam, gam_args)
  elapsed <- (proc.time() - t0)[["elapsed"]]

  # Cross-validated R-squared
  cv_folds <- if (is.null(cv_folds)) 0L else as.integer(cv_folds)
  cv_rsq <- NULL
  if (cv_folds >= 2L) {
    cv_rsq <- tryCatch(
      cv_rsq_(data, formula, family, method, knots, select, gamma,
              cv_folds, response, weights),
      error = function(e) {
        message("  CV R-sq failed: ", e$message)
        NULL
      }
    )
  }

  structure(
    list(
      model        = model,
      formula      = formula,
      response     = response,
      smooth_specs = smooth_specs,
      earth_knots  = earth_knots,
      elapsed      = elapsed,
      cv_rsq       = cv_rsq
    ),
    class = "mgcvUI_result"
  )
}


#' Check for Sign Discrepancies Between Earth and GAM
#'
#' Compares the direction implied by earth hinge functions with the
#' estimated GAM smooth derivatives.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A data frame with columns: `variable`, `earth_direction`,
#'   `gam_direction`, `consistent`, `warning`. Returns `NULL` if no
#'   earth knots are available.
#' @export
#' @examples
#' if (requireNamespace("earth", quietly = TRUE)) {
#'   m <- earth::earth(mpg ~ wt + hp, data = mtcars)
#'   er <- structure(
#'     list(model = m, target = "mpg",
#'          predictors = c("wt", "hp"),
#'          categoricals = character(0), linpreds = character(0),
#'          degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
#'          data = mtcars, elapsed = 0, trace_output = character(0)),
#'     class = "earthUI_result"
#'   )
#'   ek <- import_earth(er)
#'   specs <- list(
#'     list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'     list(vars = "hp", type = "s", bs = "tp", k = NULL)
#'   )
#'   gam_res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek)
#'   check_sign_consistency(gam_res)
#' }
check_sign_consistency <- function(gam_result) {
  ek <- gam_result$earth_knots
  if (is.null(ek)) return(NULL)

  model <- gam_result$model
  rows <- list()

  for (var in names(ek$knots)) {
    knot_vals <- ek$knots[[var]]
    earth_signs <- ek$signs[[var]]

    # Determine dominant earth direction for this variable
    earth_dir <- if (all(earth_signs == 1L)) {
      "increasing"
    } else if (all(earth_signs == -1L)) {
      "decreasing"
    } else {
      "mixed"
    }

    # Estimate GAM smooth direction at the knot points
    gam_dir <- estimate_smooth_direction_(model, var)

    consistent <- (earth_dir == "mixed") ||
      (earth_dir == gam_dir) ||
      (gam_dir == "mixed")

    warning_msg <- if (!consistent) {
      paste0("Earth suggests '", earth_dir,
             "' but GAM smooth is '", gam_dir, "' for ", var)
    } else {
      NA_character_
    }

    rows[[var]] <- data.frame(
      variable        = var,
      earth_direction = earth_dir,
      gam_direction   = gam_dir,
      consistent      = consistent,
      warning         = warning_msg,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Estimate the dominant direction of a GAM smooth
#' @param model A fitted gam object.
#' @param variable Character name of the variable.
#' @return Character: "increasing", "decreasing", or "mixed".
#' @noRd
estimate_smooth_direction_ <- function(model, variable) {
  data <- model$model
  if (!variable %in% names(data)) return("mixed")

  x_vals <- data[[variable]]
  x_seq <- seq(min(x_vals, na.rm = TRUE), max(x_vals, na.rm = TRUE),
               length.out = 100L)

  newdata <- data[rep(1L, 100L), , drop = FALSE]
  newdata[[variable]] <- x_seq

  preds <- predict(model, newdata = newdata, type = "terms")

  # Find the column corresponding to this smooth
  smooth_col <- grep(paste0("s\\(", variable), colnames(preds), value = TRUE)
  if (length(smooth_col) == 0L) return("mixed")

  vals <- preds[, smooth_col[1L]]
  diffs <- diff(vals)

  pos_frac <- mean(diffs > 0)
  if (pos_frac > 0.7) {
    "increasing"
  } else if (pos_frac < 0.3) {
    "decreasing"
  } else {
    "mixed"
  }
}


#' Reconcile smooth spec k values with earth knot counts
#'
#' For cr basis, mgcv requires k == length(knots). This adjusts k in
#' each spec to match the actual knot count when earth knots are used.
#'
#' @param smooth_specs List of smooth-term spec lists.
#' @param earth_knots An mgcvUI_earth_knots object, or NULL.
#' @return Updated smooth_specs with reconciled k values.
#' @noRd
reconcile_knots_ <- function(smooth_specs, earth_knots) {
  if (is.null(earth_knots)) return(smooth_specs)

  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next

    var <- spec$vars
    ek <- earth_knots$knots[[var]]
    if (is.null(ek)) next

    n_knots <- length(ek)

    if (identical(spec$bs, "cr")) {
      if (n_knots < 3L) {
        # Too few knots for cr basis -- fall back to tp, ignore knots
        smooth_specs[[i]]$bs <- "tp"
        smooth_specs[[i]]$k <- NULL
      } else {
        # cr basis: k must equal the number of knots supplied
        smooth_specs[[i]]$k <- n_knots
      }
    } else if (identical(spec$bs, "ps") || identical(spec$bs, "bs")) {
      # ps/bs: k must be >= number of knots; set to knots + 2 if too low
      if (!is.null(spec$k) && spec$k < n_knots) {
        smooth_specs[[i]]$k <- n_knots + 2L
      }
    }
  }

  smooth_specs
}


#' K-fold cross-validated R-squared
#' @param data Data frame.
#' @param formula Formula object.
#' @param family Family object.
#' @param method Character fitting method.
#' @param knots Knots list or NULL.
#' @param select Logical.
#' @param gamma Numeric.
#' @param k Integer number of folds.
#' @param response Character response variable name.
#' @return Numeric CV R-squared.
#' @noRd
cv_rsq_ <- function(data, formula, family, method, knots, select,
                    gamma, k, response, weights = NULL) {
  # Remove rows with NA in columns used by the model
  used_vars <- all.vars(formula)
  complete <- stats::complete.cases(data[, used_vars, drop = FALSE])
  data <- data[complete, , drop = FALSE]
  if (!is.null(weights)) weights <- weights[complete]
  n <- nrow(data)

  folds <- sample(rep(seq_len(k), length.out = n))
  preds <- numeric(n)
  failed <- 0L

  for (fold in seq_len(k)) {
    train <- data[folds != fold, , drop = FALSE]
    test  <- data[folds == fold, , drop = FALSE]
    train_wt <- if (!is.null(weights)) weights[folds != fold] else NULL

    gam_args <- list(
      formula = formula,
      data    = train,
      family  = family,
      method  = method,
      knots   = knots,
      select  = select,
      gamma   = gamma
    )
    if (!is.null(train_wt)) gam_args$weights <- train_wt

    fit <- tryCatch(
      do.call(mgcv::gam, gam_args),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      failed <- failed + 1L
      preds[folds == fold] <- NA_real_
    } else {
      preds[folds == fold] <- predict(fit, newdata = test, type = "response")
    }
  }

  if (failed > k / 2) {
    stop("Too many CV folds failed (", failed, "/", k, ").", call. = FALSE)
  }

  valid <- !is.na(preds)
  actual <- data[[response]][valid]
  p <- preds[valid]
  ss_res <- sum((actual - p)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - ss_res / ss_tot
}
