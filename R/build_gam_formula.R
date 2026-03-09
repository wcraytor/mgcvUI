#' Build a GAM Formula from Variable Specifications
#'
#' Constructs a [stats::formula] suitable for [mgcv::gam()] from a list
#' of smooth-term specifications.
#'
#' @param response Character scalar — the response variable name.
#' @param smooth_specs A list of smooth-term specification lists. Each
#'   element should have:
#'   \describe{
#'     \item{vars}{Character vector of variable names (length 1 for
#'       univariate smooths, 2+ for tensor products).}
#'     \item{type}{`"s"`, `"te"`, `"ti"`, or `"linear"`.}
#'     \item{bs}{Basis type: `"tp"`, `"cr"`, `"ps"`, `"bs"`, etc.
#'       Ignored for `"linear"` and tensor types.}
#'     \item{k}{Integer basis dimension, or `NULL` for automatic.}
#'   }
#' @return A [stats::formula] object.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "cr", k = 5L),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL),
#'   list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
#' )
#' build_gam_formula("mpg", specs)
build_gam_formula <- function(response, smooth_specs) {
  if (length(smooth_specs) == 0L) {
    stop("At least one predictor term is required.", call. = FALSE)
  }

  terms <- vapply(smooth_specs, format_smooth_term_, character(1))
  rhs <- paste(terms, collapse = " + ")
  as.formula(paste(response, "~", rhs))
}


#' Build the knots list for gam() from earth knot data
#'
#' When earth knots are available for variables using `"cr"` or `"ps"`
#' basis types, this function prepares the `knots` argument for
#' [mgcv::gam()].
#'
#' @param smooth_specs List of smooth-term specs (same as
#'   [build_gam_formula()]).
#' @param earth_knots An `mgcvUI_earth_knots` object, or `NULL`.
#' @return A named list suitable for the `knots` argument of
#'   [mgcv::gam()], or `NULL` if no knots apply.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "cr", k = 5L)
#' )
#' build_gam_knots(specs, NULL)
build_gam_knots <- function(smooth_specs, earth_knots) {
  if (is.null(earth_knots)) return(NULL)

  knots_out <- list()
  for (spec in smooth_specs) {
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next
    # Only pass knots for basis types that use them
    if (!spec$bs %in% c("cr", "ps", "bs")) next
    var <- spec$vars
    ek <- earth_knots$knots[[var]]
    if (!is.null(ek)) {
      # Trim knots to match k if k was capped
      if (!is.null(spec$k) && length(ek) > spec$k) {
        # Keep evenly spaced subset
        idx <- round(seq(1, length(ek), length.out = spec$k))
        ek <- ek[idx]
      }
      knots_out[[var]] <- ek
    }
  }

  if (length(knots_out) == 0L) NULL else knots_out
}


#' Format a single smooth term for a GAM formula
#' @param spec A smooth-term specification list.
#' @return Character string (e.g. `"s(wt, bs = \"cr\", k = 5)"`).
#' @noRd
format_smooth_term_ <- function(spec) {
  if (spec$type == "linear") {
    return(paste(spec$vars, collapse = " + "))
  }

  vars_str <- paste(spec$vars, collapse = ", ")
  parts <- vars_str

  # Basis type (only for s())
  if (spec$type == "s" && !is.null(spec$bs)) {
    parts <- c(parts, paste0("bs = \"", spec$bs, "\""))
  }

  # Basis dimension
  if (!is.null(spec$k)) {
    parts <- c(parts, paste0("k = ", spec$k))
  }

  paste0(spec$type, "(", paste(parts, collapse = ", "), ")")
}
