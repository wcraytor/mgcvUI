#' Check if a LaTeX installation is available
#'
#' @return Logical `TRUE` if pdflatex, xelatex, lualatex, or TinyTeX
#'   is found.
#' @noRd
has_latex_ <- function() {
  nzchar(Sys.which("pdflatex")) ||
    nzchar(Sys.which("xelatex")) ||
    nzchar(Sys.which("lualatex")) ||
    (requireNamespace("tinytex", quietly = TRUE) &&
       tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))
}


#' Render a GAM Report
#'
#' Generates an HTML, PDF, or Word report for a fitted GAM model.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param output_format Character: `"html"`, `"pdf"`, or `"docx"`.
#'   Default `"html"`.
#' @param output_file Character path for the output file. If `NULL`
#'   (default), a temporary file is created.
#' @param title Character report title. Default
#'   `"GAM Model Report"`.
#' @return The output file path (invisibly).
#' @export
#' @examples
#' if (interactive()) {
#'   specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#'   res <- fit_gam(mtcars, "mpg", specs)
#'   render_gam_report(res, "html")
#' }
render_gam_report <- function(gam_result, output_format = "html",
                              output_file = NULL, title = "GAM Model Report") {
  template <- system.file("rmd", "gam_report.Rmd", package = "mgcvUI")
  if (!nzchar(template)) {
    stop("Report template not found. Try re-installing mgcvUI.",
         call. = FALSE)
  }

  fmt <- switch(output_format,
    html = "html_document",
    pdf  = "pdf_document",
    docx = "word_document",
    stop("Unsupported format: ", output_format,
         ". Use html, pdf, or docx.", call. = FALSE)
  )

  if (is.null(output_file)) {
    ext <- switch(output_format, html = ".html", pdf = ".pdf", docx = ".docx")
    output_file <- tempfile(fileext = ext)
  }

  # Save result object to temp file for the Rmd to load
  result_path <- tempfile(fileext = ".rds")
  saveRDS(gam_result, result_path)

  rmarkdown::render(
    input       = template,
    output_format = fmt,
    output_file = output_file,
    params      = list(
      result_path = result_path,
      title       = title
    ),
    envir       = new.env(parent = globalenv()),
    quiet       = TRUE
  )

  unlink(result_path)
  invisible(output_file)
}


#' Export GAM Summary to Word (officer)
#'
#' Creates a Word document with model summary tables using
#' \pkg{officer}.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param file Character output file path.
#' @return The file path (invisibly).
#' @export
#' @examples
#' if (interactive()) {
#'   specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#'   res <- fit_gam(mtcars, "mpg", specs)
#'   export_gam_docx(res, tempfile(fileext = ".docx"))
#' }
export_gam_docx <- function(gam_result, file) {
  summ <- format_gam_summary(gam_result)
  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc, "GAM Model Report",
                               style = "heading 1")
  doc <- officer::body_add_par(doc, paste("Date:", Sys.Date()))
  doc <- officer::body_add_par(doc, "")

  # Model overview
  doc <- officer::body_add_par(doc, "Model Overview", style = "heading 2")
  overview <- data.frame(
    Metric = c("R-squared", "Deviance Explained", "AIC", "BIC",
               "Observations", "Smooth Terms", "Method", "Family"),
    Value = c(
      round(summ$r_squared, 4),
      paste0(round(summ$dev_explained * 100, 1), "%"),
      round(summ$aic, 1),
      round(summ$bic, 1),
      summ$n_obs,
      summ$n_smooths,
      summ$method,
      summ$family
    ),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, overview, style = "table_template")

  # Smooth terms
  if (nrow(summ$smooth_table) > 0L) {
    doc <- officer::body_add_par(doc, "Smooth Terms", style = "heading 2")
    st <- summ$smooth_table
    st$EDF <- round(st$EDF, 2)
    st$Ref.df <- round(st$Ref.df, 2)
    st$F <- round(st$F, 2)
    st$p_value <- format.pval(st$p_value, digits = 3)
    doc <- officer::body_add_table(doc, st, style = "table_template")
  }

  # Parametric terms
  if (nrow(summ$parametric_table) > 0L) {
    doc <- officer::body_add_par(doc, "Parametric Terms",
                                 style = "heading 2")
    pt <- summ$parametric_table
    pt$Estimate <- round(pt$Estimate, 4)
    pt$Std_Error <- round(pt$Std_Error, 4)
    pt$t_value <- round(pt$t_value, 3)
    pt$p_value <- format.pval(pt$p_value, digits = 3)
    doc <- officer::body_add_table(doc, pt, style = "table_template")
  }

  print(doc, target = file)
  invisible(file)
}
