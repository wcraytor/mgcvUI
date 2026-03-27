pkgname <- "mgcvUI"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "mgcvUI-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('mgcvUI')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_gam_formula")
### * build_gam_formula

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build_gam_formula
### Title: Build a GAM Formula from Variable Specifications
### Aliases: build_gam_formula

### ** Examples

specs <- list(
  list(vars = "wt", type = "s", bs = "cr", k = 5L),
  list(vars = "hp", type = "s", bs = "tp", k = NULL),
  list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
)
build_gam_formula("mpg", specs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build_gam_formula", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("build_gam_knots")
### * build_gam_knots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build_gam_knots
### Title: Build the knots list for gam() from earth knot data
### Aliases: build_gam_knots

### ** Examples

specs <- list(
  list(vars = "wt", type = "s", bs = "cr", k = 5L)
)
build_gam_knots(specs, NULL)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build_gam_knots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check_sign_consistency")
### * check_sign_consistency

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check_sign_consistency
### Title: Check for Sign Discrepancies Between Earth and GAM
### Aliases: check_sign_consistency

### ** Examples

if (requireNamespace("earth", quietly = TRUE)) {
  m <- earth::earth(mpg ~ wt + hp, data = mtcars)
  er <- structure(
    list(model = m, target = "mpg",
         predictors = c("wt", "hp"),
         categoricals = character(0), linpreds = character(0),
         degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
         data = mtcars, elapsed = 0, trace_output = character(0)),
    class = "earthUI_result"
  )
  ek <- import_earth(er)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  gam_res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek)
  check_sign_consistency(gam_res)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check_sign_consistency", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("detect_column_types")
### * detect_column_types

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: detect_column_types
### Title: Detect Column Types from a Data Frame
### Aliases: detect_column_types

### ** Examples

detect_column_types(mtcars)
df <- data.frame(
  x = 1:5, y = letters[1:5],
  d = Sys.Date() + 1:5,
  t = Sys.time() + 1:5
)
detect_column_types(df)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("detect_column_types", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_gam_docx")
### * export_gam_docx

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_gam_docx
### Title: Export GAM Summary to Word (officer)
### Aliases: export_gam_docx

### ** Examples

if (interactive()) {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs)
  export_gam_docx(res, tempfile(fileext = ".docx"))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_gam_docx", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("export_knots_csv")
### * export_knots_csv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_knots_csv
### Title: Export Knot Positions to CSV
### Aliases: export_knots_csv

### ** Examples

if (requireNamespace("earth", quietly = TRUE)) {
  m <- earth::earth(mpg ~ wt + hp, data = mtcars)
  er <- structure(
    list(model = m, target = "mpg",
         predictors = c("wt", "hp"),
         categoricals = character(0), linpreds = character(0),
         degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
         data = mtcars, elapsed = 0, trace_output = character(0)),
    class = "earthUI_result"
  )
  knots <- import_earth(er)
  tmp <- tempfile(fileext = ".csv")
  export_knots_csv(knots, tmp)
  cat(readLines(tmp), sep = "\n")
  unlink(tmp)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_knots_csv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_smooth_grids")
### * extract_smooth_grids

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_smooth_grids
### Title: Extract Smooth Functions as Lookup Tables
### Aliases: extract_smooth_grids

### ** Examples

specs <- list(
  list(vars = "wt", type = "s", bs = "tp", k = NULL),
  list(vars = "hp", type = "s", bs = "tp", k = NULL)
)
res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
grids <- extract_smooth_grids(res, n_points = 50)
names(grids)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_smooth_grids", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_gam")
### * fit_gam

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_gam
### Title: Fit a GAM Model
### Aliases: fit_gam

### ** Examples

specs <- list(
  list(vars = "wt", type = "s", bs = "tp", k = NULL),
  list(vars = "hp", type = "s", bs = "tp", k = NULL)
)
result <- fit_gam(mtcars, "mpg", specs)
summary(result$model)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_gam", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_gam_summary")
### * format_gam_summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_gam_summary
### Title: Format GAM Summary Statistics
### Aliases: format_gam_summary

### ** Examples

specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
res <- fit_gam(mtcars, "mpg", specs)
format_gam_summary(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_gam_summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("glance_gam")
### * glance_gam

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: glance_gam
### Title: Glance at GAM Model Fit
### Aliases: glance_gam

### ** Examples

specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
res <- fit_gam(mtcars, "mpg", specs)
glance_gam(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("glance_gam", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_data")
### * import_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_data
### Title: Import Data from CSV or Excel
### Aliases: import_data

### ** Examples

tmp <- tempfile(fileext = ".csv")
write.csv(mtcars, tmp, row.names = FALSE)
df <- import_data(tmp)
head(df)
unlink(tmp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_earth")
### * import_earth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_earth
### Title: Import an Earth Model and Extract Knot Structure
### Aliases: import_earth

### ** Examples

if (requireNamespace("earth", quietly = TRUE)) {
  m <- earth::earth(mpg ~ wt + hp + disp, data = mtcars)
  er <- structure(
    list(model = m, target = "mpg",
         predictors = c("wt", "hp", "disp"),
         categoricals = character(0), linpreds = character(0),
         degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
         data = mtcars, elapsed = 0, trace_output = character(0)),
    class = "earthUI_result"
  )
  knots <- import_earth(er)
  str(knots$knots)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_earth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mgcvUI")
### * mgcvUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mgcvUI
### Title: Launch the mgcvUI Shiny Application
### Aliases: mgcvUI

### ** Examples

if (interactive()) {
  mgcvUI()
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mgcvUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_data_ui")
### * mod_data_ui

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_data_ui
### Title: Data Import Module - UI
### Aliases: mod_data_ui

### ** Examples

if (interactive()) {
  ui <- fluidPage(mod_data_ui("data1"))
  server <- function(input, output, session) {
    mod_data_server("data1")
  }
  shinyApp(ui, server)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_data_ui", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_earth_import_ui")
### * mod_earth_import_ui

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_earth_import_ui
### Title: Earth Import Module - UI
### Aliases: mod_earth_import_ui

### ** Examples

if (interactive()) {
  ui <- fluidPage(mod_earth_import_ui("earth1"))
  server <- function(input, output, session) {
    mod_earth_import_server("earth1")
  }
  shinyApp(ui, server)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_earth_import_ui", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_model_fit_ui")
### * mod_model_fit_ui

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_model_fit_ui
### Title: Model Fit Button - UI (sidebar)
### Aliases: mod_model_fit_ui

### ** Examples

if (interactive()) {
  ui <- fluidPage(mod_model_fit_ui("model1"))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_model_fit_ui", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_report_ui")
### * mod_report_ui

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_report_ui
### Title: Report Export Module - UI
### Aliases: mod_report_ui

### ** Examples

if (interactive()) {
  ui <- fluidPage(mod_report_ui("report1"))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_report_ui", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_variables_ui")
### * mod_variables_ui

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_variables_ui
### Title: Variable Selection Module - UI
### Aliases: mod_variables_ui

### ** Examples

if (interactive()) {
  ui <- fluidPage(mod_variables_ui("vars1"))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_variables_ui", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_actual_vs_predicted")
### * plot_actual_vs_predicted

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_actual_vs_predicted
### Title: Plot Actual vs Predicted
### Aliases: plot_actual_vs_predicted

### ** Examples

specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
res <- fit_gam(mtcars, "mpg", specs)
plot_actual_vs_predicted(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_actual_vs_predicted", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_diagnostics")
### * plot_diagnostics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_diagnostics
### Title: Plot GAM Residual Diagnostics
### Aliases: plot_diagnostics

### ** Examples

specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
res <- fit_gam(mtcars, "mpg", specs)
plot_diagnostics(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_diagnostics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_smooth_single")
### * plot_smooth_single

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_smooth_single
### Title: Plot a Single GAM Smooth Term
### Aliases: plot_smooth_single

### ** Examples

specs <- list(
  list(vars = "wt", type = "s", bs = "tp", k = NULL),
  list(vars = "hp", type = "s", bs = "tp", k = NULL)
)
res <- fit_gam(mtcars, "mpg", specs)
plot_smooth_single(res, "wt")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_smooth_single", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_smooths")
### * plot_smooths

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_smooths
### Title: Plot GAM Smooth Terms
### Aliases: plot_smooths

### ** Examples

specs <- list(
  list(vars = "wt", type = "s", bs = "tp", k = NULL),
  list(vars = "hp", type = "s", bs = "tp", k = NULL)
)
res <- fit_gam(mtcars, "mpg", specs)
plot_smooths(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_smooths", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("render_gam_report")
### * render_gam_report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: render_gam_report
### Title: Render a GAM Report
### Aliases: render_gam_report

### ** Examples

if (interactive()) {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs)
  render_gam_report(res, "html")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("render_gam_report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy_gam")
### * tidy_gam

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy_gam
### Title: Tidy GAM Results with broom
### Aliases: tidy_gam

### ** Examples

specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
res <- fit_gam(mtcars, "mpg", specs)
tidy_gam(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy_gam", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
