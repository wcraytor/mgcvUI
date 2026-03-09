# mgcvUI

Interactive GAM Builder for Real Estate Appraisers

mgcvUI is a [Shiny](https://shiny.posit.co/) application for building,
diagnosing, and reporting Generalized Additive Models (GAMs) via
[mgcv](https://cran.r-project.org/package=mgcv). It is designed for real
estate appraisers who need transparent, defensible nonlinear models.

## Features

- **Point-and-click GAM building** -- load CSV/Excel data, select a
  response and predictors, choose smooth types, and fit with one click.
- **Automatic data handling** -- Date/POSIXct columns are converted to
  numeric, low-cardinality variables are capped or converted to factors,
  and constants are dropped automatically.
- **Earth/MARS integration** -- optionally import knot positions from an
  `earth` model (e.g. via [earthUI](https://github.com/earthUI/earthUI))
  to seed GAM smooths.
- **Cross-validated R-squared** -- 10-fold CV R-squared is computed
  alongside in-sample statistics so you can gauge out-of-sample
  performance.
- **Interactive smooth plots** -- plotly-based plots show each smooth's
  contribution with hover tooltips displaying the value, 95% confidence
  interval, and rate of change per unit (per 0.001 degrees for
  lat/long).
- **Sign consistency checks** -- when earth knots are imported, the
  direction of each GAM smooth is compared against the earth hinge
  direction.
- **Function export** -- export fitted smooth functions as self-contained
  R, Python, or C++ code (lookup-table interpolation), or as a SQLite
  database. Download as a zip bundle.
- **Reporting** -- generate an R Markdown report with model summary,
  diagnostics, smooth plots, and sign-consistency tables.
- **Persistent settings** -- variable selections and model options are
  saved per dataset in a local SQLite database so they are restored
  automatically on reload.
- **Dark mode** -- toggle between light and dark themes.

## Installation

```r
# Install from GitHub (not yet on CRAN)
# install.packages("pak")
pak::pak("mgcvUI/mgcvUI")
```

## Quick start

```r
library(mgcvUI)
mgcvUI()
```

This launches the Shiny app on port 7880 by default. Load a CSV or Excel
file, pick a response variable, check the predictors you want, and click
**Fit Model**.

## Programmatic usage

The core functions can be used outside the Shiny app:

```r
library(mgcvUI)

specs <- list(
  list(vars = "wt", type = "s", bs = "tp", k = NULL),
  list(vars = "hp", type = "s", bs = "tp", k = NULL)
)

result <- fit_gam(mtcars, "mpg", specs)
summary(result$model)

# Interactive smooth plot
plot_smooth_interactive(result, "wt")

# Export smooth functions as R code
cat(generate_r_code(result))
```

## Dependencies

**Imports:** shiny, mgcv, readr, readxl, DT, ggplot2, officer,
rmarkdown, broom, gratia, plotly, rlang

**Suggests:** earth, DBI, RSQLite, jsonlite, bslib, testthat, knitr

## License

AGPL (>= 3)
