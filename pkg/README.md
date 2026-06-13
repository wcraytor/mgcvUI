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
- **NA-aware fitting** -- variable table shows NA counts and percentages
  per column (color-coded). Variables with > 50% NAs are auto-excluded.
  Pre-fit checks warn or block when too few complete cases remain.
  Only model-relevant columns are passed to mgcv to prevent unrelated
  NAs from dropping rows.
- **Earth/MARS integration** -- optionally import knot positions from an
  `earth` model (e.g. via
  [earthUI](https://github.com/wcraytor/earthUI)) to seed GAM smooths.
  Imports degree, allowed interaction matrix, linear predictors, and
  categorical designations from earthUI results.
- **Allowed interactions matrix** -- configure tensor product
  interactions (ti/te) between smooth predictors. Click variable names
  to toggle all interactions for that variable. earthUI allowed matrix
  and interactions are imported automatically.
- **Weights support** -- specify a weights column for weighted GAM
  fitting, matching the earthUI workflow.
- **Cross-validated R-squared** -- 10-fold CV R-squared is computed
  alongside in-sample statistics so you can gauge out-of-sample
  performance.
- **Interactive smooth plots** -- plotly-based plots show each smooth's
  contribution with hover tooltips displaying the value, 95% confidence
  interval, and rate of change per unit (per 0.001 degrees for
  lat/long).
- **Model equation** -- MathJax-rendered equation showing smooth
  function definitions, with factor variables grouped by level count.
- **Sign consistency checks** -- when earth knots are imported, the
  direction of each GAM smooth is compared against the earth hinge
  direction.
- **Per-term contributions and adjustments** -- Excel output includes
  intercept (basis), per-term contributions, and residuals for every
  observation. Appraisal mode computes RCA adjustments with CQA scores.
- **Function export** -- export fitted smooth functions as self-contained
  R, Python, or C++ code (lookup-table interpolation), or as a SQLite
  database. Download as a zip bundle.
- **Reporting** -- generate Word or PDF reports with model summary,
  diagnostics, smooth plots, and sign-consistency tables.
- **Persistent settings** -- variable selections, model options, project
  output folder, effective date, purpose, and weights column are saved
  per dataset in a local SQLite database and restored on reload.
  Uploaded data and earthUI files are cached for cross-session
  persistence.
- **Dark mode** -- toggle between Nord Light and Nord Dark themes.

## System Requirements

- **R** >= 4.1.0 (RStudio Desktop recommended)
- **All platforms**: HTML and Word reports work out of the box
- **PDF reports**: require a LaTeX installation. If not detected, the
  PDF option is automatically hidden. Install with:
  `tinytex::install_tinytex()`
- **Linux**: may need system libraries before package installation:
  `sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
  libsqlite3-dev libfontconfig1-dev`

See the **System Requirements & Troubleshooting** appendix in the
[User Guide](docs/mgcvUI-User-Guide.pdf) for full platform details.

## Installation

```r
# Install from GitHub (not yet on CRAN). The R package lives in the
# pkg/ subdirectory of the repo, so point pak at it:
# install.packages("pak")
pak::pak("wcraytor/mgcvUI/pkg")
```

## Quick start

```r
library(mgcvUI)
mgcvUI()
```

This launches the Shiny app on port 7880 by default. Load a CSV or Excel
file, pick a response variable, check the predictors you want, and click
**Fit Model**.

## earthUI integration

mgcvUI works as a companion to
[earthUI](https://github.com/wcraytor/earthUI). When earthUI degree is
<= 2, export the result as an .rds file and import it into mgcvUI to:

- Seed GAM smooths with MARS knot positions (cr basis)
- Import variable selections, linear predictor designations, and
  categorical types
- Import the allowed interaction matrix for tensor products
- Compare GAM smooth directions against earth hinge directions

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
rmarkdown, broom, gratia, plotly, rlang, stats, tools, utils

**Suggests:** earth (>= 5.3.0), DBI, RSQLite, jsonlite, bslib,
testthat, knitr, sysfonts, showtext, thematic, writexl

## License

AGPL (>= 3)
