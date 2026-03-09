# mgcvUI 0.1.0

Initial release.

## Shiny application

- Interactive GAM builder with point-and-click variable selection,
  smooth type configuration, and one-click fitting.
- Variable table with Include, Type (tp/cr/ps/bs), and Linear
  checkboxes for each predictor.
- Family (gaussian, Gamma, inverse.gaussian, poisson), method
  (REML/GCV.Cp/ML), select, and gamma controls.
- Dark mode toggle via bslib themes.

## Data handling

- CSV and Excel import with automatic column-name cleaning.
- Date and POSIXct columns converted to numeric for mgcv compatibility.
- Low-cardinality handling: variables with 1 unique value are dropped,
  binary (2 unique values) are converted to factor, and k is capped to
  the number of unique values when fewer than the default.

## Model fitting

- `fit_gam()` wraps `mgcv::gam()` with defensive NULL handling,
  data-type coercion, and low-cardinality logic.
- 10-fold cross-validated R-squared with per-fold error tolerance.
- Status bar shows R-squared, CV R-squared, deviance explained, AIC,
  and sample size.

## Earth/MARS integration

- `import_earth()` extracts knot positions from earth models.
- Knot counts are reconciled with mgcv basis requirements (cr, ps, bs).
- `check_sign_consistency()` compares earth hinge directions with GAM
  smooth directions.

## Visualization

- Interactive plotly smooth plots with hover tooltips showing value,
  contribution, 95% CI, and rate of change per unit.
- Lat/long variables show rate per 0.001 degrees.
- Smooth plots stacked vertically (400px each) with comma-formatted
  y-axis labels.
- Static ggplot2 smooth plots and diagnostics panels.
- Actual vs predicted scatter plot.

## Function export

- `extract_smooth_grids()` evaluates fitted smooths on a fine grid.
- `generate_r_code()` produces self-contained R scripts using
  `approxfun`.
- `generate_python_code()` produces Python scripts using
  `numpy.interp`.
- `generate_cpp_code()` produces C++ headers with linear interpolation.
- `export_functions_sqlite()` writes grids and model metadata to SQLite.
- `export_functions_zip()` bundles selected languages into a zip
  download.

## Reporting

- R Markdown report generation with model summary, smooth plots,
  diagnostics, basis dimension checks, and sign-consistency tables.
- Word document export via officer.

## Persistent settings

- Per-dataset model settings (response, variable selections, family,
  method, select, gamma) stored in a local SQLite database and restored
  on reload.

## Testing

- 129 unit tests covering formula building, data conversion,
  low-cardinality handling, CV R-squared, import/export, plotting, and
  settings persistence.
- Passes R CMD check with 0 errors, 0 warnings, 0 notes.
- lintr clean (1 intentional exception: `mgcvUI` package-name function).
