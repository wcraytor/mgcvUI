# mgcvUI 0.2.0

## NA handling and data safety

- Variable table now shows NA count and percentage per column,
  color-coded: red (>= 50%), orange (20-49%), gray (< 20%).
- Variables with > 50% NAs are auto-excluded from the model regardless
  of saved settings or earthUI import.
- Pre-fit complete-case check warns when < 50% of rows are complete
  (listing NA culprits) and blocks fitting when < 10 complete rows
  remain.
- `fit_gam()` now subsets data to only model-relevant columns before
  passing to `mgcv::gam()`, preventing NAs in unrelated columns from
  dropping rows via `na.action`.
- k-capping logic uses the complete-case subset across all model
  variables, not per-variable unique counts.

## earthUI integration enhancements

- Import `degree`, `allowed_matrix`, `linpreds`, and `categoricals`
  from earthUI result objects.
- earthUI allowed matrix and detected interactions are imported into
  the Allowed Interactions matrix automatically.
- Linear predictor and categorical designations from earthUI override
  variable table defaults.
- Earth knot reconciliation: when cr basis has < 3 knots, falls back
  to tp basis. Knots are trimmed when k is capped below knot count.
- Degree-1 earthUI results show info message (non-blocking) in the
  interactions matrix.

## Weights support

- Weights column selector in Variable Configuration, matching earthUI
  workflow.
- Weights vector threaded through `fit_gam()` to `mgcv::gam()` and
  cross-validation.
- Weights column persisted in settings database across sessions.

## Allowed interactions matrix

- Click on a variable name to toggle all its interactions on/off.
- Allow All / Clear All buttons for bulk selection.
- earthUI interactions and allowed matrix imported automatically.
- Tensor type selector (ti/te) for interaction terms.

## Model equation display

- MathJax-rendered equation with smooth function definitions listed
  below.
- Factor variables grouped as single term with level count instead of
  individual coefficients.
- Proper handling of underscores in variable names within `\text{}`.

## Settings persistence

- SQLite schema migration adds `output_folder`, `effective_date`,
  `purpose`, and `weights_col` columns.
- Merge-based settings save prevents field overwriting between modules.
- Project output folder, effective date, and purpose mode saved and
  restored per dataset.
- Uploaded data files and earthUI result files cached for cross-session
  persistence (auto-loaded on startup).

## Anova table

- Fixed `cannot coerce class 'anova.gam' to a data.frame` error by
  extracting parametric (`p.table`) and smooth (`s.table`) components
  separately from the model summary.

## Upload limits

- Maximum upload size increased to 200 MB for large earthUI result
  files.

## Per-term contributions and adjustments

- Excel output includes intercept (basis), per-term contributions, and
  residuals for every observation.
- RCA adjustment mode computes per-comparable adjustments, net/gross
  adjustment totals and percentages, CQA and CQA/SF scores.
- RCA adjustment percentage histograms with mean/median/std dev
  annotations.

## Theme

- Nord Light and Nord Dark themes via bslib.
- Theme preference persisted in localStorage.
- DataTables, variable table, and collapsible sections adapt to
  current theme.

---

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
