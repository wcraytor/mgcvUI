# mgcvUI 0.3.0

## Data Preview

- **Full-text cell pop-up**: Double-click any cell in the Data Preview to open a pop-up with its complete contents. Cells are now truncated to the column width and kept to a single line, so long free-text fields (e.g. property remarks) no longer stretch a row down the page. Single-click still selects a row.

## earthUI Import

- **Earth-seed note**: When an earthUI result is imported, the Variable
  Configuration table now shows a note explaining that the variable
  selections (Include / Linear / Factor) and spline knots were *seeded* from
  the earth model. Unlike glmnetUI (whose glmnet fit is bound to earth's
  basis), mgcvUI fits the GAM from the user's selections, so every control
  stays fully editable — earth only provides a head start. A new test pins
  this seed-but-editable behavior.

## CRAN preparation

- Cleaned `R CMD check --as-cran`: escaped non-ASCII characters in
  `R/fit_gam.R` and `R/mod_data.R`, and documented the `select` argument of
  `plot_smooths()` (resolving a code/documentation mismatch). The check now
  reports a single expected "New submission" note.

## regProj project model (shared with earthUI / glmnetUI)

- Replaced the flat file-upload with a **Project** section backed by the
  shared regProj tree and SQLite databases at `~/regProj` (override with the
  `REGPROJ_ROOT` env var or the regProj-root field in Settings). Projects,
  input files, and per-method outputs are shared across the sibling apps;
  mgcvUI reads/writes the `mgcv` settings columns and writes to
  `<os>_out_mgcv`.
- New **Project** section: pick an existing project or create one via a
  country / state / county / city geo cascade. Import Data now lists the
  files in the active project's `<os>_in/` folder. The purpose and output
  folder are derived from the active project (the manual output-folder box
  was removed).
- New exported functions: `regproj_path()`, `regproj_list_projects()`,
  `regproj_geo_db_connect()`, `regproj_projects_db_connect()`,
  `get_project_settings()`, `set_project_settings()`, `register_project()`,
  and supporting helpers.

## Two-step Quarto report

- The single-step report was replaced by a two-step Quarto flow:
  **Generate Quarto Report** writes a self-contained `.qmd` bundle (source +
  pre-rendered plots + `report_data.rds` + `reference.docx`) to the project's
  mgcv output folder; **Convert Quarto Report** renders any `.qmd` to HTML /
  Word / PDF via `quarto::quarto_render()`.
- New exported functions: `prepare_report_assets()`,
  `generate_quarto_report()`, `convert_quarto_file()`. The legacy
  `render_gam_report()` / `export_gam_docx()` remain for programmatic use;
  `officer` moved to Suggests.
- New dependencies promoted to Imports: `DBI`, `RSQLite`, `jsonlite`,
  `quarto`.


# mgcvUI 0.2.0

## Contribution plots for all term types

- Interactive plotly contribution plots now generated for all model
  terms that contribute to predictions, not just univariate smooths:
  - **Interaction heatmaps** for `ti()` and `te()` tensor product terms
  - **Factor-by-smooth line plots** for `s(x, by=factor)` terms with
    one colored line per factor level
  - **Parametric term plots** for linear numeric terms (scatter + line)
    and factor terms (bar chart)
- New exported functions: `plot_interaction_interactive()`,
  `plot_by_smooth_interactive()`, `plot_parametric_interactive()` (plotly),
  and `plot_interaction_single()`, `plot_by_smooth_single()`,
  `plot_parametric_single()` (static ggplot2 for reports).

## Predictor Settings table improvements

- Checkbox columns (Include, Factor, Linear) grouped together after
  the Type column with rotated vertical header labels matching glmnetUI.
- "Inc" label renamed to "Include".
- Visible borders around Variable name, Type dropdown, Special dropdown,
  and NAs cells.

## Report generation overhaul

- **Equation section** added to all report formats (HTML, PDF, Word):
  model equation with f_i notation for smooth terms, explicit
  coefficients for linear terms, smooth function definitions table.
- **All contribution plots** included in reports: univariate smooths,
  interaction heatmaps, factor-by-smooth plots, and parametric term
  plots.
- **PDF equation wrapping**: long equations use LaTeX `aligned`
  environment with line breaks every 3 terms.
- **LaTeX underscore escaping** prevents PDF compilation failure from
  variable names containing underscores.
- **Word report Equation section** includes family/method, equation
  text, smooth definitions list, and smooth specs table.
- **Diagnostics fallback**: `gratia::appraise()` failure caught in all
  report formats with `gam.check()` fallback (PDF/HTML) or placeholder
  (Word).
- **ANOVA table** split into separate Parametric and Smooth tables.
- **Concurvity** overall table transposed for readability; pairwise
  table uses shortened labels.
- **Correlation heatmap** legend moved to bottom to prevent truncation.
- All report figures centered with `fig.align = "center"`, reduced
  widths to prevent right-side clipping.
- Every Rmd chunk wrapped in `tryCatch` so individual section failures
  produce inline error text instead of aborting the report.

## Settings persistence expanded

- `response_transform`, `cv`, `default_basis`, `default_k`,
  `tensor_type`, `optimizer`, `scale`, `discrete`, and `nthreads` now
  persisted in SQLite database. Previously only saved to UI state.
- Log10/Log transform selection remembered across sessions.

## Cross-platform robustness

- **R version check**: launch functions require R >= 4.1.0 with clear
  error message.
- **LaTeX detection**: PDF report option automatically hidden when no
  LaTeX installation found (`has_latex_()` helper).
- **Pandoc detection**: HTML report option hidden when pandoc not
  available. Word (officer) always available.
- **Font fallback**: `font_add_google("Roboto Condensed")` wrapped in
  `tryCatch`; offline machines fall back to system sans-serif.
- **CSS font stacks**: replaced `monospace` fallback with
  `'Arial Narrow', Helvetica, Arial, sans-serif`.
- **SQLite resilience**: database connection and directory creation
  wrapped in `tryCatch`; read-only filesystems get console warning,
  app continues without persistence.
- **Temp directory check**: report generation verifies `tempdir()` is
  writable before proceeding.
- **Upload limit**: `shiny.maxRequestSize` set to 3 GB.
- **`plot_diagnostics()` self-healing**: `gratia::appraise()` failure
  caught inside the function itself, returns placeholder ggplot.

## UI improvements

- White checkmarks on buttons cleared when switching purpose mode.
- Unescaped double quote in JavaScript (`input[type="file"]`) fixed,
  preventing app startup crash.

## Documentation

- **User Guide**: new "System Requirements & Troubleshooting" appendix
  covering platform notes, optional dependencies, graceful degradation
  table, and troubleshooting FAQ.
- **README.md**: System Requirements section added.
- **Getting Started vignette**: Prerequisites section added.
- **CLAUDE.md**: R `tryCatch` scoping bug and Shiny scoping traps
  documented in Common Pitfalls.

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
