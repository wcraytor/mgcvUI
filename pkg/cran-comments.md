## R CMD check results

0 errors | 0 warnings | 2 notes

* "New submission" (the package is not currently on CRAN).
* "Package vignette without corresponding tangle output:
  mgcvUI-user-guide.Rmd" — the User Guide vignette is prose with no
  executable R code chunks, so there is nothing to tangle. This is
  expected and harmless.

## Test environments

* macOS Tahoe 26.5 (aarch64), R 4.5.3 — local R CMD check --as-cran
* GitHub Actions (windows-latest, macos-latest, ubuntu-latest) via the
  r-lib check-r-package workflow (release, devel, oldrel-1)

## Submission notes

mgcvUI provides a 'shiny' GUI for building, diagnosing, and reporting
generalized additive models with 'mgcv'. It shares the maintainer's
"regProj" project system with the sibling apps (earthUI, already on CRAN;
glmnetUI). Projects, geo reference data, and per-project settings are
stored in SQLite databases under a configurable root directory, resolved
in this order: the REGPROJ_ROOT environment variable, a `regproj_root`
field in the per-user preferences file (stored under
tools::R_user_dir("mgcvUI", "config")), and finally a per-OS default.
These files are created only when the user runs the interactive
application and saves a project — never on package load, in examples, in
tests, or in vignettes. All examples that launch the app or write files
are guarded by `if (interactive())`, and all tests write only to
tempdir().

Software names in the Title and Description are single-quoted, the
underlying methods carry a DOI reference (Wood, 2011), and all exported
functions document their return value with \value.

## Downstream dependencies

This is a new package with no downstream dependencies.
