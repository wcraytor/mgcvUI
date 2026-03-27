## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(mgcvUI)
# mgcvUI()

## -----------------------------------------------------------------------------
library(mgcvUI)

specs <- list(
  list(vars = "wt", type = "s", bs = "tp", k = NULL),
  list(vars = "hp", type = "s", bs = "tp", k = NULL)
)

result <- fit_gam(mtcars, "mpg", specs)
format_gam_summary(result)

## ----fig.width=7, fig.height=4------------------------------------------------
plot_smooths(result)

## ----fig.width=6, fig.height=5------------------------------------------------
plot_actual_vs_predicted(result)

