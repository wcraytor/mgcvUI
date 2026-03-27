## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(earthUI)
# earthUI::launch()

## ----eval=FALSE---------------------------------------------------------------
# library(earthUI)
# result <- earthUI::fit_earth(
#   df         = sales_data,
#   target     = "sale_price",
#   predictors = c("living_area", "lot_size", "year_built", "condition")
# )
# saveRDS(result, "earth_result.rds")

## ----eval=FALSE---------------------------------------------------------------
# library(mgcvUI)
# mgcvUI()

## ----eval=FALSE---------------------------------------------------------------
# library(mgcvUI)
# 
# # Import earth result and extract knots
# ek <- import_earth("earth_result.rds")
# 
# # View the knots
# str(ek$knots)
# 
# # Build GAM specs informed by earth knots
# specs <- list(
#   list(vars = "living_area", type = "s", bs = "cr",
#        k = length(ek$knots$living_area)),
#   list(vars = "lot_size", type = "s", bs = "cr",
#        k = length(ek$knots$lot_size)),
#   list(vars = "year_built", type = "s", bs = "cr",
#        k = length(ek$knots$year_built)),
#   list(vars = "condition", type = "linear", bs = NULL, k = NULL)
# )
# 
# # Fit GAM with earth knots -- note earth pipeline defaults
# result <- fit_gam(
#   data         = sales_data,
#   response     = "sale_price",
#   smooth_specs = specs,
#   earth_knots  = ek,
#   method       = "REML",
#   select       = TRUE,
#   gamma        = 1.4
# )
# 
# # Check concurvity
# mgcv::concurvity(result$model, full = TRUE)
# 
# # Check sign consistency
# check_sign_consistency(result)
# 
# # Export knots to CSV for documentation
# export_knots_csv(ek, "knot_positions.csv")
# 
# # Generate appraisal report
# render_gam_report(result, "docx", "appraisal_model_report.docx",
#                   title = "GAM Valuation Model")

