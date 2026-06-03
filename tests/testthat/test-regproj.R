# Tests for the regProj project model and the two-step Quarto report.

test_that("regProj path + flat-segment round-trip", {
  seg <- regproj_flat_segment("us", c("ca", "081", "oakland"), "demo_proj")
  expect_equal(seg, "us_ca_081_oakland_demo_proj")
  parsed <- regproj_parse_flat(seg)
  expect_equal(parsed$country, "us")
  expect_equal(parsed$levels, c("ca", "081", "oakland"))
  expect_equal(parsed$project_name, "demo_proj")
})

test_that("country schema + city abbreviation behave", {
  expect_equal(country_schema("us"), c("state", "county", "city"))
  expect_equal(country_schema("zz"), c("region", "city"))  # fallback
  expect_equal(city_abbreviation("Carmel-by-the-Sea"), "carmel")
  expect_equal(city_abbreviation("Oakland", c("oaklan")), "oaklan_1")
})

test_that("geo DB seeds and the project settings round-trip per method", {
  skip_if_not_installed("RSQLite")
  root <- file.path(tempdir(), paste0("regProj_test_", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  con <- regproj_geo_db_connect(root)
  n_countries <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM countries")$n
  n_states <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM admin_entries WHERE country='us' AND level=1")$n
  DBI::dbDisconnect(con)
  expect_gt(n_countries, 20L)
  expect_equal(n_states, 51L)
  expect_equal(regproj_index_get("us", "California", root = root), "ca")

  in_dir <- regproj_path("appr", "us", c("ca", "081", "oakland"), "demo",
                         in_or_out = "in", create = TRUE, root = root)
  out_dir <- regproj_path("appr", "us", c("ca", "081", "oakland"), "demo",
                          in_or_out = "out", method = "mgcv", create = TRUE,
                          root = root)
  expect_true(dir.exists(in_dir))
  expect_match(basename(out_dir), "_out_mgcv$")

  proj_dir <- dirname(out_dir)
  register_project(proj_dir, "appr", "us", c("ca", "081", "oakland"), "demo",
                   root = root)
  set_project_settings(proj_dir, settings = '{"family":"gaussian"}',
                       variables = '{"resp":"price"}', method = "mgcv",
                       purpose = "appraisal", root = root)
  got <- get_project_settings(proj_dir, method = "mgcv", purpose = "appraisal",
                              root = root)
  expect_equal(got$settings, '{"family":"gaussian"}')
  # A different method does not see the mgcv row's data.
  expect_null(get_project_settings(proj_dir, method = "earth",
                                   purpose = "appraisal", root = root))

  lp <- regproj_list_projects(root = root)
  expect_equal(nrow(lp), 1L)
  expect_equal(lp$project_name, "demo")
})

test_that("prepare_report_assets + generate_quarto_report build a bundle", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL),
                list(vars = "disp", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  expect_s3_class(res, "mgcvUI_result")

  dest <- file.path(tempdir(), paste0("rep_", as.integer(runif(1, 1, 1e6))))
  dir.create(dest, showWarnings = FALSE)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  qmd <- generate_quarto_report(res, dest_dir = dest, base = "gam_report")
  expect_true(file.exists(qmd))
  bundle <- dirname(qmd)
  expect_true(file.exists(file.path(bundle, "report_data.rds")))
  expect_true(dir.exists(file.path(bundle, "plots")))
  expect_gt(length(list.files(file.path(bundle, "plots"))), 0L)

  payload <- readRDS(file.path(bundle, "report_data.rds"))
  expect_equal(payload$result$response, "mpg")
  expect_true(is.list(payload$summary_info))
  # The heavy fitted model must NOT be in the lean payload.
  expect_null(payload$result$model)
})
