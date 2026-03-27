# test-launch.R — Tests for the mgcvUI launcher

test_that("mgcvUI is an exported function", {
  expect_true(is.function(mgcvUI))
})

test_that("mgcvUI has expected default port", {
  expect_equal(formals(mgcvUI)$port, 7880L)
})

test_that("mgcvUI accepts custom port", {
  fmls <- formals(mgcvUI)
  expect_true("port" %in% names(fmls))
})

test_that("mgcvUI accepts ... for shiny::runApp", {
  fmls <- formals(mgcvUI)
  expect_true("..." %in% names(fmls))
})

test_that("app.R exists in inst/app", {
  app_dir <- system.file("app", package = "mgcvUI")
  expect_true(nzchar(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})
