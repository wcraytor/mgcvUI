test_that("mgcvUI exists and is a function", {
  expect_true(is.function(mgcvUI))
})

test_that("mgcvUI has expected default port", {
  expect_equal(formals(mgcvUI)$port, 7880L)
})
