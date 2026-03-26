# test-settings_db_locale.R — Tests for locale persistence in settings DB

local_test_settings_db()

test_that("settings_db_write_locale_ and read_locale_ round-trip", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  locale_settings <- list(
    locale_country = "de",
    locale_paper = "a4",
    locale_csv_sep = ";",
    locale_dec = ",",
    locale_date = "dmy"
  )
  mgcvUI:::settings_db_write_locale_(locale_settings)

  saved <- mgcvUI:::settings_db_read_locale_()
  expect_false(is.null(saved))
  expect_equal(saved$locale_country, "de")
  expect_equal(saved$locale_paper, "a4")
  expect_equal(saved$locale_csv_sep, ";")
  expect_equal(saved$locale_dec, ",")
  expect_equal(saved$locale_date, "dmy")
})

test_that("settings_db_write_locale_ overwrites previous", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  mgcvUI:::settings_db_write_locale_(list(locale_country = "fr"))
  mgcvUI:::settings_db_write_locale_(list(locale_country = "gb"))

  saved <- mgcvUI:::settings_db_read_locale_()
  expect_equal(saved$locale_country, "gb")
})

test_that("settings_db_read_locale_ returns NULL when empty", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- mgcvUI:::settings_db_connect_()
  DBI::dbExecute(con, "DELETE FROM settings_v2 WHERE filename = '__locale_defaults__'")
  DBI::dbDisconnect(con)

  saved <- mgcvUI:::settings_db_read_locale_()
  expect_null(saved)
})

test_that("locale defaults don't interfere with file settings", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  # Save locale defaults
  mgcvUI:::settings_db_write_locale_(list(locale_country = "jp"))

  # Save file settings
  fname <- paste0("locale_test_", as.numeric(Sys.time()), ".csv")
  settings <- list(
    response = "price",
    variables = list(sqft = list(inc = TRUE)),
    family = "gaussian",
    method = "REML",
    select = FALSE,
    gamma = 1
  )
  mgcvUI:::settings_db_write_(fname, settings)

  # Both should be independently retrievable
  locale <- mgcvUI:::settings_db_read_locale_()
  expect_equal(locale$locale_country, "jp")

  file_settings <- mgcvUI:::settings_db_read_(fname)
  expect_equal(file_settings$response, "price")
})

test_that("locale write with partial fields works", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  mgcvUI:::settings_db_write_locale_(list(locale_country = "se"))
  saved <- mgcvUI:::settings_db_read_locale_()
  expect_equal(saved$locale_country, "se")
  expect_null(saved$locale_paper)
})

test_that("settings_db_connect_ creates table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- mgcvUI:::settings_db_connect_()
  on.exit(DBI::dbDisconnect(con))
  expect_true("settings_v2" %in% DBI::dbListTables(con))
})

test_that("settings_db_path_ returns a sqlite path", {
  withr::with_options(list(mgcvUI.settings_db_path = NULL), {
    path <- mgcvUI:::settings_db_path_()
    expect_true(grepl("settings\\.sqlite$", path))
  })
})

test_that("jsonlite_encode_ handles NULL", {
  result <- mgcvUI:::jsonlite_encode_(NULL)
  expect_equal(result, "{}")
})

test_that("jsonlite_encode_ handles empty list", {
  result <- mgcvUI:::jsonlite_encode_(list())
  expect_equal(result, "{}")
})

test_that("jsonlite_decode_ handles NULL", {
  result <- mgcvUI:::jsonlite_decode_(NULL)
  expect_equal(result, list())
})

test_that("jsonlite_decode_ handles empty string", {
  result <- mgcvUI:::jsonlite_decode_("")
  expect_equal(result, list())
})

test_that("jsonlite_decode_ handles empty JSON", {
  result <- mgcvUI:::jsonlite_decode_("{}")
  expect_equal(result, list())
})

test_that("jsonlite_encode_ and decode_ round-trip", {
  skip_if_not_installed("jsonlite")
  x <- list(a = 1, b = "hello", c = TRUE)
  json <- mgcvUI:::jsonlite_encode_(x)
  result <- mgcvUI:::jsonlite_decode_(json)
  expect_equal(result$a, 1)
  expect_equal(result$b, "hello")
  expect_equal(result$c, TRUE)
})

test_that("settings_db_evict_ respects max_files", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  con <- mgcvUI:::settings_db_connect_()
  on.exit(DBI::dbDisconnect(con))

  # Insert a bunch of test entries
  for (i in seq_len(5)) {
    fname <- paste0("evict_test_", i, "_", as.numeric(Sys.time()), ".csv")
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO settings_v2 (filename, response, variables, updated_at)
      VALUES (?, 'y', '{}', datetime('now'))
    ", params = list(fname))
  }

  # Evict should not error
  expect_no_error(mgcvUI:::settings_db_evict_(con, max_files = 1000L))
})
