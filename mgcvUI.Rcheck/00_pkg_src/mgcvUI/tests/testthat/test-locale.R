# test-locale.R — Tests for locale/internationalization feature

local_test_settings_db()

# Reset locale to US after each test to avoid cross-contamination
withr_reset <- function() mgcvUI:::set_locale_("us")

# ---- Country presets ----

test_that("all country presets have required fields", {
  presets <- mgcvUI:::locale_country_presets_()
  required <- c("csv_sep", "csv_dec", "big_mark", "dec_mark", "date_fmt", "paper")
  for (code in names(presets)) {
    for (field in required) {
      expect_true(!is.null(presets[[code]][[field]]),
                  info = paste("Country", code, "missing field", field))
    }
  }
})

test_that("country choices match preset keys", {
  choices <- mgcvUI:::locale_country_choices_()
  presets <- mgcvUI:::locale_country_presets_()
  expect_true(all(choices %in% names(presets)))
})

test_that("all presets have valid values", {
  presets <- mgcvUI:::locale_country_presets_()
  for (code in names(presets)) {
    p <- presets[[code]]
    expect_true(p$csv_sep %in% c(",", ";"), info = paste(code, "csv_sep"))
    expect_true(p$csv_dec %in% c(".", ","), info = paste(code, "csv_dec"))
    expect_true(p$date_fmt %in% c("mdy", "dmy", "ymd"), info = paste(code, "date_fmt"))
    expect_true(p$paper %in% c("letter", "a4"), info = paste(code, "paper"))
    # csv_sep and csv_dec must differ
    expect_true(p$csv_sep != p$csv_dec,
                info = paste(code, "csv_sep and csv_dec must differ"))
  }
})

test_that("country choices have display names", {
  choices <- mgcvUI:::locale_country_choices_()
  expect_true(all(nzchar(names(choices))))
  expect_true("United States" %in% names(choices))
  expect_true("Germany" %in% names(choices))
})

test_that("preset count matches choices count", {
  presets <- mgcvUI:::locale_country_presets_()
  choices <- mgcvUI:::locale_country_choices_()
  expect_equal(length(presets), length(choices))
})

# ---- set_locale_ / get_locale_ ----

test_that("set_locale_ applies country defaults", {
  mgcvUI:::set_locale_("de")
  on.exit(withr_reset())
  loc <- mgcvUI:::get_locale_()
  expect_equal(loc$csv_sep, ";")
  expect_equal(loc$csv_dec, ",")
  expect_equal(loc$big_mark, ".")
  expect_equal(loc$dec_mark, ",")
  expect_equal(loc$date_fmt, "dmy")
  expect_equal(loc$paper, "a4")
})

test_that("set_locale_ applies overrides on top of country", {
  mgcvUI:::set_locale_("us", csv_sep = ";", paper = "a4")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_csv_sep_(), ";")
  expect_equal(mgcvUI:::locale_paper_(), "a4")
  # Non-overridden fields keep US defaults
  expect_equal(mgcvUI:::locale_csv_dec_(), ".")
  expect_equal(mgcvUI:::locale_big_mark_(), ",")
})

test_that("set_locale_ falls back to US for unknown country", {
  mgcvUI:::set_locale_("xx")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")
  expect_equal(mgcvUI:::locale_paper_(), "letter")
})

test_that("get_locale_ returns all 6 fields", {
  loc <- mgcvUI:::get_locale_()
  expect_true(all(c("csv_sep", "csv_dec", "big_mark",
                     "dec_mark", "date_fmt", "paper") %in% names(loc)))
  expect_equal(length(loc), 6L)
})

test_that("convenience accessors match get_locale_", {
  mgcvUI:::set_locale_("fr")
  on.exit(withr_reset())
  loc <- mgcvUI:::get_locale_()
  expect_equal(mgcvUI:::locale_csv_sep_(), loc$csv_sep)
  expect_equal(mgcvUI:::locale_csv_dec_(), loc$csv_dec)
  expect_equal(mgcvUI:::locale_big_mark_(), loc$big_mark)
  expect_equal(mgcvUI:::locale_dec_mark_(), loc$dec_mark)
  expect_equal(mgcvUI:::locale_paper_(), loc$paper)
})

# ---- Specific country conventions ----

test_that("US conventions are correct", {
  mgcvUI:::set_locale_("us")
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")
  expect_equal(mgcvUI:::locale_csv_dec_(), ".")
  expect_equal(mgcvUI:::locale_big_mark_(), ",")
  expect_equal(mgcvUI:::locale_dec_mark_(), ".")
  expect_equal(mgcvUI:::locale_paper_(), "letter")
})

test_that("Canada uses ymd date format", {
  mgcvUI:::set_locale_("ca")
  on.exit(withr_reset())
  loc <- mgcvUI:::get_locale_()
  expect_equal(loc$date_fmt, "ymd")
  expect_equal(loc$paper, "letter")
})

test_that("Finland uses space as thousands separator", {
  mgcvUI:::set_locale_("fi")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), " ")
  expect_equal(mgcvUI:::locale_csv_sep_(), ";")
  expect_equal(mgcvUI:::locale_csv_dec_(), ",")
})

test_that("Switzerland uses apostrophe as thousands separator", {
  mgcvUI:::set_locale_("ch")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), "'")
})

test_that("France uses space as thousands separator", {
  mgcvUI:::set_locale_("fr")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), " ")
  expect_equal(mgcvUI:::locale_csv_sep_(), ";")
})

test_that("Lithuania uses ISO date format (ymd)", {
  mgcvUI:::set_locale_("lt")
  on.exit(withr_reset())
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("Ukraine uses space thousands, comma decimal, dmy dates", {
  mgcvUI:::set_locale_("ua")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), " ")
  expect_equal(mgcvUI:::locale_csv_dec_(), ",")
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%y")
})

test_that("Russia uses space thousands, semicolon CSV", {
  mgcvUI:::set_locale_("ru")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), " ")
  expect_equal(mgcvUI:::locale_csv_sep_(), ";")
})

test_that("Turkey uses dot thousands, comma decimal", {
  mgcvUI:::set_locale_("tr")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), ".")
  expect_equal(mgcvUI:::locale_csv_dec_(), ",")
})

test_that("Japan uses comma thousands, dot decimal, ymd dates", {
  mgcvUI:::set_locale_("jp")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_big_mark_(), ",")
  expect_equal(mgcvUI:::locale_csv_dec_(), ".")
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("UK uses A4 paper but comma thousands like US", {
  mgcvUI:::set_locale_("gb")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_paper_(), "a4")
  expect_equal(mgcvUI:::locale_big_mark_(), ",")
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")
})

test_that("South Korea uses ymd date format", {
  mgcvUI:::set_locale_("kr")
  on.exit(withr_reset())
  loc <- mgcvUI:::get_locale_()
  expect_equal(loc$date_fmt, "ymd")
  expect_equal(loc$csv_sep, ",")
  expect_equal(loc$paper, "a4")
})

test_that("Sweden uses ymd date format", {
  mgcvUI:::set_locale_("se")
  on.exit(withr_reset())
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("Mexico uses letter paper", {
  mgcvUI:::set_locale_("mx")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_paper_(), "letter")
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")
})

test_that("Brazil uses semicolon CSV and A4", {
  mgcvUI:::set_locale_("br")
  on.exit(withr_reset())
  expect_equal(mgcvUI:::locale_csv_sep_(), ";")
  expect_equal(mgcvUI:::locale_csv_dec_(), ",")
  expect_equal(mgcvUI:::locale_paper_(), "a4")
})

# ---- Date format ordering ----

test_that("US locale tries MM/DD first", {
  mgcvUI:::set_locale_("us")
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%m/%d/%y")
})

test_that("German locale tries DD/MM first", {
  mgcvUI:::set_locale_("de")
  on.exit(withr_reset())
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%y")
})

test_that("Swedish locale tries YYYY-MM-DD first", {
  mgcvUI:::set_locale_("se")
  on.exit(withr_reset())
  fmts <- mgcvUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
})

test_that("all date format lists include ISO format", {
  for (code in c("us", "de", "fi", "jp", "ua")) {
    mgcvUI:::set_locale_(code)
    fmts <- mgcvUI:::locale_date_formats_()
    expect_true("%Y-%m-%d" %in% fmts, info = paste(code, "missing ISO format"))
  }
  withr_reset()
})

test_that("date format lists include datetime formats", {
  for (code in c("us", "de", "jp")) {
    mgcvUI:::set_locale_(code)
    fmts <- mgcvUI:::locale_date_formats_()
    expect_true(any(grepl("H:%M:%S", fmts)),
                info = paste(code, "missing datetime format"))
  }
  withr_reset()
})

test_that("date format lists have no duplicates", {
  for (code in c("us", "de", "fi", "jp", "se")) {
    mgcvUI:::set_locale_(code)
    fmts <- mgcvUI:::locale_date_formats_()
    expect_equal(length(fmts), length(unique(fmts)),
                 info = paste(code, "has duplicate date formats"))
  }
  withr_reset()
})

# ---- Paper size ----

test_that("US, Canada, and Mexico use letter paper", {
  for (code in c("us", "ca", "mx")) {
    mgcvUI:::set_locale_(code)
    expect_equal(mgcvUI:::locale_paper_(), "letter", info = code)
  }
  withr_reset()
})

test_that("European and Asian countries use A4 paper", {
  for (code in c("de", "fr", "fi", "gb", "ua", "ru", "tr", "jp", "kr", "br")) {
    mgcvUI:::set_locale_(code)
    expect_equal(mgcvUI:::locale_paper_(), "a4", info = code)
  }
  withr_reset()
})

# ---- CSV import with locale ----

test_that("import_data reads semicolon-separated CSV with comma decimal", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("price;area;name",
               "250000,50;120,5;Berlin",
               "300000,00;150,0;Munich"), tmp)
  df <- import_data(tmp, sep = ";", dec = ",")
  expect_equal(nrow(df), 2L)
  expect_equal(df$price[1], 250000.50)
  expect_equal(df$area[2], 150.0)
})

test_that("import_data with US defaults reads comma-separated CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(data.frame(price = c(100.5, 200.75)), tmp, row.names = FALSE)
  df <- import_data(tmp)
  expect_equal(df$price[1], 100.5)
})

test_that("import_data sep and dec default to US conventions", {
  fmls <- formals(import_data)
  expect_equal(fmls$sep, ",")
  expect_equal(fmls$dec, ".")
})

test_that("import_data with semicolon sep handles quoted fields", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("name;value",
               '"Smith, John";100,5',
               '"Doe, Jane";200,0'), tmp)
  df <- import_data(tmp, sep = ";", dec = ",")
  expect_equal(nrow(df), 2L)
  expect_equal(df$value[1], 100.5)
})

test_that("import_data still cleans names with locale params", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("First Name;Last Name;AGE",
               "Alice;Smith;30",
               "Bob;Jones;25"), tmp)
  df <- import_data(tmp, sep = ";")
  expect_true(all(grepl("^[a-z_]+$", names(df))))
})

# ---- Locale persistence (SQLite) ----

test_that("locale defaults can be saved and loaded from SQLite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  locale_settings <- list(
    locale_country = "fi",
    locale_paper = "a4",
    locale_csv_sep = ";",
    locale_dec = ",",
    locale_date = "dmy"
  )
  mgcvUI:::settings_db_write_locale_(locale_settings)

  saved <- mgcvUI:::settings_db_read_locale_()
  expect_false(is.null(saved))
  expect_equal(saved$locale_country, "fi")
  expect_equal(saved$locale_paper, "a4")
  expect_equal(saved$locale_csv_sep, ";")
  expect_equal(saved$locale_dec, ",")
  expect_equal(saved$locale_date, "dmy")
})

test_that("locale defaults overwrite previous values", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")

  mgcvUI:::settings_db_write_locale_(list(locale_country = "de"))
  mgcvUI:::settings_db_write_locale_(list(locale_country = "jp"))

  saved <- mgcvUI:::settings_db_read_locale_()
  expect_equal(saved$locale_country, "jp")
})

test_that("locale read returns NULL when no defaults saved", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  # Clear any existing locale defaults
  con <- mgcvUI:::settings_db_connect_()
  DBI::dbExecute(con, "DELETE FROM settings_v2 WHERE filename = '__locale_defaults__'")
  DBI::dbDisconnect(con)

  saved <- mgcvUI:::settings_db_read_locale_()
  expect_null(saved)
})

# ---- Multiple locale switches ----

test_that("switching locales multiple times works correctly", {
  mgcvUI:::set_locale_("us")
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")

  mgcvUI:::set_locale_("de")
  expect_equal(mgcvUI:::locale_csv_sep_(), ";")
  expect_equal(mgcvUI:::locale_csv_dec_(), ",")

  mgcvUI:::set_locale_("jp")
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")
  expect_equal(mgcvUI:::locale_csv_dec_(), ".")

  mgcvUI:::set_locale_("us")
  expect_equal(mgcvUI:::locale_csv_sep_(), ",")
  expect_equal(mgcvUI:::locale_paper_(), "letter")
})

test_that("override persists only for current set_locale_ call", {
  mgcvUI:::set_locale_("us", paper = "a4")
  expect_equal(mgcvUI:::locale_paper_(), "a4")

  # Switching to another country clears the override

  mgcvUI:::set_locale_("us")
  expect_equal(mgcvUI:::locale_paper_(), "letter")
})
