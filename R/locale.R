# Internal locale environment for number/date formatting
mgcv_locale_env_ <- new.env(parent = emptyenv())
mgcv_locale_env_$csv_sep   <- ","
mgcv_locale_env_$csv_dec   <- "."
mgcv_locale_env_$big_mark  <- ","
mgcv_locale_env_$dec_mark  <- "."
mgcv_locale_env_$date_fmt  <- "mdy"
mgcv_locale_env_$paper     <- "letter"

# Country presets: csv_sep, csv_dec, big_mark, dec_mark, date_fmt, paper
#' @noRd
locale_country_presets_ <- function() {
  list(
    us = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "mdy", paper = "letter"),
    ca = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "ymd", paper = "letter"),
    gb = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    ie = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    au = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    nz = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    de = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    at = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ch = list(csv_sep = ";", csv_dec = ",", big_mark = "'",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    fr = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    be = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    nl = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    it = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    es = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    pt = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    tr = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    fi = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    se = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "ymd", paper = "a4"),
    no = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    dk = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    pl = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    cz = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    lt = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "ymd", paper = "a4"),
    lv = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ee = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ua = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ru = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    jp = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "ymd", paper = "a4"),
    kr = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "ymd", paper = "a4"),
    mx = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "letter"),
    br = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4")
  )
}

# Country display names (for UI dropdown)
#' @noRd
locale_country_choices_ <- function() {
  rest <- c("Australia"       = "au", "Austria"        = "at",
            "Belgium"         = "be", "Brazil"         = "br",
            "Canada"          = "ca", "Czech Republic"  = "cz",
            "Denmark"         = "dk", "Estonia"        = "ee",
            "Finland"         = "fi", "France"         = "fr",
            "Germany"         = "de", "Ireland"        = "ie",
            "Italy"           = "it", "Japan"          = "jp",
            "Latvia"          = "lv", "Lithuania"      = "lt",
            "Mexico"          = "mx", "Netherlands"    = "nl",
            "New Zealand"     = "nz", "Norway"         = "no",
            "Poland"          = "pl", "Portugal"       = "pt",
            "Russia"          = "ru", "South Korea"    = "kr",
            "Spain"           = "es", "Sweden"         = "se",
            "Switzerland"     = "ch", "Turkey"         = "tr",
            "Ukraine"         = "ua", "United Kingdom"  = "gb")
  c("United States" = "us", rest)
}

#' @noRd
set_locale_ <- function(country = "us", csv_sep = NULL, csv_dec = NULL,
                        big_mark = NULL, dec_mark = NULL,
                        date_fmt = NULL, paper = NULL) {
  presets <- locale_country_presets_()
  preset <- presets[[country]] %||% presets[["us"]]

  mgcv_locale_env_$csv_sep  <- csv_sep  %||% preset$csv_sep
  mgcv_locale_env_$csv_dec  <- csv_dec  %||% preset$csv_dec
  mgcv_locale_env_$big_mark <- big_mark %||% preset$big_mark
  mgcv_locale_env_$dec_mark <- dec_mark %||% preset$dec_mark
  mgcv_locale_env_$date_fmt <- date_fmt %||% preset$date_fmt
  mgcv_locale_env_$paper    <- paper    %||% preset$paper
}

#' @noRd
get_locale_ <- function() {
  list(
    csv_sep  = mgcv_locale_env_$csv_sep,
    csv_dec  = mgcv_locale_env_$csv_dec,
    big_mark = mgcv_locale_env_$big_mark,
    dec_mark = mgcv_locale_env_$dec_mark,
    date_fmt = mgcv_locale_env_$date_fmt,
    paper    = mgcv_locale_env_$paper
  )
}

#' @noRd
locale_csv_sep_ <- function() mgcv_locale_env_$csv_sep

#' @noRd
locale_csv_dec_ <- function() mgcv_locale_env_$csv_dec

#' @noRd
locale_big_mark_ <- function() mgcv_locale_env_$big_mark

#' @noRd
locale_dec_mark_ <- function() mgcv_locale_env_$dec_mark

#' @noRd
locale_paper_ <- function() mgcv_locale_env_$paper

#' @noRd
locale_date_formats_ <- function() {
  fmt <- mgcv_locale_env_$date_fmt %||% "mdy"
  preferred <- switch(fmt,
    mdy = c("%m/%d/%Y", "%m-%d-%Y", "%m/%d/%Y %H:%M:%S"),
    dmy = c("%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y", "%d/%m/%Y %H:%M:%S"),
    ymd = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S"),
    c("%m/%d/%Y", "%d/%m/%Y")
  )
  all_fmts <- c("%Y-%m-%d", "%Y/%m/%d",
                "%m/%d/%Y", "%m-%d-%Y",
                "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
                "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S",
                "%d/%m/%Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
                "%b %d, %Y", "%B %d, %Y")
  unique(c(preferred, all_fmts))
}
