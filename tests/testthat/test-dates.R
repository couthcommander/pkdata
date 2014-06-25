context("Create Dates")

options(pkdata.tz='America/Chicago')

test_that("parse_dates creates dates from vector", {
  x <- c("2014-01-15", "20140202")
  y <- as.Date(c("2014-01-15", "2014-02-02"))
  expect_equal(parse_dates(x), y)
  x <- c("03272014", "04/05/2014")
  y <- as.Date(c("2014-03-27", "2014-04-05"))
  expect_equal(parse_dates(x), y)
  x <- c("05-22-14", "061214")
  y <- as.Date(c("2014-05-22", "2014-06-12"))
  expect_equal(parse_dates(x), y)
})

test_that("parse_dates creates date-times from vector", {
  x <- c("2014-01-15 01:51", "20140202 04:35:18")
  y <- as.POSIXct(c("2014-01-15 01:51:00", "2014-02-02 04:35:18"))
  expect_equal(parse_dates(x), y)
  x <- c("03272014 13:52:44", "04/05/2014 21:21")
  y <- as.POSIXct(c("2014-03-27 13:52:44", "2014-04-05 21:21:00"))
  expect_equal(parse_dates(x), y)
})

test_that("parse_dates is consistent on DST border", {
  x <- c("2014-03-09 01:00", "2014-03-09 03:00", "2014-11-02 00:59",
  "2014-11-02 01:00", "2014-11-02 01:59", "2014-11-02 02:00")
  y <- dst(parse_dates(x))
  expect_equal(y, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("parse_dates fails on bad dates", {
  expect_true(is.na(parse_dates("2007-03-12 NA")))
  expect_error(parse_dates("2014-02-29"))
  expect_error(parse_dates("07-22-14 6"))
  expect_error(parse_dates("2014-03-09 02:15:00"))
  expect_warning(parse_dates("08/18/14 26:05"))
})

test_that("round_halfhours works around DST", {
  x <- parse_dates(c("2014-03-09 00:00:00", "2014-03-09 00:29:59",
      "2014-03-09 00:30:00", "2014-03-09 00:59:59", "2014-03-09 01:35:00",
      "2014-03-09 03:15:00", "2014-11-02 00:30:00", "2014-11-02 00:59:59",
      "2014-11-02 01:35:00", "2014-11-02 02:15:00", "2014-11-02 02:45:00"
  ))
  y <- as.POSIXct(c("2014-03-09 00:00:00", "2014-03-09 00:00:00",
      "2014-03-09 01:00:00", "2014-03-09 01:00:00", "2014-03-09 03:00:00",
      "2014-03-09 03:00:00", "2014-11-02 01:00:00", "2014-11-02 01:00:00",
      "2014-11-02 02:00:00", "2014-11-02 02:00:00", "2014-11-02 03:00:00"))
  expect_equal(round_hours(x), y)
})
