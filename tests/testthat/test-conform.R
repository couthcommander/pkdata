context("Conform Data Set")

x <- read.csv("dose1.csv", stringsAsFactors = FALSE)

iv <- "id"
dv <- "date.dose"
idt <- "inf.time"
idv <- "inf.dose"
bdt <- "bol.time"
bdv <- "bol.dose"
odt <- "patch.time"
odv <- "patch.dose"
ov <- c("gender", "weight")

test_that("conformDoses creates proper columns", {
  y <- conformDoses(x, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov
  )
  expect_equal(names(y), c(iv, dv, sprintf("%s.real", idt), idt, idv, bdt, bdv, odt, odv, ov))
  y <- conformDoses(x, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv
  )
  expect_equal(names(y), c(iv, dv, sprintf("%s.real", idt), idt, idv))
  y <- conformDoses(x, idVar = iv, dateVar = dv,
    otherDoseTimeVar = odt, otherDoseVar = odv, otherVars = "gender"
  )
  expect_equal(names(y), c(iv, dv, odt, odv, "gender"))
})

test_that("conformDoses rounds dates to nearest hour", {
  y <- conformDoses(x, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv
  )
  rounded <- parse_dates(c("2014-03-07 24:00","2014-03-08 22:00",
    "2014-03-09 03:00","2014-03-09 08:00"
  ))
  expect_equal(y[,idt], rounded)
})

test_that("conformDoses sorts by date", {
  y <- conformDoses(x, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov
  )
  expect_equal(seq(nrow(y)), do.call(order, y[,c(iv,sprintf("%s.real", idt),bdt,odt)]))
})

test_that("conformDoses removes blank rows", {
  x1 <- rbind(x, c(1,"2014-03-10",NA,NA,NA,NA,NA,NA,"M",NA))
  y <- conformDoses(x1, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv
  )
  expect_equal(nrow(y), 4)
})

test_that("conformDoses fails on bad columns", {
  expect_error(conformDoses(x, idVar = iv, dateVar = dv))
  expect_error(conformDoses(x, idVar = "badid", dateVar = dv, infusionDoseTimeVar = idt, infusionDoseVar = idv))
  expect_error(conformDoses(x, idVar = "badid", dateVar = dv, infusionDoseTimeVar = idt, infusionDoseVar = idv, otherVars = "extraneous"))
})
