context("Prepare Data Set")

options(pkdata.tz='America/Chicago')

x <- read.csv("dose1.csv", stringsAsFactors = FALSE)
dld <- read.csv("druglevel.csv", stringsAsFactors = FALSE)

iv <- "id"
dv <- "date.dose"
idt <- "inf.time"
idv <- "inf.dose"
bdt <- "bol.time"
bdv <- "bol.dose"
odt <- "patch.time"
odv <- "patch.dose"
ov <- c("gender", "weight")
lv <- "id"
dlt <- "date.time"
dlv <- "fent.level"
lf <- 7

test_that("prepareDoses creates proper data set", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  expect_equal(names(y), c(iv, dv, sprintf("%s.real", idt), idt, idv, bdt, bdv, odt, odv, ov))
  expect_true(inherits(y[,sprintf("%s.real", idt)], "POSIXct"))
  expect_true(inherits(y[,idt], "POSIXct"))
  expect_true(inherits(y[,bdt], "POSIXct"))
  expect_true(inherits(y[,odt], "POSIXct"))
  expect_equal(nrow(y), 10)
})
