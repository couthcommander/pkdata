context("Trim Data Set")

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
last <- 10

y <- conformDoses(x, idVar = iv, dateVar = dv,
  infusionDoseTimeVar = idt, infusionDoseVar = idv,
  bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
  otherDoseTimeVar = odt, otherDoseVar = odv,
  otherVars = ov
)

test_that("trimDoses creates proper columns", {
  z <- trimDoses(y, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    lookForward = lf, last = NA
  )
  expect_equal(names(y), names(z))
})

test_that("trimDoses removes invalid dates using druglevel data", {
  z <- trimDoses(y, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv
  )
  expect_equal(nrow(z), 6)
  z <- trimDoses(y, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    lookForward = 0
  )
  expect_equal(nrow(z), 6)
})

test_that("trimDoses removes invalid dates using last", {
  z <- trimDoses(y, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    lookForward = lf, last = 1
  )
  expect_equal(nrow(z), 2)
  z <- trimDoses(y, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    lookForward = 0, last = 3
  )
  expect_equal(nrow(z), 4)
})

test_that("trimDoses fails on bad columns", {
  expect_error(trimDoses(y, idVar = dld))
  expect_error(trimDoses(y, dld, drugLevelID = lv, drugLevelTimeVar = dlt,
    drugLevelVar = dlv
  ))
  expect_error(trimDoses(y, dld, drugLevelID = lv, drugLevelTimeVar = dlt,
    drugLevelVar = dlv, bolusDoseTimeVar = "unreal.bolus"
  ))
})
