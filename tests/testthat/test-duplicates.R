context("Fix Dose Duplicates")

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

test_that("fixDuplicates rounds infusion doses", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  y <- y[c(1:2,2:6),]
  d <- fixDuplicates(y, infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv
  )
  z <- sortDoses(d, idVar = iv, infusionDoseTimeVar = idt,
     bolusDoseTimeVar = bdt, otherDoseTimeVar = odt
  )
  expect_true(as.character(z[2,idt]) == "2014-03-08 21:00:00")
  expect_true(as.character(z[3,idt]) == "2014-03-08 22:00:00")
})

test_that("fixDuplicates rounds infusion doses around DST", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  y <- y[c(1:3,3:6),]
  d <- fixDuplicates(y, infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv
  )
  z <- sortDoses(d, idVar = iv, infusionDoseTimeVar = idt,
     bolusDoseTimeVar = bdt, otherDoseTimeVar = odt
  )
  expect_true(as.character(z[3,idt]) == "2014-03-09 01:00:00")
  expect_true(as.character(z[4,idt]) == "2014-03-09 03:00:00")
})

test_that("fixDuplicates moves infusion to bolus", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  y <- y[c(1:2,2:6),]
  y[2:3, 'inf.time.real'] <- "2014-03-08 22:00:00"
  d <- fixDuplicates(y, infusionDoseTimeVar = idt, infusionDoseVar = idv,
    moveBolus = TRUE, bolusDoseTimeVar = bdt, bolusDoseVar = bdv
  )
  z <- sortDoses(d, idVar = iv, infusionDoseTimeVar = idt,
     bolusDoseTimeVar = bdt, otherDoseTimeVar = odt
  )
  expect_true(as.character(z[5,bdt]) == "2014-03-08 22:00:00" && z[5,'tobolus'] == 1)
})

test_that("fixDuplicates adds bolus doses", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  y <- y[c(1:5,5:6),]
  d <- fixDuplicates(y, infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv
  )
  z <- sortDoses(d, idVar = iv, infusionDoseTimeVar = idt,
     bolusDoseTimeVar = bdt, otherDoseTimeVar = odt
  )
  expect_true(as.character(z[5,bdv]) == y[5,bdv]*2)
})

test_that("fixDuplicates plays well with imputeDoses", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  y <- y[c(1:2,2:10),]
  y[2:3, 'inf.time.real'] <- "2014-03-08 22:00:00"
  z <- imputeDoses(y, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv, maxskips = 3
  )
  d <- fixDuplicates(z, infusionDoseTimeVar = idt, infusionDoseVar = idv,
    moveBolus = TRUE, bolusDoseTimeVar = bdt, bolusDoseVar = bdv
  )
  zz <- sortDoses(d, idVar = iv, infusionDoseTimeVar = idt,
     bolusDoseTimeVar = bdt, otherDoseTimeVar = odt
  )
  expect_true(as.character(zz[10,bdt]) == "2014-03-08 22:00:00" && zz[10,'tobolus'] == 1)
})
