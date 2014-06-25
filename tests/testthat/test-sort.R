context("Sort Data Set")

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

test_that("sortDoses sorts data set", {
  y <- prepareDoses(x, dld, drugLevelID = lv,
    drugLevelTimeVar = dlt, drugLevelVar = dlv,
    idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv,
    bolusDoseTimeVar = bdt, bolusDoseVar = bdv,
    otherDoseTimeVar = odt, otherDoseVar = odv,
    otherVars = ov, lookForward = lf
  )
  y[,iv] <- 1
  y1 <- y2 <- y[c(1:4,7:10,5,6),]
  y3 <- y[c(5,1:4,6,7:10),]
  y4 <- y[c(6,1:4,5,7:10),]
  rownames(y1) <- NULL
  rownames(y2) <- NULL
  rownames(y3) <- NULL
  rownames(y4) <- NULL
  z1 <- sortDoses(y, idVar = iv, infusionDoseTimeVar = idt,
     bolusDoseTimeVar = bdt, otherDoseTimeVar = odt
  )
  z2 <- sortDoses(y, idVar = iv, infusionDoseTimeVar = idt)
  z3 <- sortDoses(y, idVar = iv, bolusDoseTimeVar = bdt)
  z4 <- sortDoses(y, idVar = iv, otherDoseTimeVar = odt)
  expect_equal(y1, z1)
  expect_equal(y2, z2)
  expect_equal(y3, z3)
  expect_equal(y4, z4)
})
