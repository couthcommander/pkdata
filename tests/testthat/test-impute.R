context("Impute Data Set")

options(pkdata.tz='America/Chicago')

x <- read.csv("dose1.csv", stringsAsFactors = FALSE)

iv <- "id"
dv <- "date.dose"
idt <- "inf.time"
idv <- "inf.dose"

y <- conformDoses(x, idVar = iv, dateVar = dv,
  infusionDoseTimeVar = idt, infusionDoseVar = idv
)

test_that("imputeDoses imputes missing hours up to maxskips", {
  z <- imputeDoses(y, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv, maxskips = 3
  )
  expect_equal(z[,idv], c(500,0,rep(100,4),100,0,100,250,250,rep(200,3),rep(250,4)))
  z <- imputeDoses(y, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv, maxskips = 5
  )
  expect_equal(z[,idv], c(500,0,rep(100,10),250,250,rep(200,3),rep(250,4)))
  z <- imputeDoses(y, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv, maxskips = 0
  )
  expect_equal(z[,idv], c(500,0,100,0,100,0,100,250,0,200,200,0,250,0,250))
})

test_that("imputeDoses creates datetime objects", {
  z <- imputeDoses(y, idVar = iv, dateVar = dv,
    infusionDoseTimeVar = idt, infusionDoseVar = idv, maxskips = 3
  )
  expect_true(inherits(z[,idt], "POSIXct"))
})

test_that("imputeDoses fails on bad columns", {
  expect_error(imputeDoses(y))
  expect_error(imputeDoses(y, dateVar = "oops",
    infusionDoseTimeVar = idt, infusionDoseVar = idv
  ))
  expect_error(imputeDoses(y,
    infusionDoseTimeVar = dv, infusionDoseVar = idv
  ))
})
