#' PK Data
#'
#' This package will prepare data sets for PK data analysis.
#'
#' @name pkdata-package
#' @aliases pkdata-package pkdata
#' @docType package
#' @author Cole Beck, Leena Choi
#'
#' Maintainer: Cole Beck \email{cole.beck@@vumc.org}
#'
#' @keywords package
#' @import lubridate
NULL

#'Internal objects.
#'
#'Internal objects.
#'
#'This function should not be called by the user.
#'
#'@name pkdata-internal
#'@keywords internal
#'

.onLoad <- function(...) {
  # new option, pkdata time-zone
  tz <- Sys.timezone()
  if(is.na(tz)) tz <- ''
  options(pkdata.tz = tz)
}
