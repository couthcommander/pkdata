#'PK Data
#'
#'This package will prepare data sets for PK data analysis.
#'
#'@name pkdata-package
#'@aliases pkdata-package pkdata
#'@docType package
#'@author Cole Beck, Leena Choi
#'
#'Maintainer: Cole Beck \email{cole.beck@@vanderbilt.edu}
#'
#'@references Lu B, Greevy R, Xu X, Beck C. Optimal Nonbipartite Matching and
#'its Statistical Applications. The American Statistician. Vol. 65, no. 1. :
#'21-30. 2011.
#'
#'@keywords package
#'@importFrom lubridate parse_date_time
#'@examples
#'\dontrun{
#'options(pkdata.tz='America/Chicago')
#'dose.file <- read.csv('dosage.csv', stringsAsFactors = FALSE)
#'drug.level.file <- read.csv('levels.csv', stringsAsFactors = FALSE)
#'prepped <- prepareDoses(dose.file, drug.level.file)
#'}
#'
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
