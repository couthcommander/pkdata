#' Determine Format of Date and Date-Time Variables
#'
#' Given a vector of dates or date-times, determine the format if one is used
#' consistently.  If inconsistentencies are found, the function will fail.
#' See \code{\link{strptime}} for examples of format specifications.
#'
#' \code{guessDateFormat} is rigid when determining the date format.  For
#' date-times it expects the date and time parts to be separated by a space.  It
#' does not recognize all format specifications, such as the AM/PM indicator.  
#' The time part should have at least one colon to separate hours from minutes.
#' The date part may have any separator as non-numerical values are removed
#' before parsing.
#'
#' @aliases guessDateFormat
#' @param x character vector of dates or date-times
#' @return Returns a character string representing the format of the date-time
#' variables.
#'
#' @export
#' @rdname guessDateFormat
#' @author Cole Beck
#' @examples
#' x <- c("2014-01-15", "20140202")
#' guessDateFormat(x)

guessDateFormat <- function(x) {
    x1 <- x
    # set to character string
    if(!inherits(x1[1], "character")) {
      x1 <- as.character(x1)
    }
    # replace blanks with NA and remove
    x1[x1 == ""] <- NA
    x1 <- x1[!is.na(x1)]
    if(length(x1) == 0) return(NA)
    dateTimes <- do.call(rbind, strsplit(x1, ' '))
    for(i in ncol(dateTimes)) {
        dateTimes[dateTimes[,i] == "NA"] <- NA
    }
    # assume the time part can be found with a colon
    timePart <- which(apply(dateTimes, MARGIN=2, FUN=function(i) { any(grepl(":", i)) }))
    # everything not in the timePart should be in the datePart
    datePart <- setdiff(seq(ncol(dateTimes)), timePart)
    # should have 0 or 1 timeParts and exactly one dateParts
    if(length(timePart) > 1 || length(datePart) != 1) stop("cannot parse your time variable")
    timeFormat <- NA
    if(length(timePart)) {
        # find maximum number of colons in the timePart column
        hasNoTm <- is.na(dateTimes[,timePart])
        ncolons <- max(nchar(gsub("[^:]", "", dateTimes[!hasNoTm,timePart])))
        if(ncolons == 1) {
            timeFormat <- "%H:%M"
        } else if(ncolons == 2) {
            timeFormat <- "%H:%M:%S"
        } else stop("timePart should have 1 or 2 colons")
    }
    hasNoDt <- is.na(dateTimes[,datePart])
    nonMissDt <- dateTimes[!hasNoDt,datePart]
    # sep is any non-numeric value found, hopefully / or -
    # if there isn't a separator, it will be ""
    DtMinusDate <- substr(gsub("[0-9]", "", nonMissDt), 1, 1)
    sep <- unique(DtMinusDate)
    if(length(sep) > 1 && "" %in% sep) {
        sep <- sep[sep != '']
    }
    if(length(sep) > 1) stop("too many separators in datePart")
    dates <- gsub("[^0-9]", "", nonMissDt)
    # maximum number of characters found in the date part
    dlen <- max(nchar(dates))
    # does every date include the separator?
    hasSep <- sep != '' & DtMinusDate == sep
    if(all(hasSep)) {
      dateVals <- nonMissDt
      useSep <- sep
    } else {
      dateVals <- dates
      useSep <- ''
    }
    # create date format options
    dateFormat <- NA
    dtfrm1 <- paste("%y", "%m", "%d", sep = useSep)
    dtfrm2 <- paste("%m", "%d", "%y", sep = useSep)
    dtfrm3 <- paste("%Y", "%m", "%d", sep = useSep)
    dtfrm4 <- paste("%m", "%d", "%Y", sep = useSep)
    # when six, expect the century to be omitted
    if(dlen %in% 4:6) {
        if(sum(is.na(lubridate::fast_strptime(dateVals, format=dtfrm1, lt = FALSE))) == 0) {
          dateFormat <- dtfrm1
        } else if(sum(is.na(lubridate::fast_strptime(dateVals, format=dtfrm2, lt = FALSE))) == 0) {
          dateFormat <- dtfrm2
        } else stop("datePart format [four-six characters] is inconsistent")
    } else if(dlen %in% 7:8) {
        if(sum(is.na(lubridate::fast_strptime(dateVals, format=dtfrm3, lt = FALSE))) == 0) {
          dateFormat <- dtfrm3
        } else if(sum(is.na(lubridate::fast_strptime(dateVals, format=dtfrm4, lt = FALSE))) == 0) {
          dateFormat <- dtfrm4
        } else stop("datePart format [seven-eight characters] is inconsistent")
    } else {
        stop(sprintf("datePart has unusual length: %s", dlen))
    }
    # add separator back if needed
    if(sep != '' && useSep == '') {
      dateFormat <- paste(substr(dateFormat, 1, 2), substr(dateFormat, 3, 4), substr(dateFormat, 5, 6), sep = sep)
    }
    if(is.na(timeFormat)) {
        format <- dateFormat
    } else if(timePart == 1) {
        format <- paste(timeFormat, dateFormat)
    } else if(timePart == 2) {
        format <- paste(dateFormat, timeFormat)
    } else stop("cannot parse your time variable")
    format
}

#' Parse Date and Date-Time Variables
#'
#' Given a vector of dates or date-times, create Date or POSIXct variables.
#'
#' \code{parse_dates} calls \code{\link[lubridate]{parse_date_time}} from the
#' lubridate package.  While \code{\link[lubridate]{parse_date_time}} accepts
#' multiple date formats, \code{parse_dates} requires a consistent format.
#'
#' @aliases parse_dates
#' @param x character vector of dates or date-times
#' @param tz character string; specifies the time zone to be used for the
#' conversion.  Defaults to the current time zone.
#' @return vector of Date or POSIXct objects
#'
#' @export
#' @rdname parse_dates
#' @author Cole Beck
#' @examples
#' x <- c("2014-01-15", "20140202")
#' parse_dates(x)
#' x <- c("2014-01-15 01:51", "20140202 04:35:18")
#' parse_dates(x)

parse_dates <- function(x, tz = getOption('pkdata.tz', '')) {
  if(inherits(x, "POSIXct")) return(x)
  res <- rep(NA, length(x))
  # look for NA, replace if found
  x[grepl("NA", x)] <- NA
  fmt <- guessDateFormat(x)
  if(is.na(fmt)) return(res)
  fmt <- gsub("[^ymd ]", "", tolower(fmt))
  # space indicates time part
  if(grepl(" ", fmt)) {
    fmt <- sub(" .*$", "_HMS", fmt)
    # if time part is missing, set to NA
    x[!grepl(" ", sub('[[:space:]]$', '', x))] <- NA
    res <- lubridate::parse_date_time(x, orders = fmt, tz = tz, truncated = 2)
  } else {
    res <- as.Date(lubridate::parse_date_time(x, orders = fmt, tz = tz))
  }
  res
}

round_hours <- function(x) {
  toAdd <- ifelse(lubridate::minute(x) >= 30, lubridate::dhours(1), lubridate::dhours(0))
  lubridate::floor_date(x, unit = 'hour') + toAdd
}
