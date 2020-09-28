#' Impute Dose Data
#'
#' Use last-observation-carried-forward to impute missing dose values by hour.
#'
#' Creates skips column
#'
#' @aliases imputeDoses
#' @param data data.frame; data set with dose information
#' @param idVar character string; name of ID variable, defaults to id
#' @param dateVar character string; name of date variable, defaults to date.dose
#' @param infusionDoseTimeVar character string; name of infusion dose time
#' variable
#' @param infusionDoseVar character string; name of infusion dose variable
#' @param maxskips integer; maximum number of missed doses that may be imputed
#' @return data.frame, containing dose data
#'
#' @export
#' @rdname imputeDoses
#' @author Cole Beck

imputeDoses <- function(data, idVar="id", dateVar="date.dose", infusionDoseTimeVar=NULL, infusionDoseVar=NULL, maxskips=3) {
    if(is.null(idVar)) stop("idVar is required")
    if(is.null(dateVar)) stop("dateVar is required")
    if(is.null(infusionDoseTimeVar)) stop("infusionDoseTimeVar is required")
    if(is.null(infusionDoseVar)) stop("infusionDoseVar is required")
    colnames <- names(data)
    idcol <- match(idVar, colnames)
    datecol <- match(dateVar, colnames)
    idtv <- match(infusionDoseTimeVar, colnames)
    idv <- match(infusionDoseVar, colnames)
    if(is.na(idcol)) stop(sprintf("column %s does not exist", idVar))
    if(is.na(datecol)) stop(sprintf("column %s does not exist", dateVar))
    if(is.na(idtv)) stop(sprintf("column %s does not exist", infusionDoseTimeVar))
    if(is.na(idv)) stop(sprintf("column %s does not exist", infusionDoseVar))
    rtcol <- match(sprintf("%s.real", infusionDoseTimeVar), colnames)
    if(is.na(rtcol)) stop("real time column is not present")
    if(!is.numeric(maxskips) || maxskips < 0) maxskips <- 0

    # change datetime strings to datetime objects
    data[,idtv] <- parse_dates(data[,idtv])
    data[,rtcol] <- parse_dates(data[,rtcol])
    # set infusion dose to NA if time is missing
    data[is.na(data[,idtv]) & !is.na(data[,idv]), idv] <- NA
    # determine format for datetime variables
    #dateformatA <- guessDateFormat(data[,rtcol])
    dateformatB <- guessDateFormat(data[,datecol])
    #dateformatC <- guessDateFormat(data[,idtv])
    # determine total number of skips we need to impute
    # split by ID, count occurrences of time difference (in hours) greater than 1
    missingness <- sapply(split(data[,idtv], f=data[,idcol]), FUN=function(i) {
        missed <- as.numeric(diff(i[!is.na(i)]), units="hours")-1
        sum(ifelse(missed >= 0 & missed <= maxskips, missed, ifelse(missed < 0, 0, 1)))
    })
    missingness <- missingness[!(is.na(missingness) | missingness == 0)]
    ids <- as.character(names(missingness))
    nskips <- sum(missingness)
    index <- nrow(data)+1
    # create new data frame with room to add nskips rows
    # init new rows as the first row, and reset values
    imp <- data[c(seq(nrow(data)),rep(NA,nskips)),]
    ix <- seq(index,nrow(imp))
    # initialize tobolus/change if available
    if('tobolus' %in% names(imp)) imp[ix, 'tobolus'] <- 0
    if('change' %in% names(imp)) imp[ix, 'change'] <- 0
    imp$skips <- 0
    imp$imputed <- 0
    imp[ix, 'imputed'] <- 1
    for(i in seq(length(ids))) {
        # subset data with non-missing dose values by each ID
        info <- subset(data, subset=data[,idcol] == ids[i] & !is.na(data[,idv]))
        infuse.date <- info[,idtv]
        # get time interval between all points
        interval <- as.numeric(diff(infuse.date), units="hours")-1
        if(maxskips > 0) {
            # indeces where time interval is between 1 and MAXSKIPS
            skipped <- which(interval >= 1 & interval <= maxskips) + 1
            for(j in skipped) {
                # locf previous dose value (j-1) until current value (j)
                val <- info[j-1,idv]
                time <- infuse.date[j-1] + lubridate::dhours(1)
                while(time < infuse.date[j]) {
                    # don't impute when dose value is ZERO on both sides (j-1, j)
                    if(val != 0 || info[j,idv] != 0) {
                        imp[index,idcol] <- info[j-1,idcol]
                        imp[index, rtcol] <- time
                        imp[index, datecol] <- format(time, dateformatB)
                        imp[index, idtv] <- time
                        imp[index,idv] <- val
                        imp$skips[index] <- 1
                    } else {
                        # NA skips will be thrown out
                        imp$skips[index] <- NA
                    }
                    time <- time + lubridate::dhours(1)
                    index <- index + 1
                }
            }
        }
        # indeces of time gaps larger than MAXSKIPS
        bigskip <- which(interval > maxskips)+1
        if(length(bigskip) > 0) {
            skipsize <- interval[interval > maxskips]
            # don't actually impute rows where skip is greater than MAXSKIPS hours
            for(j in seq_along(bigskip)) {
                # insert a ZERO val in first missing record
                time <- infuse.date[bigskip[j]-1]
                time <- time + lubridate::dhours(1)
                imp[index,idcol] <- info[bigskip[j]-1,idcol]
                imp[index,rtcol] <- time
                imp[index,datecol] <- format(time, dateformatB)
                imp[index,idtv] <- time
                imp[index,idv] <- 0
                imp$skips[index] <- 0
                # find the previous record and set skipsize
                imp$skips[rownames(imp) == rownames(info)[bigskip[j]-1]] <- skipsize[j]
                index <- index + 1
            }
        }
    }
    # remove invalid rows
    if(any(is.na(imp$skips))) imp <- imp[!is.na(imp$skips),]
    # this reformats datetime columns as character strings
    #imp[,rtcol] <- format(imp[,rtcol], dateformatA)
    #imp[,idtv] <- format(imp[,idtv], dateformatC)
    sortDoses(imp, idVar, dateVar, infusionDoseTimeVar)
}
