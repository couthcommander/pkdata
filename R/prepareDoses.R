#' Create a Conforming Dose Data Set
#'
#' Modify dose data such that it conforms for future use.
#'
#' Date-time variables are given a consistent format.  Invalid records are
#' removed.  The data set is sorted by date.
#'
#' @aliases conformDoses
#' @param doseData data.frame; data set with dose information
#' @param idVar character string; name of ID variable, defaults to id
#' @param dateVar character string; name of date variable, defaults to date.dose
#' @param infusionDoseTimeVar character string; name of infusion dose time
#' variable
#' @param infusionDoseVar character string; name of infusion dose variable
#' @param bolusDoseTimeVar character string; name of bolus dose time variable
#' @param bolusDoseVar character string; name of bolus dose variable
#' @param otherDoseTimeVar character string; name of additional dose time
#' variable
#' @param otherDoseVar character string; name of additional dose variable
#' @param otherVars character string; name of other variables within data set
#' @return data.frame, containing dose data
#'
#' @export
#' @rdname conformDoses
#' @author Cole Beck

conformDoses <- function(doseData, idVar="id", dateVar="date.dose",
                  infusionDoseTimeVar=NULL, infusionDoseVar=NULL,
                  bolusDoseTimeVar=NULL, bolusDoseVar=NULL,
                  otherDoseTimeVar=NULL, otherDoseVar=NULL, otherVars=NULL) {
    colnames <- names(doseData)
    useInfusion <- useBolus <- useOther <- TRUE
    # validate column information
    idc <- match(idVar, colnames)
    dc <- match(dateVar, colnames)
    itc <- match(infusionDoseTimeVar, colnames)
    ivc <- match(infusionDoseVar, colnames)
    btc <- match(bolusDoseTimeVar, colnames)
    bvc <- match(bolusDoseVar, colnames)
    otc <- match(otherDoseTimeVar, colnames)
    ovc <- match(otherDoseVar, colnames)
    if(is.na(idc)) stop(sprintf("column %s does not exist", idVar))
    if(is.na(dc)) stop(sprintf("column %s does not exist", dateVar))
    if(is.na(itc) && !is.null(infusionDoseTimeVar)) stop(sprintf("column %s does not exist", infusionDoseTimeVar))
    if(is.na(ivc) && !is.null(infusionDoseVar)) stop(sprintf("column %s does not exist", infusionDoseVar))
    if(is.na(btc) && !is.null(bolusDoseTimeVar)) stop(sprintf("column %s does not exist", bolusDoseTimeVar))
    if(is.na(bvc) && !is.null(bolusDoseVar)) stop(sprintf("column %s does not exist", bolusDoseVar))
    if(is.na(otc) && !is.null(otherDoseTimeVar)) stop(sprintf("column %s does not exist", otherDoseTimeVar))
    if(is.na(ovc) && !is.null(otherDoseVar)) stop(sprintf("column %s does not exist", otherDoseVar))
    if(!length(itc) || !length(ivc) || is.na(itc) || is.na(ivc)) useInfusion <- FALSE
    if(!length(btc) || !length(bvc) || is.na(btc) || is.na(bvc)) useBolus <- FALSE
    if(!length(otc) || !length(ovc) || is.na(otc) || is.na(ovc)) useOther <- FALSE
    if(!useInfusion && !useBolus && !useOther) stop("you must have at least one dose and dose time variable")
    extras <- match(otherVars, colnames)
    addn <- length(extras)
    if(addn && any(is.na(extras))) stop(sprintf("column %s does not exist", otherVars[is.na(extras)]))
    # build new dataset, starting with ID and date
    info <- doseData[,c(idc, dc)]
    # sort by ID first
    sortorder <- c(1)
    if(useInfusion) {
      infuse.time <- parse_dates(paste(doseData[,dc], doseData[,itc]))
      # sort by infuse time
      sortorder <- append(sortorder, ncol(info)+1)
      # add infusion data (realtime/time/dose) to info
      info[,sprintf("%s.real", infusionDoseTimeVar)] <- infuse.time
      info[,infusionDoseTimeVar] <- round_hours(infuse.time)
      info[,infusionDoseVar] <- doseData[,ivc]
    }
    if(useBolus) {
      bolus.time <- parse_dates(paste(doseData[,dc], doseData[,btc]))
      # sort by bolus time
      sortorder <- append(sortorder, ncol(info)+1)
      # add bolus data (time/dose) to info
      info[,bolusDoseTimeVar] <- bolus.time
      info[,bolusDoseVar] <- doseData[,bvc]
    }
    if(useOther) {
      other.time <- parse_dates(paste(doseData[,dc], doseData[,otc]))
      # sort by other time
      sortorder <- append(sortorder, ncol(info)+1)
      # add other data (time/dose) to info
      info[,otherDoseTimeVar] <- other.time
      info[,otherDoseVar] <- doseData[,ovc]
    }
    if(addn) {
      # add additional "other" columns
      info[,otherVars] <- doseData[,extras]
    }
    # remove empty records
    info <- info[rowSums(!is.na(info[,seq(3, ncol(info)-addn)])) > 0, ]
    # reorder
    info <- info[do.call(order, info[,sortorder]),]
    # reset row names
    rownames(info) <- NULL
    info
}

#' Trim Dose Data Set
#'
#' Remove invalid doses by creating a time frame window constructed from drug
#' level data.
#'
#' The time frame window is generally seven days before drug level data up
#' through the last drug level data record.  The window can be adjusted by
#' setting the lookForward and last arguments.
#'
#' @aliases trimDoses
#' @param doseData data.frame; data set with dose information
#' @param drugLevelData data.frame; data set with drug level data
#' @param drugLevelID character string; name of ID variable, defaults to id
#' @param drugLevelTimeVar character string; name of date-time variable,
#' defaults to date.time
#' @param drugLevelVar character string; name of drug level variable, defaults
#' to fent.level
#' @param infusionDoseTimeVar character string; name of infusion dose time
#' variable
#' @param infusionDoseVar character string; name of infusion dose variable
#' @param bolusDoseTimeVar character string; name of bolus dose time variable
#' @param bolusDoseVar character string; name of bolus dose variable
#' @param otherDoseTimeVar character string; name of additional dose time
#' variable
#' @param otherDoseVar character string; name of additional dose variable
#' @param lookForward integer; initializes the time frame window with the number
#' of days prior to the first drug level data; defaults to 7
#' @param last integer; sets the end of the time frame window to be "last" days
#' after the first dose date, rather than the date of the last drug level data
#' @return data.frame, containing trimmed dose data
#'
#' @export
#' @rdname trimDoses
#' @author Cole Beck

trimDoses <- function(doseData, drugLevelData,
                    drugLevelID="id", drugLevelTimeVar="date.time", drugLevelVar="fent.level",
                    infusionDoseTimeVar=NULL, infusionDoseVar=NULL, bolusDoseTimeVar=NULL, bolusDoseVar=NULL,
                    otherDoseTimeVar=NULL, otherDoseVar=NULL, lookForward=7, last=NA) {
    colnames1 <- names(doseData)
    colnames2 <- names(drugLevelData)
    useInfusion <- useBolus <- useOther <- TRUE
    # validate column information for dosage dataset
    itc <- match(infusionDoseTimeVar, colnames1)
    ivc <- match(infusionDoseVar, colnames1)
    btc <- match(bolusDoseTimeVar, colnames1)
    bvc <- match(bolusDoseVar, colnames1)
    otc <- match(otherDoseTimeVar, colnames1)
    ovc <- match(otherDoseVar, colnames1)
    if(is.na(itc) && !is.null(infusionDoseTimeVar)) stop(sprintf("column %s does not exist", infusionDoseTimeVar))
    if(is.na(ivc) && !is.null(infusionDoseVar)) stop(sprintf("column %s does not exist", infusionDoseVar))
    if(is.na(btc) && !is.null(bolusDoseTimeVar)) stop(sprintf("column %s does not exist", bolusDoseTimeVar))
    if(is.na(bvc) && !is.null(bolusDoseVar)) stop(sprintf("column %s does not exist", bolusDoseVar))
    if(is.na(otc) && !is.null(otherDoseTimeVar)) stop(sprintf("column %s does not exist", otherDoseTimeVar))
    if(is.na(ovc) && !is.null(otherDoseVar)) stop(sprintf("column %s does not exist", otherDoseVar))
    if(!length(itc) || !length(ivc) || is.na(itc) || is.na(ivc)) useInfusion <- FALSE
    if(!length(btc) || !length(bvc) || is.na(btc) || is.na(bvc)) useBolus <- FALSE
    if(!length(otc) || !length(ovc) || is.na(otc) || is.na(ovc)) useOther <- FALSE
    if(!useInfusion && !useBolus && !useOther) stop("you must have at least one dose and dose time variable")
    if(useInfusion) {
        rtc <- match(sprintf("%s.real", infusionDoseTimeVar), colnames1)
        if(is.na(rtc)) stop(sprintf("column %s.real does not exist", infusionDoseTimeVar))
    }
    if(!(lookForward %in% seq(100))) lookForward <- 7
    # validate column information for drug level dataset
    dlidc <- match(drugLevelID, colnames2)
    dltc <- match(drugLevelTimeVar, colnames2)
    dlvc <- match(drugLevelVar, colnames2)
    if(!length(dlidc) || is.na(dlidc)) stop(sprintf("column %s does not exist", drugLevelID))
    if(!length(dltc) || is.na(dltc)) stop(sprintf("column %s does not exist", drugLevelTimeVar))
    if(!length(dlvc) || is.na(dlvc)) stop(sprintf("column %s does not exist", drugLevelVar))
    plasma.dates <- data.frame(ids=unique(doseData[,1]), start=NA, end=NA)
    dformat <- guessDateFormat(drugLevelData[,dltc])
    # make list of first and last plasma dates for each individual
    for(i in seq(nrow(plasma.dates))) {
        # subset drug level data on ID and non-missing dose values
        drug.info <- subset(drugLevelData, 
            subset=drugLevelData[,dlidc] == plasma.dates[i,1] & !is.na(drugLevelData[,dltc]) & !is.na(drugLevelData[,dlvc]),
            select=c(drugLevelID, drugLevelTimeVar, drugLevelVar))
        if(nrow(drug.info) > 0) {
#             drug.info$datetime <- as.POSIXlt(drug.info[,drugLevelTimeVar], format=dformat, tz=TZONE)
            drug.info$datetime <- parse_dates(drug.info[,drugLevelTimeVar])
            # first drug level datetime
            pd1 <- drug.info[head(order(drug.info$datetime), 1), "datetime"]
            plasma.dates[i,2] <- format(pd1, format=dformat)
            # last drug level datetime
            pd2 <- drug.info[tail(order(drug.info$datetime), 1), "datetime"]
            if(!is.na(last)) {
              pd1 <- pd1 + ddays(last + 1)
              second(pd1) <- 0
              minute(pd1) <- 0
              hour(pd1) <- 0
#               pd1$mday <- pd1$mday + last + 1
#               pd1$sec <- 0
#               pd1$min <- 0
#               pd1$hour <- 0
              pd2 <- pd1
            }
            plasma.dates[i,3] <- format(pd2, format=dformat)
        }
    }
    plasma.dates$start <- parse_dates(plasma.dates$start)
    plasma.dates$end <- parse_dates(plasma.dates$end)
    doseData$valid <- TRUE
    lastid <- NA
    lastdose <- NA
    # mark records invalid if they occur too early or too late
    for(i in seq(nrow(doseData))) {
        ix <- match(doseData[i,1], plasma.dates$ids)
        endpoint <- plasma.dates[ix,3]
        if(is.na(endpoint)) {
            doseData[i, "valid"] <- FALSE
        } else {
            startpoint <- plasma.dates[ix,2]
            # subtract lookForward (usually 1 week) from startpoint
            startpoint <- startpoint - ddays(lookForward)
            # check doses for infusion times
            if(useInfusion && !is.na(doseData[i,rtc])) {
                # infuse record invalid b/c before startpoint or after endpoint
                if(doseData[i,rtc] < startpoint || doseData[i,rtc] > endpoint) {
                    doseData[i, "valid"] <- FALSE
                }
            # check bolus times
            } else if(useBolus && !is.na(doseData[i,btc]) && (doseData[i,btc] < startpoint || doseData[i,btc] > endpoint)) {
                doseData[i, "valid"] <- FALSE
            # check other times
            } else if(useOther && !is.na(doseData[i,otc]) && (doseData[i,otc] < startpoint || doseData[i,otc] > endpoint)) {
                doseData[i, "valid"] <- FALSE
            }
        }
    }
    validcolnum <- match("valid", names(doseData))
    doseData <- subset(doseData, subset=doseData$valid)[,-validcolnum]
    rownames(doseData) <- NULL
    doseData
}

#' Prepare the Dose Data Set
#'
#' Create a dose data set with conforming data, and remove invalid records.
#'
#' Wrapper function for \code{\link{conformDoses}} and \code{\link{trimDoses}}.
#'
#' @aliases prepareDoses
#' @param doseData data.frame; data set with dose information
#' @param drugLevelData data.frame; data set with drug level data
#' @param drugLevelID character string; name of ID variable, defaults to id
#' @param drugLevelTimeVar character string; name of date-time variable,
#' defaults to date.time
#' @param drugLevelVar character string; name of drug level variable, defaults
#' to fent.level
#' @param idVar character string; name of ID variable, defaults to id
#' @param dateVar character string; name of date variable, defaults to date.dose
#' @param infusionDoseTimeVar character string; name of infusion dose time
#' variable
#' @param infusionDoseVar character string; name of infusion dose variable
#' @param bolusDoseTimeVar character string; name of bolus dose time variable
#' @param bolusDoseVar character string; name of bolus dose variable
#' @param otherDoseTimeVar character string; name of additional dose time
#' variable
#' @param otherDoseVar character string; name of additional dose variable
#' @param otherVars character string; name of other variables within data set
#' @param lookForward integer; initializes the time frame window with the number
#' of days prior to the first drug level data; defaults to 7
#' @return data.frame, containing dose data
#'
#' @export
#' @rdname prepareDoses
#' @author Cole Beck

prepareDoses <- function(doseData, drugLevelData,
                    drugLevelID="id", drugLevelTimeVar="date.time", drugLevelVar="fent.level",
                    idVar="id", dateVar="date.dose",
                    infusionDoseTimeVar=NULL, infusionDoseVar=NULL, bolusDoseTimeVar=NULL, bolusDoseVar=NULL,
                    otherDoseTimeVar=NULL, otherDoseVar=NULL, otherVars=NULL, lookForward=7) {
    # set-up dataset to conform to expected formats
    d1 <- conformDoses(doseData, idVar, dateVar, infusionDoseTimeVar, infusionDoseVar,
        bolusDoseTimeVar, bolusDoseVar, otherDoseTimeVar, otherDoseVar, otherVars)
    # remove values that won't be included in analysis
    d2 <- trimDoses(d1, drugLevelData, drugLevelID, drugLevelTimeVar, drugLevelVar,
        infusionDoseTimeVar, infusionDoseVar, bolusDoseTimeVar, bolusDoseVar, otherDoseTimeVar, otherDoseVar, lookForward)
    return(d2)
}
