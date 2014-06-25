#' Remove Duplicate Dose Data
#'
#' Modify dose data to remove duplicate dose values.
#'
#' Some duplicates can be adjusted by changing the date-time rounding.
#' Others may be converted from one dose type to another, such as moving
#' infusion to bolus.
#'
#' @aliases fixDuplicates
#' @param data data.frame; data set with dose information
#' @param idVar character string; name of ID variable, defaults to id
#' @param infusionDoseTimeVar character string; name of infusion dose time
#' variable
#' @param infusionDoseVar character string; name of infusion dose variable
#' @param moveBolus logical; allow duplicate infusion doses to be categorized
#' as bolus
#' @param bolusDoseTimeVar character string; name of bolus dose time variable
#' @param bolusDoseVar character string; name of bolus dose variable
#' @return data.frame, containing dose data
#'
#' @export
#' @rdname fixDuplicates
#' @author Cole Beck

fixDuplicates <- function(data, idVar="id", infusionDoseTimeVar=NULL, infusionDoseVar=NULL, moveBolus=FALSE, bolusDoseTimeVar=NULL, bolusDoseVar=NULL) {
    colnames <- names(data)
    if(!is.null(idVar) && !(idVar %in% colnames)) stop(sprintf("column %s does not exist", idVar))
    if(!is.null(infusionDoseTimeVar) && !(infusionDoseTimeVar %in% colnames)) stop(sprintf("column %s does not exist", infusionDoseTimeVar))
    if(!is.null(infusionDoseVar) && !(infusionDoseVar %in% colnames))  stop(sprintf("column %s does not exist", infusionDoseVar))
    idcol <- match(idVar, colnames)
    idtv <- match(infusionDoseTimeVar, colnames)
    idv <- match(infusionDoseVar, colnames)
    rtcol <- match(sprintf("%s.real", infusionDoseTimeVar), colnames)
    if(is.na(rtcol)) stop("real time column is not present")
    bdtv <- which(bolusDoseTimeVar == colnames)
    bdv <- which(bolusDoseVar == colnames)
    # if bolus columns aren't present, don't use moveBolus
    if(moveBolus && (!length(bdtv) || !length(bdv))) moveBolus <- FALSE

    # set infusion dose to NA if time is missing
    data[is.na(data[,idtv]) & !is.na(data[,idv]), idv] <- NA
    # save time variables independently from dataframe
    real.time <- parse_dates(data[,rtcol])
    # save infusion datetime format for later
    inf.format <- guessDateFormat(data[,idtv])
    infuse.time <- parse_dates(data[,idtv])
    bolus.time <- NULL
    if(length(bdtv)) bolus.time <- parse_dates(data[,bdtv])
    data$tobolus <- 0
    data$change <- 0
    # is data set ordered by ID?
    if(!all.equal(seq(nrow(data)), order(data[,idcol]))) stop(sprintf("data set should be ordered by %s", idVar))
    # find rows duplicated by ID and infusion dose time
    # duprows <- which(unlist(tapply(data[,idtv], data[,idcol], duplicated)))
    duprows <- which(unlist(tapply(data[,idtv], data[,idcol], FUN=function(i) c(FALSE, diff(i)==0))))
    # unless infuse.time is NA
    duprows <- duprows[!is.na(data[duprows, idtv])]
    for(i in duprows) {
        # pair is the duplicated record before i (though not always i-1)
        pair <- i-1
        # look until row with same ID and infusion time
        # using as.character on datetime b/c time format is uncouth (ie CST != CDT)
        while(is.na(data[pair,idtv]) || as.character(data[i,idtv]) != as.character(data[pair,idtv])) {
            pair <- pair-1
        }
        solved <- FALSE
        # if pair is rounded, consider rounding down
        if(minute(real.time[pair]) != 0) {
            index <- pair-1
            # index should be previous row with valid infusion time
            while(index > 1 && is.na(data[index,idtv]) && data[index,idcol] == data[pair,idcol]) {
                index <- index-1
            }
            tmp <- infuse.time[pair] - dhours(1)
            # if first record (for given ID) or there's a skip, round down
            if(index == 0 || data[index,idcol] != data[pair,idcol] || tmp > infuse.time[index]) {
                data[pair, idtv] <- format(tmp, inf.format)
                solved <- TRUE
            }
        }
        # if i is rounded, consider rounding up
        if(!solved && minute(real.time[i]) != 0) {
            index <- i+1
            # index should be next row with valid infusion time
            while(is.na(data[index,idtv]) && data[index,idcol] == data[i,idcol]) {
                index <- index+1
            }
            tmp <- infuse.time[i] + dhours(1)
            # if last record (for given ID) or there's a skip, round up
            if(data[index,idcol] != data[i,idcol] || infuse.time[index] > tmp) {
                data[i, idtv] <- format(tmp, inf.format)
                solved <- TRUE
            }
        }
        # try moving duplicate to bolus (if using a bolus variable)
        if(!solved && !is.null(bolus.time)) {
            # don't set an even time to bolus, unless it's pair has already been set
            if(minute(real.time[pair]) != 0 && data$tobolus[pair] == 0) {
                j <- pair
            } else {
                j <- i
            }
            # ensure bolus data is missing
            if(all(is.na(data[j,c(bdtv,bdv)]))) {
                if(moveBolus) {
                    data[j,bdtv] <- data[j,rtcol]
                    data[j,bdv] <- data[j,idv]
                    # remove infusion dose data as it is now bolus
                    data[j,rtcol] <- NA
                    data[j,idtv] <- NA
                    data[j,idv] <- NA
                }
                data$tobolus[j] <- 1
            }
        }
    }
    if(!is.null(bolus.time)) {
        # find rows duplicated by ID and bolus dose time
        # duprows <- which(unlist(tapply(data[,bdtv], data[,idcol], duplicated)))
        duprows <- which(unlist(tapply(data[,bdtv], data[,idcol], FUN=function(i) c(FALSE, diff(i)==0))))
        # unless bolus.time is NA
        duprows <- duprows[!is.na(data[duprows, bdtv])]
        # set duplicate bolus data to NA, but add dose value to first instance
        if(length(duprows)) {
          vals <- apply(data[, c(idcol, bdtv)], MARGIN=1, paste, collapse='|')
          for(i in duprows) {
            init.ix <- match(vals[i], vals)
            data[init.ix, bdv] <- data[init.ix, bdv] + data[i, bdv]
          }
          data[duprows, c(bdtv, bdv)] <- c(NA, NA)
          data[duprows, 'tobolus'] <- NA
        }
    }
    # remove records with tobolus == NA
    if(any(is.na(data$tobolus))) data <- data[!is.na(data$tobolus),]
    data
}
