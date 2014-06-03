#' Sort Dose Data Set
#'
#' Helper function to sort dose data set by ID and time variables.
#'
#' Sort order will be the ID variable, then infusion time, bolus time, or other
#' dose date-time variables.
#'
#' @aliases sortDoses
#' @param data data.frame; data set with dose information
#' @param idVar character string; name of ID variable, defaults to id
#' @param dateVar character string; name of date variable, defaults to date.dose
#' @param infusionDoseTimeVar character string; name of infusion dose time
#' variable
#' @param bolusDoseTimeVar character string; name of bolus dose time variable
#' @param otherDoseTimeVar character string; name of additional dose time
#' variable
#' @return data.frame, containing dose data
#'
#' @export
#' @rdname sortDoses
#' @author Cole Beck

sortDoses <- function(data, idVar="id", infusionDoseTimeVar=NULL, bolusDoseTimeVar=NULL, otherDoseTimeVar=NULL) {
    colnames <- names(data)
    if(is.null(idVar)) stop("idVar argument must be present")
    idcol <- match(idVar, colnames)
    if(is.na(idcol)) stop(sprintf("column %s does not exist", idVar))
    icol <- match(infusionDoseTimeVar, colnames)
    bcol <- match(bolusDoseTimeVar, colnames)
    ocol <- match(otherDoseTimeVar, colnames)
    dc <- data.frame(id=data[,idcol])
    if(length(icol) && !is.na(icol)) dc$a <- parse_dates(data[,icol])
    if(length(bcol) && !is.na(bcol)) dc$b <- parse_dates(data[,bcol])
    if(length(ocol) && !is.na(ocol)) dc$c <- parse_dates(data[,ocol])
    # do.call/order will return indeces of sorted (by all columns) data frame
    data <- data[do.call(order, dc),]
    rownames(data) <- NULL
    data
}
