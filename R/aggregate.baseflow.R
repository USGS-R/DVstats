#' Baseflow Summary
#'
#' Computes baseflow statistics for user-specified periods of time.
#'
#' The aregument \code{by} can be either a character indicating the period, or a list
#'created by \code{setSeasons}. If a character , then must be "months," "years," 
#'"calendar years," "water years," "climate years," or "total."
#'May be abbreviated; and "years" is the same as "calendar years."
#'
#' @param x an object of class "baseflow."
#' @param by the time period to aggregate by. See \bold{Details}.
#' @param index compute the baseflow index (proportion of baseflow
#'to total flow) rather than baseflow?
#' @param \dots  not used, required for other methods.
#' @return The baseflow for each period specified in \code{by}. The units are
#'the same as for \code{x}.
#' @seealso \code{\link{part}}, \code{\link{hysep}}, \code{\link{bfi}}, 
#'\code{\link{setSeasons}}
#' @keywords baseflow
#' @examples
#'\dontrun{
#'library(USGSwsData)
#'data(GlacialRidge)
#'G12.hysep <- with(ChoptankFlow, hysep(Flow, datetime, da=113,
#'  STAID="01491000"))
#'# monthly summary of recharge in feet
#'aggregate(G12.hysep)
#'}
#' @S3method aggregate baseflow
#' @method aggregate baseflow
aggregate.baseflow <- function(x, by="months", index=FALSE, ...) {
  ## Coding history:
  ##    2013Sep04 DLLorenz Original based on aggregate.rise
  if(is.character(by)) {
    by <- match.arg(by, c("months", "years", "water years", "climate years", 
                          "calendar years", "total"))
    INDEX <- switch(by,
      "total"=rep("Total", nrow(x)),
      "months"=paste(months(x$Dates), year(x$Dates), sep=" "),
      "years"=year(x$Dates),
      "water years"=waterYear(x$Dates),
      "climate years"=climateYear(x$Dates),
      "calendar yeears"=year(x$Dates))
  }
  else { # must be season object
    tmp <- by # create a temporary list with the same names as the seasons
    for(i in names(by)) {
      tmp.ndx <- seasonYear(x$Dates, by[[i]][1L], by[[i]][2L], numeric=TRUE)
      tmp.ndx[!is.na(tmp.ndx)] <- paste(i, tmp.ndx[!is.na(tmp.ndx)], sep=" ")
      tmp[[i]] <- tmp.ndx
    }
    INDEX <- coalesce(do.call(cbind, tmp))
  }
  ## Now ready to aggregate the data
  INDEX <- factor(INDEX, levels=unique(INDEX)) # keep order
  Bflow <- tapply(x$BaseQ, INDEX, FUN=sum)
  Ndays <- tapply(x$BaseQ, INDEX, FUN=length)
  if(index) {
    Bflow <- Bflow/tapply(x$Flow, INDEX, FUN=sum)
    retval <- data.frame(Period=names(Bflow), BaseIndx=Bflow, Ndays=Ndays, 
                         row.names=NULL, stringsAsFactors=FALSE)
  } else
    retval <- data.frame(Period=names(Bflow), BaseQ=Bflow, Ndays=Ndays, 
                         row.names=NULL, stringsAsFactors=FALSE)
  return(retval)
}
