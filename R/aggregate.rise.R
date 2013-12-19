#' Compute Recharge
#'
#' Computes recharge from an object of class "rise" from a groundwater rise analysis.
#'
#' The argument \code{by} can be either a character indicating the period, or a list
#'created by \code{setSeasons}. If a character , thne must be "months," "years," 
#'"calendar years," "water years," or "climate years." May be abbreviated; and 
#'"years" is the same as "calendar years."
#'
#' @param x an object of class "rise."
#' @param by the time period to aggregate by. See \bold{Details}.
#' @param SY the specific yield requied to give results in recharge rather than rise. 
#' @param \dots  not used, required for other methods.
#' @return The recharge for each period specified in \code{by}. The units are
#'the same as for \code{x}.
#' @seealso \code{\link{rise}}, \code{\link{setSeasons}}
#' @keywords recharge
#' @examples
#'\dontrun{
#'library(USGSwsData)
#'data(GlacialRidge)
#'G12.rise <- with(GlacialRidge, rise(G12, datetime, MPelev=1126.42, STAID="G12"))
#'# monthly summary of recharge in feet
#'aggregate(G12.rise)
#'}
#' @S3method aggregate rise
#' @method aggregate rise
aggregate.rise <- function(x, by="months", SY=.25, ...) {
  ## Coding history:
  ##    2013Apr21 DLLorenz Complete rewrite of S+ code
  if(is.character(by)) {
    by <- match.arg(by, c("months", "years", "water years", "climate years", "calendar years"))
    INDEX <- switch(by,
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
  Recharge <- tapply(x$Rise, INDEX, FUN=sum) * SY
  Ndays <- tapply(x$Rise, INDEX, FUN=length)
  retval <- data.frame(Period=names(Recharge), Recharge=Recharge, Ndays=Ndays, row.names=NULL,
    stringsAsFactors=FALSE)
  return(retval)
}
