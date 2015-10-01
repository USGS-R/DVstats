# This needs work--not converted fro S+ to R
#' Compute Recharge
#'
#' Computes recharge from an object of class "rora" from a streamflow recession 
#'displacement analysis.
#'
#' The argument \code{by} can be either a character indicating the period, or a list
#'created by \code{setSeasons}. If a character , thne must be "months," "years," 
#'"calendar years," "water years," or "climate years." May be abbreviated; and 
#'"years" is the same as "calendar years."
#'
#' @param x an object of class "rora."
#' @param by the time period to aggregate by. See \bold{Details}.
#' @param \dots  not used, required for other methods.
#' @return The recharge for each period specified in \code{by}. The units are
#'the same as for \code{x}.
#' @seealso \code{\link{rora}}, \code{\link{setSeasons}}
#' @keywords recharge
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(GlacialRidge)
#'G12.rise <- with(GlacialRidge, rise(G12, datetime, MPelev=1126.42, STAID="G12"))
#'# monthly summary of recharge in feet
#'aggregate(G12.rise)
#'}
#' @export
#' @method aggregate rora
aggregate.rora <- function(x, by="months", ...) {
  ## by must be "weeks", "months", "quarters", "years", or "water.year"
  ## unlike aggregate, partial match allowed
  if(x$ierr > 0) {
    cat("Fatal error in rora, nothing to aggregate\n")
    return(x)
  }
  timeDate <- timeSeq <- timeSreies <- aggregateSeries <- wdydy <- function(...) {
    stop("not converted to R yet")
  }
  Dates <- timeDate(julian=julian(x$mon, x$day, x$year))
  ## construct complete series
  CDates <- timeSeq(Dates[1], Dates[length(x$year)], by='days')
  Rech <- double(length(CDates))
  Num <- double(length(CDates))
  Rech[CDates %in% Dates] <- x$rech
  Num[CDates %in% Dates] <- 1
  data <- timeSeries(cbind(Rech, Num), positions.=CDates)
  Per <- c("weeks", "months", "quarters", "years", "water.year")
  by.match <- pmatch(by, Per, nomatch=0)
  if(by.match == 0)
    stop(paste("invalid by value: ", by, sep=""))
  by <- Per[by.match]
  if(by.match == 5) {
    WY <- range(wdydy(Dates)$year)
    WY <- seq(WY[1] - 1, WY[2] + 1)
    WY <- timeDate(julian=julian(10,1,WY))
    retval <- aggregateSeries(data, pos=WY, FUN=sum)
  } else {
    retval <- aggregateSeries(data, by=by, FUN=sum, week.align=0)
  }
  retval <- data.frame(retval@positions, retval@data[,1], retval@data[,2])
  names(retval) <- c("Period", "Recharge", "Npeaks")
  FMT <- c("Week of %2m/%2d/%4Y", "%b %Y", "Q%q %Y")
  if(by.match < 4) {
    retval$Period@format <- FMT[by.match]
    retval$Period <- as.character(retval$Period)
    retval$Period <- factor(retval$Period, levels=retval$Period) # maintain order
  } else if(by.match == 4)
    retval$Period <- years(retval$Period)
  else {
    retval$Period <- waterYear(retval$Period)
    levels(retval$Period) <- paste("WY", levels(retval$Period)) # make it clear
  }
  return(retval)
}
