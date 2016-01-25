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
#' @return A data frame reporting the recharge and number of peaks for each period 
#'specified in \code{by}. The units of recharge are inches.
#' @seealso \code{\link{rora}}, \code{\link{setSeasons}}
#' @keywords recharge
#' @examples
#' # See the vignette
#' @export
#' @method aggregate rora
aggregate.rora <- function(x, by="months", ...) {
  ## by must be "weeks", "months", "quarters", "years", or "water.year"
  ## unlike aggregate, partial match allowed
  if(x$ierr > 0) {
    stop("Fatal error in rora, nothing to aggregate")
  }
  Dates <- as.Date(ISOdate(x$year, x$mon, x$day))
  if(is.character(by)) {
    by <- match.arg(by, c("months", "years", "water years", "climate years", "calendar years"))
    INDEX <- switch(by,
                    "months"=paste(months(Dates), year(Dates), sep=" "),
                    "years"=year(Dates),
                    "water years"=waterYear(Dates),
                    "climate years"=climateYear(Dates),
                    "calendar yeears"=year(Dates))
  }
  else { # must be season object
    tmp <- by # create a temporary list with the same names as the seasons
    for(i in names(by)) {
      tmp.ndx <- seasonYear(Dates, by[[i]][1L], by[[i]][2L], numeric=TRUE)
      tmp.ndx[!is.na(tmp.ndx)] <- paste(i, tmp.ndx[!is.na(tmp.ndx)], sep=" ")
      tmp[[i]] <- tmp.ndx
    }
    INDEX <- coalesce(do.call(cbind, tmp))
  }
  ## Now ready to aggregate the data
  INDEX <- factor(INDEX, levels=unique(INDEX)) # keep order
  rech <- tapply(x$rech, INDEX, sum)
  npk <- tapply(x$rech, INDEX, length)
  retval <- data.frame(Period=names(rech), Recharge=rech, NumPeaks=npk,
                       row.names=as.character(seq(along=rech)))
  return(retval)
}
