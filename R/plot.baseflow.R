#' Plot an Object
#'
#' Plot an object of class "baseflow" from any baseflow analysis.
#'
#'
#' @param x an object of class "baseflow."
#' @param which either "All" for the entire hydrogaph, "by year" for calendar year
#'hydrographs, or the calendar year to plot.
#' @param set.up set up the graphics page? Set to \code{FALSE} if the graphics page
#'has been set up with a call to \code{setPage}. 
#' @param \dots any additional arguments to \code{timePlot}, other than
#'\code{x} and \code{y}.
#' @return The object \code{x} is returned invisibly
#' @section Side Effect: The object \code{x} is plotted
#' @seealso \code{\link{bfi}}, \code{\link{hysep}}, \code{\link{part}}, 
#' @keywords hplot
#' @method plot baseflow
#' @importFrom grDevices dev.off
#' @importFrom graphics par plot plot.new text
#' @importFrom utils capture.output menu
#' @export
plot.baseflow <- function(x, which = "All", set.up = TRUE, ...) {
  STAID <- attr(x, "STAID")
  if(set.up) 
    setGD("BASEFLOW")
  Years <- year(x$Dates)
  if(is.character(which)) {
    which <- match.arg(which, c("All", "by year"))
    if(which == "All") { # create 1 group to plot
      Years <- rep(1, nrow(x))
      loop <- 1
    }
    else
      loop <- unique(Years)
  }
  else
    loop <- which
  ## Check if ytitle was included
  NmDots <- names(list(...))
  IncYtit <- TRUE
  if(!is.null(NmDots) && ("ytitle" %in% NmDots))
    IncYtit <- FALSE
  for(i in loop) {
    if(IncYtit) {
      AA.pl <- timePlot(x$Dates[Years == i], x$Flow[Years == i],
                        ytitle="Streamflow", ...)
    } else
      AA.pl <- timePlot(x$Dates[Years == i], x$Flow[Years == i],...)
    addXY(x$Dates[Years == i], x$BaseQ[Years == i], Plot=list(what="lines", color="blue", 
       width="color"), current=AA.pl)
    TTL <- paste("Baseflow separation by ", attr(x, "type"), sep="")
    if(!is.null(STAID) && STAID != "Unknown")
      TTL <- paste(TTL, " for station ", STAID, sep="")
    addTitle(Main=TTL)
  }
  invisible(x)
}
