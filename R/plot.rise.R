#' Plot an Object
#'
#' Plot an object of class "rise" from a groundwater rise analysis.
#'
#'
#' @param x an object of class "rise."
#' @param which either "All" for the entire hydrogaph, "by year" for calendar year
#'hydrographs, or the calendar year to plot.
#' @param set.up set up the graphics page? Set to \code{FALSE} if the graphics page
#'has been set up with a call to \code{setPage}. 
#' @param plot.Hypo a character string designating the color to draw the hypothetical
#'recession curve. If "none" (the default value), then do not draw the recession.
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @seealso \code{\link{rise}}
#' @keywords hplot
#' @method plot rise
#' @export
plot.rise <- function(x, which = "All", set.up = TRUE, plot.Hypo="none", ...) {
  STAID <- attr(x, "STAID")
  if(set.up) 
    setGD("RISE")
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
  ## identify rises as line segments, logic forces to include first day of rise event
  rises <- ifelse(x$Rise > 0 | c(x$Rise[-1L] > 0, FALSE), x$GWLevel, NA)
  hypos <- ifelse(x$Rise > 0 | c(x$Rise[-1L] > 0, FALSE), x$HypoRecess, NA)
  indexes <- pmax(x$EventRise, shiftData(x$EventRise, -1, fill=0))
  for(i in loop) {
    AA.pl <- timePlot(x$Dates[Years == i], x$GWLevel[Years == i],
      ytitle="Groundwater Level")
    addXY(x$Dates[Years == i], rises[Years == i], Plot=list(what="lines", color="red", 
      width="color"), current=AA.pl)
    if(plot.Hypo != "none") {
      for(ie in indexes[Years == i]) {
        picks <- Years == i & indexes == ie
        addXY(x$Dates[picks], hypos[picks], Plot=list(what="lines", color=plot.Hypo, 
          width="color"), current=AA.pl)
      }
    }
    if(!is.null(STAID) && STAID != "Unknown")
      addTitle(Main=STAID)
  }
  invisible(x)
}
