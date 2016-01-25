# This needs work, not updated to smwrGraphs
#' Plot an Object
#'
#' Plot an object of class "rora" from a streamflow recession 
#'displacement analysis.
#'
#' @param x an object of class "rora"
#' @param which either "All" for the entire hydrogaph, "by year" for calendar year
#'hydrographs, or the calendar year to plot.
#' @param set.up set up the graphics page? Set to \code{FALSE} if the graphics page
#'has been set up with a call to \code{setPage}. 
#' @param colors a vector of length 3 specifying the colors for the peak and
#'critical time, the hypothetical streamflow recession, and the calulated
#'streamflow recession displacement, respectively.
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @seealso \code{\link{rora}}
#' @keywords hplot
#' @method plot rora
#' @export
plot.rora <- function(x, which = "All", set.up = TRUE, 
                      colors=c("none", "blue", "cyan"), ...) {
  if(x$ierr > 0) {
    cat("Fatal error in rora, nothing to plot\n")
    return(invisible(x))
  }
  if(set.up) 
    setGD("RORA")
  Years <- x$iyr
  YearP <- x$year
  if(is.character(which)) {
    which <- match.arg(which, c("All", "by year"))
    if(which == "All") { # create 1 group to plot
      Years <- as.integer(Years %in% unique(YearP))
      YearP <- rep(1L, length(YearP))
      loop <- 1L
    } else {
      loop <- unique(YearP)
    }
  } else {
    loop <- which
  }
  Start <- x$iyearst
  End <- x$iyearen
  iyear <- x$iyr
  ## select the  data
  Flow <- x$flow
  Dates <- as.Date(ISOdate(iyear, x$imon, x$idy))
  DDates <- as.double(Dates)
  iyear <- iyear
  ## extract peaks
  tp <- as.Date(ISOdate(x$year, x$mon, x$day))
  ta <- tp + x$ta
  tbc <- shiftData(ta, -1)
  tbc[length(tbc)] <- tp[length(tbc)] + 0.2144 * x$k
  te <- tp + x$te
  qe <- Flow[as.integer(Dates) %in% as.integer(te)]
  qa <- x$qa
  qb <- x$qb
  qc <- x$qc
  qp <- x$qp
  # Loop through the periods in which
  for(i in loop) {
    peaks <- which(YearP == i)
    sel <- Years == i
    if(length(peaks)) {
      ylim <- c(10^(min(log10(qb[peaks])-.2)), max(Flow[sel]))
    } else {
      ylim <- range(Flow[sel])
    }
    AA.pl <- timePlot(range(Dates[sel]), ylim, Plot=list(what="none"),
                      ytitle="Streamflow", xtitle="", yaxis.log=TRUE)
    addXY(DDates[sel], Flow[sel], current=AA.pl)
    for(j in peaks) {
      # The peak and critical time
      if(colors[1L] != "none") {
        addXY(c(tbc[j], tp[j], tp[j]), c(qb[j], qb[j], qp[j]),
              Plot=list(what="lines", color=colors[1L]), current=AA.pl)
      }
      # The recessions
      if(colors[2L] != "none") {
        # These are the recessions beginning at the end of TC for the previous peak recession
        addXY(c(ta[j], tbc[j]), c(qa[j], qb[j]),
              Plot=list(what="lines", color=colors[2L], size=0.05),
              current=AA.pl)
        # These are the recessions from the actual beginning of the peak
        addXY(c(te[j], tbc[j]), c(qe[j], qc[j]),
              Plot=list(what="lines", color=colors[2L], size=0.05),
              current=AA.pl)
      }
      # The streamflow displacements
      if(colors[3L] != "none") {
        addXY(c(tbc[j], tbc[j]), c(qb[j], qc[j]),
              Plot=list(what="overlaid", fill=FALSE, color=colors[3L], size=0.04),
              current=AA.pl)
      }
    } # end j loop
  } # end i loop
  invisible(x)
}
