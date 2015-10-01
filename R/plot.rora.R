# This needs work, not updated to smwrGraphs
#' Plot an Object
#'
#' Plot an object of class "rora" from a streamflow recession 
#'displacement analysis.
#'
#' @param x an object of class "rora"
#' @param period character string indicating the grouping of the graphs.
#'Must be a valid argument to the \code{by} arguemnt of \code{seq.Date}.
#' @param col colors. 
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @seealso \code{\link{rora}}
#' @keywords hplot
#' @method plot rora
#' @export
plot.rora <- function(x, period="quarter", col=c(2,4,5), ...) {
  if(x$ierr > 0) {
    cat("Fatal error in rora, nothing to plot\n")
    return(invisible(x))
  }
  Start <- x$iyearst
  End <- x$iyearen
  iyear <- x$iyr
  ## select the streamflow data
  Flow <- x$flow[iyear >= Start & iyear <= End]
  Dates <- as.Date(ISOdate(iyear, x$imon, x$idy))[iyear >= Start & iyear <= End]
  DDates <- as.double(Dates)
  iyear <- iyear[iyear >= Start & iyear <= End]
  ## extract peaks
  tp <- as.Date(ISOdate(x$year, x$mon, x$day))
  ta <- tp + x$ta
  tbc <- shiftData(ta, -1)
  tbc[length(tbc)] <- tp[length(tbc)] + 0.2144 * x$k
  te <- tp + x$te
  qe <- log10(Flow[as.integer(Dates) %in% as.integer(te)])
  qa <- log10(x$qa)
  qb <- log10(x$qb)
  qc <- log10(x$qc)
  qp <- log10(x$qp)
  ## make periods
  Pers <- as.double(seq(as.Date(ISOdate(Start, 1, 1)),
                            as.Date(ISOdate(End+1, 1, 1)), by=period))
  Percut <- as.integer(cut(as.double(Dates), Pers-.5, include.lowest=T))
  Perloop <- unique(Percut)
  for(i in Perloop) {
    peaks <- which(tp >= Pers[i] & tp < Pers[i+1])
    if(length(peaks)) {
      Sel <- DDates >= Pers[i] & DDates <= tbc[max(peaks)] + 1 # account for fraction
      ylim <- c(10^(min(qb[peaks]-.2)), max(Flow[Sel]))
      Dlim <- c(Pers[i], tbc[max(peaks)] + 1)
    } else {
      Sel <- DDates >= Pers[i] & DDates < Pers[i+1]
      ylim <- range(Flow[Sel])
      Dlim <- c(Pers[i], Pers[i+1])
    }
    plot(as.Date(Dlim, origin="1970-01-01"), ylim, log='y', ylab="streamflow",
         xlab="", type='n')
    lines(DDates[Sel], log10(Flow[Sel]))
    for(j in peaks) {
      lines(c(tbc[j], tp[j], tp[j]), c(qb[j], qb[j], qp[j]), col=col[1])
      points(ta[j], qa[j])
      points(tbc[j], qb[j])
      lines(c(ta[j], tbc[j]), c(qa[j], qb[j]), col=col[2])
      points(tbc[j], qc[j])
      lines(c(te[j], tbc[j]), c(qe[j], qc[j]), col=col[2])
      lines(c(tbc[j], tbc[j]), c(qb[j], qc[j]), col=col[3])
    } # end j loop
  } # end i loop
  invisible(x)
}
