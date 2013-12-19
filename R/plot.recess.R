#'Plot an Object
#'
#'Plot an object of class "rise" from a groundwater rise analysis.
#'
#'The \code{before} and \code{after} arguments prepend and append those
#'number of days to the plot of the recession, which can be useful for 
#'assessing the recession. If \code{after} is set to a positive value,
#'then \code{before} is forced to a value greater than 0.
#'
#'@param x an object of class "recess."
#'@param which either "All" for all recessions, a any number of sequence
#'numbers (not Index number) of the recessions to plot, "season" for a graph
#'of recession rates over the calendar year, or "flow" for a graph of
#'recession rates by flow.
#'@param set.up set up the graphics page? Set to \code{FALSE} if the graphics page
#'has been set up with a call to \code{setPage}.
#'@param before begin plot \code{before} days prior to recession.
#'@param after end plot \code{after} days after the end of the recession.
#'See \bold{Details}.
#'@param \dots  not used, required for other methods.
#'@return The object \code{x} is returned invisibly
#'@section Side Effect: The object \code{x} is plotted
#'@seealso \code{\link{rise}}
#'@keywords hplot
#'@method plot recess
#'@S3method plot recess
plot.recess <- function(x, which="All", set.up=TRUE, before=0, after=before, 
												...) {
  ## Preprocessing:
  after <- max(0, after) # force nonnegative
  before <- max(sign(after), before)
  flows <- x$Data$Flow
  recess <- x$Data$Recess
  Sel <- x$Recessions
  Index <- Sel$Index
  Len <- Sel$Length
  ## Set up graphs
  if(set.up) 
  	setGD("RECESS")
  if(is.character(which)) {
  	which <- match.arg(which, c("All", "season", "flow"))
  	if(which == "flow") {
  		Ks <- Sel$K
  		logQ <- Sel$logMeanQ
  		xyPlot(logQ, Ks, yaxis.log=TRUE, xtitle="Common log of mean flow",
  					 ytitle="K")
  		return(invisible(x))
  	} else if(which == "season") {
  		Ks <- Sel$K
  		Dts <- Sel$Date
  		seasonPlot(Dts, Ks, yaxis.log=T, ytitle="K")
  		return(invisible(x))
  	} else
  		loop <- seq(nrow(x$Recessions))
  } else
  	loop <- which
  ## set axis ranges
  if(before > 0)
    trange <- c(-before + 1, max(Len[loop]) + after)
  else
    trange <- c(1, max(Len[loop]) + after)
  trange <- range(pretty(trange))
  pickem <- recess %in% Index[loop]
  xrange = range(flows[pickem])
  if(xrange[1L] == 0)
  	xrange[1L] <- 0.01
  xrange <- range(pretty(log10(xrange)))
  if(diff(xrange) < 0.999) { # Force to be at least 1.0
  	cntr <- round(mean(xrange), 1L)
  	xrange <- cntr + c(-.5, .5)
  }
  STAID <- attr(x, "STAID")
  ## Go for it
  for(i in loop) {
    flowSel <- recess == Index[i]
    ttoplot <- 1:Len[i] # default if bA == 0
    if(before > 0) {
      chkSel <- range(which(flowSel)) # need to guard against recess at ends
      flowSel <- c(flowSel[-(1L:before)], rep(FALSE, before)) |
        c(rep(FALSE, after), flowSel[1L:(length(flowSel) - after)])
      chkSel2 <- range(which(flowSel))
      ttoplot <- seq(chkSel2[1L] - chkSel[1L] + 1L, Len[i] + chkSel2[2L] - chkSel[2L], 1L)
    }
    xtoplot <- log10(flows[flowSel])
    start <- x$Recessions$Date[i]
    xyPlot(ttoplot, xtoplot, Plot=list(what="points", filled=FALSE),
      yaxis.range=xrange, xaxis.range=trange, ytitle="Flow (common log)",
      xtitle="Days in recession")
    ## plot beforeAfter in different symbol too
    if(before > 0) {
      ba <- which(!(ttoplot %in% 1:Len[i]))
      addXY(ttoplot[ba], xtoplot[ba], Plot=list(what="points", color="green"))
    }
    ## document plot
    addTitle(paste("Start date: ", start, " Index Number: ", Index[i], sep=""))
    if(!is.na(Sel$K[i])) {
      refLine(coefficients=c(Sel$Icept[i], -1/Sel$K[i]))
      used <- seq(from=Sel$First[i], to=Sel$Last[i]) + before
      addXY(ttoplot[used], xtoplot[used], Plot=list(what="points", filled=TRUE))
    }
  }
  invisible(x)
}
