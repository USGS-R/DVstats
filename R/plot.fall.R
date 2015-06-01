#' Plot an Object
#'
#' Plot an object of class "rise" from a groundwater recession analysis.
#'
#' @details The \code{before} and \code{after} arguments prepend and append those
#'number of days to the plot of the recession, which can be useful for 
#'assessing the recession.
#'
#' If \code{color.line} is a vector of colors, then the rates of the recessions are
#'classified accoring to the number of colors. The smallest rates correspond to the 
#'first color and the largest rates correspond to the last color.
#'
#' @param x an object of class "fall."
#' @param which either "All" for all recessions, or any number of element
#'numbers (not Index number) of the recessions to plot. If \code{which} is element
#'numbers, then each recession is plotted individually, otherwise all recessions
#'are plotted on the same graph.
#' @param set.up set up the graphics page? Set to \code{FALSE} if the graphics page
#'has been set up with a call to \code{setPage}.
#' @param before begin plot \code{before} days prior to recession.
#' @param after end plot \code{after} days after the end of the recession.
#'See \bold{Details}.
#' @param color.line the color of the recession lines. See \bold{Details}.
#' @param color.ba the color of the before and after lines, if drawn.
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @seealso \code{\link{fall}}
#' @keywords hplot
#' @export
#' @method plot fall
plot.fall <- function(x, which="All", set.up=TRUE, before=0, after=before, 
                      color.line="blue", color.ba="black", ...) {
  ## Initial processing
  beforeAfter <- (before + after) > 0
  GWlevel <- x$Data$GWLevel
  recess <- x$Data$Index
  GWDates <- diff(as.double(x$Data$Dates))
  xrange = range(GWlevel)
  Sel <- x$Recessions
  Index <- Sel$Index
  Len <- Sel$Length
  if(beforeAfter)
    trange <- c(-before, max(Len) + after)
  else
    trange <- c(0, max(Len))
  if(length(which) == 1L && which == "All") {
    which <- seq(along=Index)
    varyRanges <- FALSE
  } else {
    varyRanges <- TRUE
  }
  if(set.up) 
    setGD("FALL")
  if(!varyRanges) {
    AA.pl <- xyPlot(trange, xrange, Plot=list(what="none"), xtitle="Days in Recession",
                    ytitle="Ground-Water Level", xaxis.range=range(pretty(trange)))
  }
  ## compute classes if requested
  if(length(color.line) > 1L) {
    Class <- cut(rank(x$Recessions$RRate), breaks=length(color.line), labels=FALSE)
  } else {
    Class <- rep(1L, length(Index))
  }
  for(i in which) {
    GWSel <- recess == Index[i]
    if(beforeAfter) { # pack with before and after
      GWSel <- c(GWSel[-(1:before)], rep(F, before)) |
        c(rep(FALSE, after), GWSel[1:(length(GWSel) - after)]) |
        GWSel
    }
    xtoplot <- GWlevel[GWSel]
    if(beforeAfter) {
      ttoplot <- (-before):(Len[i] + after)
      ## check to verify that the preceeding and succeeding days are valid
      ttcheck <- GWDates[GWSel]
      if(ttcheck[before] != 1) {
        xtoplot[1:before] <- NA
      }
      if(ttcheck[Len[i] + after + 1] != 1) {
        xtoplot[(Len[i] + after + 2):(Len[i] + 2*after + 1)] <- NA
      }
    }
    else
      ttoplot <- 0:Len[i]
    if(varyRanges) {
      xrange <- range(xtoplot, na.rm=T)
      trange <- range(ttoplot)
      AA.pl <- xyPlot(trange, xrange, Plot=list(what="none"), xtitle="Days in Recession",
                      ytitle="Ground-Water Level")
    }
    start <- x$Recessions$Date[i]
    ## plot beforeAfter as line
    if(beforeAfter) {
      addXY(ttoplot, xtoplot, Plot=list(what="lines", color=color.ba))
      ba <- which((ttoplot %in% 0:Len[i]))
      addXY(ttoplot[ba], xtoplot[ba], Plot=list(what="lines", 
                                                color=color.line[Class[i]],
                                                width="color"))
    }
    else
      addXY(ttoplot, xtoplot, Plot=list(what="lines", color=color.line[Class[i]]))
    ## document plot if varyRanges
    if(varyRanges) {
      addTitle(Main=paste("Start date: ", start, ", Number: ", Index[i], sep=''))
    }
    else 
      addAnnotation(Len[i], xtoplot[beforeAfter + Len[i] + 1], annotation=Index[i], 
                    position="center")
  } # end of which loop
  invisible(x)
}

