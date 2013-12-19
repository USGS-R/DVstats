#' Plot an Object
#' 
#' Probability plot of an object of class "freqAnal" frequency analysis.
#'
#'If \code{which} is "Final," then the "traditional" probability plot 
#'is created that shows the non-zero values adjusted for any
#'zero values and the fitted line.
#'Otherwise \code{which} must be one of "default," "emp," "lp3," "ln3," 
#'or "lgev." In that case, the nonzero data are plotted on the 
#'corresponding probability scale (uniform for "emp"). If \code{which} is
#'"default," then the selected prediction method is plotted.
#'
#' @param x an object of class "freqAnal."
#' @param which either "Final," for the data and the fitted line on a normal
#'probability scale, or the method. See \bold{Details}.
#' @param set.up set up the graphics page? Set \code{set.up} to 
#'\code{FALSE} if the graphics page has been set up with a call to 
#'\code{setPage} or other device.
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @section Side Effect: The object \code{x} is plotted
#' @seealso \code{\link{freqAnal}}
#' @keywords hplot
#' @method plot freqAnal
#' @S3method plot freqAnal
plot.freqAnal <- function(x, which = "Final", set.up = TRUE, ...) {
  ## Preliminaries
  which=match.arg(which, c("Final", "default", "emp", "lp3", "ln3", "lgev"))
  STAID <- x$STAID
  mrgn <- c(NA, NA, 1.2, 0.5)
  desc <- x$desc
  if(desc == "")
    desc <- "Streamflow"
  if(x$N0 > 0L) {
    xdesc <- paste("Conditional Probability: ", desc, " > 0", sep="")
  } else
    xdesc <- "Probability"
  if(set.up) {
    setGD("freqAnal")
    xlab=7
  } else
    xlab=11
  Nx <- length(x$x)
  if(which == "Final") {
    if(Nx == 0) {
      plot.new()
      text(0.5, 0.5, "No non-zero values", family="USGS")
    } else if(min(x$x) == max(x$x)) {
      plot.new()
      pp <- x$N0/(x$N0 + length(x$x))
      if(pp == 0) {
        text(0.5, 0.5, paste("Constant observed data,\n all predicted values set to ",
                             x$x[1L], sep=""), family="USGS")
      } else {
        text(0.5, 0.5, paste("Constant observed data,\n all predicted values for probabilities greater than ",
                             round(pp, 4L), " set to ", x$x[1L], sep=""), family="USGS")
      }
    } else {
      xtoplot <- c(x$x, rep(0, x$N0))
      AA.pl <- probPlot(xtoplot, truncate=0, FLIP=TRUE, 
                        CDF=FALSE, xtitle="Probability",
                        ytitle=desc, xlabels=xlab,
                        margin=mrgn)
      ptoplot <- pnorm(AA.pl$xax$range) # The range
      # Adjust the bottom range to account for zero values
      if(x$N0) {
        ptoplot[1L] <- 1 - (1 - ptoplot[1L]) * 
          length(x$x) / length(xtoplot)
      }
      ptoplot <- seq(ptoplot[1L], ptoplot[2L], length.out=101L)
      Ytoplot <- predict(x, probs=ptoplot)$Est
      ## 1 - needed to adjust for the reverse sense of the x-axis
      addXY(1-ptoplot, Ytoplot, current=AA.pl)
    }
    which <- x$method
  } else { # The method
    if(which == "default")
      which <- x$method
    if(which == "lp3") {
      if(is.null(x$lp3)) {
        plot.new()
        text(0.5, 0.5, "No log-Pearson type III method", family="USGS")
      } else 
        probPlot(x$x, distribution="pearsonType3", alpha = 0.44,
                 ytitle=desc,
                 xtitle=xdesc,
                 margin=mrgn, xlabels=xlab,
                 mean=x$lp3$mnlx, sd=x$lp3$sdlx, skew=x$lp3$glx)
    } else if(which == "ln3") {
      if(is.null(x$ln3)) {
        plot.new()
        text(0.5, 0.5, "No 3-parameter log-normal method", family="USGS")
      } else {
        ## Check if valid 3-param fit (no zero predicted values in range)
        if(Nx < 27) {
          pck <- .02
        } else if(Nx < 55) {
          pck <- .01
        } else if(Nx < 111) {
          pck <- .005
        } else if(Nx < 279) {
          pck <- .002
        } else # this fails if we have more than about 550 observations
          pck <- .001
        xtmp <- x
        xtmp$method <- "ln3"
        if(predict(xtmp, pck)$Est == 0) {
          AA.pl <- probPlot(x$x - x$ln3$lamlx, distribution="normal", 
                            alpha = 0.44,
                            ytitle=paste(desc, " + ", round(-x$ln3$lamlx, 4), sep=""),
                            xtitle=xdesc,
                            margin=mrgn, xlabels=xlab,
                            mean=x$ln3$mnlx, sd=x$ln3$sdlx)
          refLine(horizontal=-x$ln3$lamlx, current=AA.pl)
          addAnnotation(.5, -x$ln3$lamlx, "Offset",
                        justification="center", current=AA.pl)
        } else # we are OK
          probPlot(x$x, distribution="ln3", alpha = 0.44,
                   ytitle=desc,
                   xtitle=xdesc,
                   margin=mrgn, xlabels=xlab,
                   meanlog=x$ln3$mnlx, sdlog=x$ln3$sdlx, 
                   lambda=x$ln3$lamlx, log=TRUE)
      }
    } else if(which == "lgev") {
      if(is.null(x$lgev)) {
        plot.new()
        text(0.5, 0.5, "No log-generalized extreme value method", family="USGS")
      } else 
        probPlot(x$x, distribution="gev", alpha = 0.44,
                 ytitle=desc,
                 xtitle=xdesc,
                 margin=mrgn, xlabels=xlab,
                 loc=x$lgev$loclx, scale=x$lgev$sclx, shape=x$lgev$shlx)
    } else { # which must be emp
      AA.pl <- probPlot(x$x, distribution="uniform", alpha = 0.5,
                        xlabels=(0:10)/10,
                        ytitle=desc,
                        xtitle=xdesc,
                        margin=mrgn)
      ## The empirical fit
      addXY(seq(0, 1, length.out=Nx+1L), c(x$x, x$x[Nx]),
            Plot=list(what="stairstep"), current=AA.pl)
    }
  }
  if(!is.null(STAID) && STAID != "Unknown") {
    st <- paste("Station: ", STAID, ",", sep="")
  } else
    st <- ""
  meths <- c(emp="empirical", lp3="log-Pearson type III",
             ln3="3-parameter log-normal", lgev="log-generalized extreme value")
  addCaption(paste(st, " fitted line based on the ", meths[which],
                   " method", sep=""))
  invisible(x)
}
