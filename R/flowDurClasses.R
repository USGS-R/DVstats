#' Flow Duration Classes
#'
#' Compute tables of class-limit flow values, class 
#counts, and percentages in class and percent exceedences.
#'
#' @param x the flow data to tabulate into class-limit flow values.
#' @param classes either the number of classes or the class-limits.
#' @return A data.frame containing the class limits, column LCL; the 
#'number of cases of each value of \code{x} in each class, column Cases;
#'the percentage in each class, column Pct; and the percent exceedence 
#'for each class, column Exceed.
#' @references USGS SWSTATS?
#' @export
flowDurClasses <- function(x, classes=35) {
	## Coding history:
	##    2014Feb18 DLLorenz Initial coding.
	##
  ## Initial processing, build class limits, if necessary
  if(length(classes) == 1) {
    clas <- vector(mode="double",length=classes)
    clas[1L] <- 0.0
    checkmin <- min(x)
    if (checkmin == 0) {
      checkmin <- min(x[x > 0])
      clas[2L] <- checkmin
    } else
      clas[2L] <- checkmin
    checkmax <- max(x)
    checkmax <- signif(checkmax+10^floor(log10(checkmax))/2, 1)
    factclas <- (checkmax/clas[2L])^(1/(classes-2))
    for (i in seq(3L, classes))
      clas[i] <- clas[i-1L]*factclas
    clas <- signif(clas, 2L)
    if(clas[2L] > checkmin)
      clas[2L] <- checkmin
  } else {
    clas <- classes
    classes <- length(clas)
  }
  # OK, right=F sets up cases equal to or greater than the lower limit
  # but strictly less than the upper limit, but include.lowest does allow
  # the possibility of equaling the largest
  cases <- tabulate(cut(x, breaks=clas, right=FALSE, include.lowest=TRUE))
  # Catch incomplete listing in specified classes.
  cmax <- sum(x > clas[classes], na.rm=TRUE)
  if(cmax > 0L) {
    warning("Class maximum exceeded")
    cases[classes] <- cmax
  }
  pct <- cases / sum(cases) * 100
  exceed <- 100 - cumsum(pct)
  # pad stats to length of class
  cases <- c(cases, rep(0L, classes - length(cases)))
  pct <- c(pct, rep(0, classes - length(pct)))
  if(length(exceed) < classes) {
    exceed <- c(100, exceed, rep(0, classes - length(exceed) - 1))
  } else
    exceed <- c(100, exceed[-classes])
  data.frame(LCL=clas, Cases=cases, Pct=pct, Exceed=exceed)
}
