#' Harmonic Mean
#'
#' Compute the harmonic mean accoring to the EPA DFLOW manual.
#'
#' @param x the data to compute the harmonic mean.
#' @param na.rm a logical value indicating whether NA values 
#'should be removed before computation.
#' @return The harmonic mean of \code{x}.
#' @references Rossman, L.A., No Date, DFLOW user's manual:
#'U.S. Environmental Protection Agency, Cincinnati, Ohio, 26 p.
#' @export
hmean <- function(x, na.rm=FALSE) {
	## Coding history:
	##    2013Aug01 DLLorenz Initial coding.
	##
  ## Initial processing
  if(na.rm)
    x <- x[!is.na(x)]
  if(length(x) == 0L)
    return(NaN)
  if(any(is.na(x)))
    return(NA_real_)
  if(any(x < 0))
    return(NaN)
  ## OK do it
  N <- length(x)
  ## Strip 0s
  x <- x[x > 0]
  Ngt0 <- length(x)
  if(Ngt0 == 0L)
    return(0)
  ## Finally, do it
	return(Ngt0/N * Ngt0/sum(1/x))
}
