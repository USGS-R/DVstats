#' Recess Merging
#'
#' Append two recession analyses.
#'
#' @param x the recession analysis to be appended.
#' @param y the recession analysis to append. The recession
#'period in \code{y} is assumed to be after the period in
#'\code{x}.
#' @return An object of class "recess" consisting of the
#'recession in \code{x} and \code{value}.
#'
#' @export
join <- function(x, y) {
	## Coding history:
	##    2013Jun13 DLLorenz Initial coding.
	##
	## Verify that y is class recess
	if(class(y)[1L] != "recess")
		stop("The argument y must be of class \"recess\"")
	## Determine the recession offset value and append Data
	RecStart <- max(x$Data$Recess)
	D2 <- y$Data
	D2$Recess <- ifelse(D2$Recess > 0L, D2$Recess + RecStart, 0L)
	x$Data <- rbind(x$Data, D2)
	## Now adjust the recessions
	R2 <- y$Recessions
	R2$Index <- R2$Index + RecStart
	x$Recessions <- rbind(x$Recessions, R2)
	## Now finish up
	x$End <- y$End
	x$min.duration <- min(x$min.duration, y$min.duration)
	x$months2sel <- union(x$months2sel, y$months2sel)
	attr(x, "Confirmed") <- attr(x, "Confirmed") & 
		attr(y, "Confirmed")
	return(x)
}
