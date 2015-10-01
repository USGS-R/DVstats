#' Flow-Duration Curve
#' 
#' Construct a consistent flow-duration curve from the 13 estiamtes from a regional regression.
#' 
#' @param x a vector of the 13 estimated points on the flow-duration curve from regresion regressions
#'in Minnesota.
#' @return a vector of 13 consitently decreasing estimates.
#' @export
consistentFDC <- function(x) {
	qx <- qnorm(1-c(0.0001, 0.001, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 0.9999))
	xd <- diff(x)
	if(any(xd > 0)) { # Fix em
		# protect against the last one being larger than the second to last
		x[13L] <- min(x)
		xm <- cummin(x)
		xd <- x > xm
		# log-linearly interpolate the xd values
		x[xd] <- exp(approx(qx[!xd], log(x[!xd]), xout=qx[xd])$y)
	}
	return(x)
}