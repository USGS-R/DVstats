#' Estimate FDC points
#' 
#' Estimate the streamflow at selected probability of exceedance values for
#'ungaged sites in Minnesota.
#' 
#' @param site.data a data frame consisting of one row and the correct columns
#'of basin characteristics for the region
#' @param region a character string indicating the region that contains the ungaged site,
#'must be one of "A," "BC," "D," "E," or "F."
#' @param expand logical value, if \code{TRUE}, then expand the computed points so that
#'a graph can be prepared, otherwise only the 13 estimated points.
#' @return A data frame containing the columns Probs, the exceedance probabilities,
#'and FDC, the estiamte streamflow at the specified probability level. The streamflows
#'are adjusted so the that are consistently non increasing.
#' @note The selected points on the FDC can also be obtained using StreamStats for
#'Minnesota.
#' @import smwrQW
#' @export
estFDC <- function(site.data, region, expand=TRUE) {
	if(missing(region)) {
		stop("Need region")
	}
	if(nrow(site.data) != 1) {
		stop("can only fit exactly 1 site at a time")
	}
	# The sequence correspond to 
	probs <- c(0.0001, 0.001, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 0.9999)
	retval <- numeric(13)
	# get PredModels from the package
	PredModels <- get("PredModels")
	# populate the FDC
	for(i in seq(13)) {
		retval[i] <- predict(PredModels[[region]][[i]], site.data)
	}
	retval <- consistentFDC(retval)
	if(expand) {
		# construct a longer sequence
		eprobs <- c(1e-04,0.0002125,0.000325,0.0004375,0.00055,0.0006625,0.000775,0.0008875,
								0.001,0.003375,0.00575,0.008125,0.0105,0.012875,0.01525,0.017625,0.02,
								0.02375,0.0275,0.03125,0.035,0.03875,0.0425,0.04625,0.05,
								0.05625,0.0625,0.06875,0.075,0.08125,0.0875,0.09375,0.1,
								0.11875,0.1375,0.15625,0.175,0.19375,0.2125,0.23125,0.25,
								0.28125,0.3125,0.34375,0.375,0.40625,0.4375,0.46875,0.5,
								0.53125,0.5625,0.59375,0.625,0.65625,0.6875,0.71875,0.75,
								0.76875,0.7875,0.80625,0.825,0.84375,0.8625,0.88125,0.9,
								0.90625,0.9125,0.91875,0.925,0.93125,0.9375,0.94375,0.95,
								0.955,0.96,0.965,0.97,0.975,0.98,0.985,0.99,
								0.991125,0.99225,0.993375,0.9945,0.995625,0.99675,0.997875,0.999,
								0.9991125,0.999225,0.9993375,0.99945,0.9995625,0.999675,0.9997875,0.9999)
		retval <- exp(approx(qnorm(probs), log(retval), xout=qnorm(eprobs))$y)
		# copy eprobs to probs for ouytput
		probs <- eprobs
	}
	return(data.frame(Probs=probs, FDC=retval))
}