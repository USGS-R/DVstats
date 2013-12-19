#' Frequency Analysis Predictions
#' 
#' Compute values for selected probability values.
#'
#' @param object an object of class "freqAnal."
#' @param probs the probability values to compute.
#' @param non.e treat \code{probs} as non-exceedance values?
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' 
#' @method predict freqAnal
#' @S3method predict freqAnal
predict.freqAnal <- function(object, 
                        probs=c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.96, 0.98, 0.99),
                        non.e=TRUE, ...) {
  ## Compute the conditional adjustment and the value to compute
  cond.adj <- length(object$x) / (object$N0 + length(object$x))
  pvals <- (1 - probs)/cond.adj
  if(non.e) 
    pvals <- 1 - pvals
  ToDo <- pvals > 0 & pvals < 1
  ## Compute the values
  Est <- double(length(probs))
  if(object$method == "emp") {
    Est[ToDo] <- quantile(object$x, probs=pvals[ToDo], names=FALSE, type=2)
  } else if(object$method == "lp3") {
    if(is.na(object$lp3$glx)) {
      ## Set pvals > cond to NA
      Est[ToDo] <- NA_real_
    } else 
      Est[ToDo] <- qlpearsonIII(pvals[ToDo], object$lp3$mnlx, 
                                object$lp3$sdlx, object$lp3$glx)
  } else if(object$method == "ln3") {
    Est[ToDo] <- pmax(qln3(pvals[ToDo], object$ln3$mnlx,
                      object$ln3$sdlx, object$ln3$lamlx), 0)
  } else # Must be lgev
    Est[ToDo] <- exp(qgev(pvals[ToDo], object$lgev$loclx,
                          object$lgev$sclx, object$lgev$shlx))
  retval <- data.frame(Probs=probs, Est=Est)
  return(retval)
}
