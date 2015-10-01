#' QPPQ
#' 
#' Compute estimated flow at an ungaged site given the flow at the
#' reference streamgage, and the FDCs for the reference and ungaged sites.
#' 
#' @param Q.in the input index flows (missing values?)
#' @param FDC.in the index FDC dataset
#' @param FDC.out the FDC for the ungaged location
#' @return A vector of output flows at the ungaged site
#' @note Q-normal-log interpolation is used. This function can be applied
#'anywhere the method is valid.
#'@export
QPPQ <- function(Q.in, FDC.in, FDC.out) {
  P.ind <- qnorm(FDC.in$Probs)
  Fl.ind <- log(FDC.in$FDC)
  P.out <- qnorm(FDC.out$Probs)
  Fl.out <- log(FDC.out$FDC)
  # Get the Ps
  P.in <- approx(Fl.ind, P.ind, xout=log(Q.in))$y
  Q.out <- exp(approx(P.out, Fl.out, xout=P.in)$y)
  return(Q.out)
}