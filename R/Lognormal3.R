#' The 3-Parameter Log Normal Distribution
#' 
#' Density, distribution function, quantile function and random 
#'generation for the 3-parameter log normal distribution whose 
#'logarithm of the data minus lambda has mean equal to meanlog 
#'and standard deviation equal to sdlog.
#'
#' @aliases Lognormal3, dln3, pln3, qln3, rln3
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param meanlog the mean of the logarithm of the data minus \code{lamda}.
#' @param sdlog the stadndard deviation of the logarithm of 
#'the data minus \code{lamda}.
#' @param lambda the offset.
#' @param log transform the quantiles by their natural logarithm?
#' @return The density, cumulative probability, quantile,or random
#'deviate.
#' 
#' @rdname Lognormal3
#' @export
dln3 <- function(x, meanlog = 0, sdlog = 1, lambda=0)
  return(exp(-0.5*((log(x - lambda) - meanlog)/sdlog)^2)/((x - lambda)*sdlog*sqrt(2*pi)))

#' @rdname Lognormal3
#' @export
pln3 <- function(q, meanlog = 0, sdlog = 1, lambda=0)
  plnorm(q - lambda, meanlog=meanlog, sdlog=sdlog)

#' @rdname Lognormal3
#' @export
qln3 <- function(p, meanlog = 0, sdlog = 1, lambda=0, log=FALSE) {
  retval <- qlnorm(p, meanlog = meanlog, sdlog = sdlog) + lambda
  if(log)
    retval <- log(retval)
  return(retval)
}

#' @rdname Lognormal3
#' @export
rln3 <- function(n, meanlog = 0, sdlog = 1, lambda=0)
  rlnorm(n, meanlog=meanlog, sdlog=sdlog) + lambda
