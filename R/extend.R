#'Series Extention
#'
#'Extend a regular series with missing values at the beginning or end by replacing
#'the beginning missing values with the first non-missing value and replacing
#'the ending missing values with the last non-missing value
#'
#'
#'@param x a numeric vector that represent a regular series. Missing values are permitted and
#'are replaced with non-missing values.
#'@return A vector like \code{x} with missing value replaced.
#'@note Interior missing values should be replaced first with a function like
#'\code{fillMissing}.
#'@seealso \code{\link{fillMissing}}
#'@export
#'@keywords manip
#'@examples
#'XT <- c(NA, NA, 1.2, 2.4, 3.5, NA)
#'extend(XT)
#'@export
extend <- function(x) {
  miss <- is.na(x)
  firstgood <- x[!miss][1L]
  K <- 1L
  while(miss[K]) { # First one is missing
    x[K] <- firstgood
    K <- K + 1L
  }
  N <- length(x)
  lastgood <- x[!miss][sum(!miss)]
  while(miss[N]) { # Last one is missing
    x[N] <- firstgood
    N <- N - 1L
  }
  return (x)
}
