#' Print an Object
#'
#' Print an object of class "rise" from a groundwater rise analysis.
#'
#'
#' @param x an object of class "rise."
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @section Side Effect: The object \code{x} is printed
#' @seealso \code{\link{rise}}
#' @keywords print
#' @method print rise
#' @S3method print rise
print.rise <- function(x, ...) {
  sumRise <- aggregate(as.data.frame(x[, "Rise", drop=F]), list(Years=year(x$Dates)), sum)
  cat("\tAnnual rise data")
  STAID <- attr(x, "STAID")
  if(!is.null(STAID)) cat(" for station ", STAID, sep='')
  cat("\n")
  print(sumRise)
  invisible(x)
}
