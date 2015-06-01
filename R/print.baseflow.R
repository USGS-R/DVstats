#' Print an Object
#'
#' Print an object of class "baseflow" from any baseflow analysis.
#'
#'
#' @param x an object of class "baseflow."
#' @param digits the number of digits to use for printing numbers.
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @section Side Effect: The object \code{x} is printed
#' @seealso \code{\link{part}}, \code{\link{hysep}}, \code{\link{bfi}}
#' @keywords print
#' @method print baseflow
#' @export
print.baseflow <- function(x, digits=4, ...) {
  BFI <- round(sum(x$BaseQ)/sum(x$Flow), digits)
  From <- format(x$Dates[1L])
  To <- format(x$Dates[nrow(x)])
  cat("\tBaseflow analysis")
  STAID <- attr(x, "STAID")
  if(STAID == "Unknown")
    STAID <- NULL
  if(!is.null(STAID)) cat(" for station ", STAID, sep="")
  cat(":\n")
  cat(" index ", BFI, "\nmethod ", attr(x, "type"), 
      "\nperiod ", From, " to ", To, "\n\n", sep="")
  invisible(x)
}
