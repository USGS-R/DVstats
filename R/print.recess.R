#'Print an Object
#'
#'Print surface-water recession summary information
#'
#'
#'@param x the object to be printed.
#'@param digits the number of digits to use when printing numeric values.
#'@param \dots  not used, required for other methods.
#'@return The object \code{x} is returned invisibly
#'@section Side Effect: The object \code{x} is printed
#'@seealso \code{\link{recess}}
#'@keywords print
#'@method print recess
#'@export
print.recess <- function(x, digits=4, ...) {
  ## print a recess object
  N.recess <- nrow(x$Recessions)
  cat("\tRecession summary")
  if(is.null(attr(x, "Confirmed")) || !attr(x, "Confirmed")) {
    cat(" (unconfirmed)")
    summ <- FALSE
  } else {
    cat(" (confirmed)")
    summ <- TRUE
  }
  if(!is.null(x$STAID)) cat(" for station ", x$STAID, sep='')
  St <- as.character(x$Start)
  En <- as.character(x$End)
  cat("\nNumber of recessions:", N.recess,
      "\n     Start of period:", St,
      "\n       End of period:", En,
      "\n Months start recess:", x$months2Sel,
      "\n    Minimum duration:", x$min.duration, "days\n\n", sep=" ")
  if(summ) 
    cat("Median recession rate is ", signif(median(x$Recessions$K), digits), " days per log cycle\n", sep="")
  invisible(x)
}
