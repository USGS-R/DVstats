#' Print an Object
#'
#' Print an object of class "fall" from a groundwater recession analysis.
#'
#' @param x an object of class "fall."
#' @param recessions logical if \code{TRUE}, then print the recession summary, otherwise
#'the recesisons are not printed
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @seealso \code{\link{fall}}
#' @keywords print
#' @export
#' @method print fall
print.fall <- function(x, recessions=FALSE, ...) {
  ## print a fall object
  N.recess <- nrow(x$Recessions)
  if(attr(x, "Confirmed")) {
    cat("\tConfirmed Recession summary")
  } else {
    cat("\tUnconfirmed Recession summary")
  }
  if(!is.null(x$STAID)) cat(" for well ", x$STAID, sep='')
  cat("\nNumber of recessions:", N.recess,
      "\n               Start:", as.character(x$Start),
      "\n                 End:", as.character(x$End),
      "\n Months start recess:", x$months2Sel,
      "\n    Minimum duration:", x$min.duration, "days\n\n", sep=" ")
  if(recessions) {
    print(x$Recessions)
  }
  invisible(x)
}
