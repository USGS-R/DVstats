#' Print an Object
#'
#' Print an object of class "rora" from a streamflow recession 
#'displacement analysis.
#'
#' @param x an object of class "rora"
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly
#' @section Side Effect: The object \code{x} is printed
#' @seealso \code{\link{rora}}
#' @keywords print
#' @method print rora
#' @export
print.rora <- function(x, ...) {
  if(x$ierr > 0) {
    cat("\n", switch(x$ierr,
                     "invalid value for incAnteRec",
                     "gap in data between Start and End",
                     "error computing itbase",
                     "cannot detect first peak",
                     "cannot detect first peak",
                     "more than 6000 peaks detected"),
        "\n\n")
    return(invisible(x))
  }
  N <- x$npeaks
  cat("\tRora summary")
  if(!is.null(x$STAID)) cat(" for station ", x$STAID, sep='')
  cat("\nNumber of peaks:", N,
      "\n  Starting year:", x$iyearst,
      "\n    Ending year:", x$iyearen, "\n", sep=" ")
  if(x$ierr < 0)
    cat("\nCaution ", switch(abs(x$ierr),
                             "the maximum number of days to determine ground-water discharge is less that itbase",
                             "drainage area less than 1 square mile",
                             "drainage area greater than 500 square miles"),
        "\n\n", sep="")
  invisible(x)
}

