#' Confirm an Analysis
#' 
#' Review and accept the results of an analysis
#' 
#' @param x the object to be confirmed
#' @param \dots additional arguments required for specific methods
#' @return The object returned depends on the specific method.
#' @keywords manip
#' @export confirm
confirm <- function(x, ...)
  # Required if smwrStats is not loaded
  UseMethod("confirm")
