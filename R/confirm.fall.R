#'Confirm Groundwater Recessions
#'
#'This function processes the output of function \code{fall} and requires the user to 
#'accept or reject the recession.
#'
#' @param x an object of class "fall."
#' @param all accept all recessions without review?
#' @param before begin review plot \code{before} days prior to recession.
#' @param after end review plot \code{after} days after the end of the recession.
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned with updated recessions
#' @keywords manip
#' @method confirm fall
#' @export
confirm.fall <- function(x, all=FALSE, before=3, after=3, ...) {
  ## This function steps though each of the identified recessions and
  ## forces the user to accept, reject, or modify the recession.
  ## A data frame of the accepted or modified recessions is returned.
  ## The data frame is of class recessions.
  ## if .Station == TRUE and a STAID has been defined, then the .Station
  ## list is updated with the median recession index.
  ##
  if(all) {
    attr(x, "Confirmed") <- TRUE
    return(x)
  }
  Sel <- x$Recessions
  N <- nrow(Sel)
  keep <- logical(N)
  i <- 1
  loop <- TRUE
  setGD("Confirm")
  while(loop) {
    plot(x, which=i, set.up=FALSE, before=before, after=after)
    todo <- menu(c("Accept", "Reject"),
                       title=paste("No. ", Sel$Index[i], sep=''))
    if(todo == 0)
      loop <- FALSE
    else if(todo == 1) { # keep it
      keep[i] <- TRUE
      i <- i + 1
      if(i > N) loop <- FALSE
    }
    else { # reject it
      i <- i + 1
      if(i > N) loop <- FALSE
    }
  }
  x$Recessions <- Sel[keep, ]
  attr(x, "Confirmed") <- TRUE
  return(x)
}
