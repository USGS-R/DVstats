#'Confirm Streamflow Recessions
#'
#'This function processes the output of function \code{recess} and requires the user to 
#'accept or reject the recession.
#'
#'@param x an object of class "recess."
#'@param all accept all recessions without review?
#'@param before begin review plot \code{before} days prior to recession.
#'@param after end review plot \code{after} days after the end of the recession.
#'@param \dots  not used, required for other methods.
#'@return The object \code{x} is returned with updated recessions
#'@method confirm recess
#'@S3method confirm recess
confirm.recess <- function(x, all=FALSE, before=1, after=5, ...) {
  ## This function steps though each of the identified recessions and
  ## forces the user to accept or reject the recession.
  ##
  ## Begin
  if(all) {
    attr(x, "Confirmed") <- TRUE
    return(x)
  }
  Sel <- x$Recessions
  N <- nrow(Sel)
  keep <- logical(N)
  i <- 1L
  loop <- TRUE
  setGD("Confirm")
  while(loop) {
    plot(x, which=i, set.up=FALSE, before=before, after=after)
    todo <- menu(c("Accept", "Reject"),
      title=paste("No. ", Sel$Index[i], sep=''))
    if(todo == 0)
      loop <- FALSE
    else if(todo == 1L) {
      keep[i] <- TRUE
      i <- i + 1L
      if(i > N) loop <- FALSE
    }
    else if(todo == 2L) {
      i <- i + 1L
      if(i > N) loop <- FALSE
    }
  }
  ## Close graph to indicate done!
  dev.off()
  ## Rename rows 
  Recessions <- x$Recessions[keep,]
  rownames(Recessions) <- as.character(seq(nrow(Recessions)))
  x$Recessions <- Recessions
  attr(x, "Confirmed") <- TRUE
  return(x)
}
