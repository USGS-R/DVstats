#' Daily Value Durations
#'
#' Compute durations .
#'
#' @param x the daily value data to be summarized. Missing values are permitted, but
#'are treated as gaps---durations are reset.
#' @param Dates the date for each \code{x}, should be of class "Date." Missing values
#'are not permitted.
#' @param Start the start date for the analysis, can be either a character string or
#'class "Date."
#' @param End the end date for the analysis, can be either a character string or
#'class "Date."
#' @param base the base or threshold value for the test.
#' @param test a character string indicating the comparison between
#'\code{x} and \code{base}. Should be one of "<=," "<," ">=," or ">."
#' @param STAID the station identifier for the data.
#' @return A data.frame containing the \code{STAID}, the date 
#'on which the duration started, the length of the duration period, and the
#'minimum or maximum, depending on the test, during the period of the duration.
#' @examples
#'
#'\dontrun{
#'# Get daily flow values for 05078470 for 2003
#'library(smwrData)
#'data(Q05078470)
#'# When are 0 flows?
#'with(Q05078470, baseDur(FLOW, DATES, base=0, STAID="05078470"))
#'# Peak flows?
#'with(Q05078470, baseDur(FLOW, DATES, base=3, test=">=", 
#'STAID="05078470"))
#'}
#' @export
baseDur <- function(x, Dates, Start=NULL, End=NULL, 
                    base=quantile(x, probs=.1, na.rm=TRUE),
                    test="<=", STAID="") {
  ## Coding history:
  ##    2014May08 DLLorenz Initial coding.
  ##
  call <- match.call()
  call <- capture.output(dput(call))
  Dates <- as.Date(Dates) # Force the issue
  ## Limit data if needed
  STAID <- as.character(STAID[1L])
  if(!is.null(Start)) {
    Start <- as.Date(Start)
    sel <- Dates >= Start
    Dates <- Dates[sel]
    x <- x[sel]
  }
  if(!is.null(End)) {
    End <- as.Date(End)
    sel <- Dates <= End
    Dates <- Dates[sel]
    x <- x[sel]
  }
  ## Construct working data set
  DF <- data.frame(STAID=STAID, StartDate=Dates,x=x, stringsAsFactors=FALSE)
  ## Insert Missings where gaps in Dates, to force new durations
  if(!all(diff(Dates) == 1))
    DF <- insertMissing(DF, "StartDate")
  do.test <- get(test)
  DF$e <- eventNum(do.test(DF$x, base), reset=TRUE)
  DF$Duration <- eventLen(DF$e)
  DF <- DF[DF$Duration > 0, ]
  ## Compute the min or max for each duration
  if(test %cn% "<") {
    X <- tapply(DF$x, DF$e, min)
  } else
    X <- tapply(DF$x, DF$e, max)
  retval <- DF[!duplicated(DF$e), ]
  # Clean up and add min/max
  retval$x <- NULL
  retval$e <- NULL
  if(test %cn% "<") {
    retval$Min <- X
  } else
    retval$Max <- X
  comment(retval) <- c("Call:",call)
  return(retval)
}
