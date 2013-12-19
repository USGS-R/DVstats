#'Climate Year
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the climate year.
#'
#'
#'@param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'reesult in corresponding missing values in the output.
#'@param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#'@return An ordered factor or numeric vector corresponding to the climate year.
#'@note The climate year is defined as the period from April 1 to March 30.
#'The climate year is designated by the calendar year in which it ends. Thus, the
#'year ending March 30, 1999, is the "1999 climate year."
#'@seealso \code{\link{year}}, \code{\link{waterYear}}
#'@keywords manip
#'@examples
#'
#'library(USGSwsData)
#'data(QW05078470)
#'## Return an ordered factor
#'climateYear(QW05078470$DATES)
#'@export
climateYear <- function(x, numeric=FALSE) {
  ## Coding history:
  ##    2013Apr08 DLLorenz original coding
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for climate year
  yr <- yr + ifelse(mn < 4L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}
