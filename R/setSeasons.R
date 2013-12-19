#'Define Seasons
#'
#'Create a list that defines how seasons are defined for a particular application.
#'
#'At a minimum, the end month of the season must be given. If only the end month
#'is given, then the seasons must be in sequential month order. For clarity, the beginning and
#'end months, or all months, in order, for the season can be given.
#'The season is allowed to overlap the calendar year.\cr
#'Month abbreviations may be used instead of the full month name. See the documentation
#'for \code{month.abb}.
#'
#'@param \dots named arguments of months that define a season. See \bold{Details}.
#'@return A list containing the name of the season with the first and last month.
#'@note The season year is defined as the period from the first day of the starting month
#'to the last day of the ending month.
#'The season year is designated by the calendar year in which it ends. Thus, the season
#'year starting December 1, 1998 and ending March 30, 1999, is the "1999 December-March season year."
#'@seealso \code{\link{seasonYear}}, \code{\link{month.abb}}
#'@keywords manip
#'@examples
#'# Define seasons that might possibly be used in the northern U.S.
#'setSeasons(Winter="March", Spring="May", Summer="September", Fall="November")
#'# The same using begin and end months and abbreviations
#'setSeasons(Winter=c("Dec", "Mar"), Spring=c("Apr", "May"), 
#' Summer=c("Jun", "Sep"), Fall=c("Oct", "Nov"))
#'
#'@export
setSeasons <- function(...) {
  ## Coding history:
  ##    2013Apr21 DLLorenz original coding
  ##
  dots <- list(...)
  seasName <- names(dots)
  N <- length(seasName)
  cklen <- sapply(dots, length)
  retval <- dots
  if(all(cklen == 1L)) { # Must be end months
    ## Fix abbreviations and convert to vector of month numbers
    dots <- sapply(dots, function(x) pmatch(x, month.name))
    for(i in seq(2L, N))
      retval[[i]] <- c(month.name[dots[i-1L] + 1L], retval[[i]])
    ## Fix the last one
    if(dots[N] == 12L)
      retval[[1L]] <- c("January", retval[[1L]])
    else
      retval[[1L]] <- c(month.name[dots[N] + 1L], retval[[1L]])
  }
  else # Each must contain at least the first and last months
    for(i in seasName) {
      mnths <- retval[[i]]
      retval[[i]] <- c(match.arg(mnths[1L], month.name),
        match.arg(mnths[length(mnths)], month.name))
    }
  return(retval)
}
