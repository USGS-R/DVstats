#'Season of Year
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the season of the year.
#'
#'The season year is allowed to overlap the calendar year, so the \code{end.month}
#'may precede the \code{start.month}.
#'
#' @aliases seasonYear seasonYearMD
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'result in corresponding missing values in the output.
#' @param start.month a charcter string indicating the starting month of the season, 
#'must match either the month name (\code{month.name}) or the month abbreviation
#'(\code{month.abb}). See \bold{Details}.
#' @param start.day a character string of the form "mm/dd" indicating
#'the starting day of the season.x
#' @param end.month a charcter string indicating the ending month of the season, 
#'must match either the month name (\code{month.name}) or the month abbreviation
#'(\code{month.abb}). See \bold{Details}.
#' @param end.day a character string of the form "mm/dd" indicating
#'the last day of the season. Use "02/29" for the last day in
#'February.
#' @param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#' @return An ordered factor or numeric vector corresponding to the season of the year.
#'Input data values outside of the months specified are retruned as missing values
#'\code{NA}.
#' @note For \code{seasonYear}, the season year is defined as the period from the first day of the starting month
#'to the last day of the ending month. For \code{seasonYearMD}, it is the
#'specified month and day for the beginning and ending of the period.
#'The season year is designated by the calendar year in which it ends. Thus, the season
#'year starting December 1, 1998 and ending March 30, 1999, is the "1999 December-March season year."
#' @seealso \code{\link{month.name}}, \code{\link{month.abb}}
#' @keywords manip
#' @examples
#'
#'\dontrun{
#'library(USGSwsData)
#'data(QW05078470)
#'## Return an ordered factor
#'seasonYear(QW05078470$DATES)
#'## Should be the same
#'seasonYearMD(QW05078470$DATES)
#'}
#' @export
seasonYear <- function(x, start.month="June", end.month="September", numeric=FALSE) {
  ## Coding history:
  ##    2013Apr08 DLLorenz original coding
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for season year
  if(nchar(start.month) == 3) # use month.abb
    st.mn <- which(month.abb == start.month)
  else
    st.mn <- pmatch(start.month, month.name)
  if(nchar(end.month) == 3) # use month.abb
    en.mn <- which(month.abb == end.month)
  else
    en.mn <- pmatch(end.month, month.name)
  if(en.mn == st.mn) { # only one month
     out <- mn != st.mn
     yr[out] <- NA
  }
  else if(en.mn > st.mn) { # All within the year
    out <- mn < st.mn | mn > en.mn
	yr[out] <- NA
  }
  else { # Overlap
    out <- mn > en.mn & mn < st.mn
	yr[out] <- NA
    yr <- yr + ifelse(mn < en.mn + 1L, 0L, 1L)
  }
  if(numeric)
    return(yr)
  ordered(yr)
}

#' @export
seasonYearMD <- function(x, start.day="06/01", end.day="09/30", numeric=FALSE) {
  ## Coding history:
  ##    2013Nov13 DLLorenz original coding
  ##
  Jdays <- baseDay(x)
  yr <- year(x)
  if(start.day < end.day) { # Does not overlap the calendar year
    ## Logic from USGSwsBase::seasons
    breaks1 <- paste(c(start.day, end.day), "1972", sep="/") 
    breaks1 <- as.integer(as.Date(breaks1, format="%m/%d/%Y")) - 
      c(730L, 729L) # Logic wants last day
    Test <- get("==")
  } else {
    breaks1 <- paste(c(end.day, start.day), "1972", sep="/") 
    breaks1 <- as.integer(as.Date(breaks1, format="%m/%d/%Y")) - 
      c(729L, 730L) # Logic wants last day
    Test <- get("!=")
    ## Adjust the year
    yr[Jdays > breaks1[2L]] <- yr[Jdays > breaks1[2L]] + 1L
  }
  breaks1 <- c(-12, breaks1, 400)
  ## Cut into seasons
  Jdays <- cut(Jdays, breaks1, labels=FALSE)
  Pick <- Test(Jdays, 2L)
  yr[!Pick] <- NA
  if(numeric)
    return(yr)
  ordered(yr)
}
