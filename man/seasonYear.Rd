\name{seasonYear}
\alias{seasonYear}
\alias{seasonYearMD}
\title{Season of Year}
\usage{
seasonYear(x, start.month = "June", end.month = "September",
  numeric = FALSE)
}
\arguments{
  \item{x}{an object of class "Date" or "POSIXt." Missing
  values are permitted and result in corresponding missing
  values in the output.}

  \item{start.month}{a charcter string indicating the
  starting month of the season, must match either the month
  name (\code{month.name}) or the month abbreviation
  (\code{month.abb}). See \bold{Details}.}

  \item{start.day}{a character string of the form "mm/dd"
  indicating the starting day of the season.x}

  \item{end.month}{a charcter string indicating the ending
  month of the season, must match either the month name
  (\code{month.name}) or the month abbreviation
  (\code{month.abb}). See \bold{Details}.}

  \item{end.day}{a character string of the form "mm/dd"
  indicating the last day of the season. Use "02/29" for
  the last day in February.}

  \item{numeric}{a logical value that indicates whether the
  returned values should be numeric \code{TRUE} or an
  ordered factor \code{FALSE}. The default value is
  \code{FALSE}.}
}
\value{
An ordered factor or numeric vector corresponding to the
season of the year. Input data values outside of the months
specified are retruned as missing values \code{NA}.
}
\description{
Create an ordered factor or numeric values from a vector of
dates based on the season of the year.
}
\details{
The season year is allowed to overlap the calendar year, so
the \code{end.month} may precede the \code{start.month}.
}
\note{
For \code{seasonYear}, the season year is defined as the
period from the first day of the starting month to the last
day of the ending month. For \code{seasonYearMD}, it is the
specified month and day for the beginning and ending of the
period. The season year is designated by the calendar year
in which it ends. Thus, the season year starting December
1, 1998 and ending March 30, 1999, is the "1999
December-March season year."
}
\examples{
\dontrun{
library(USGSwsData)
data(QW05078470)
## Return an ordered factor
seasonYear(QW05078470$DATES)
## Should be the same
seasonYearMD(QW05078470$DATES)
}
}
\seealso{
\code{\link{month.name}}, \code{\link{month.abb}}
}
\keyword{manip}

