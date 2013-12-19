\name{rise}
\alias{rise}
\title{Rises}
\usage{
  rise(x, Dates, Start = NULL, End = NULL, MPelev = NULL,
    STAID = "Unknown")
}
\arguments{
  \item{x}{the daily value data to be summarized. Missing
  values are not permitted within the time specified by
  \code{Start} and \code{end}.}

  \item{Dates}{the date for each \code{x}, should be of
  class "Date." Missing values are not permitted.}

  \item{Start}{the start date for the analysis, can be
  either a character string or class "Date."}

  \item{End}{the end date for the analysis, can be either a
  character string or class "Date."}

  \item{MPelev}{the measuring point elevation. Required if
  \code{x} is a depth below land surface or other measuring
  point.}

  \item{STAID}{the station identifier for the data.}
}
\value{
  an object of class "rise" and inherits class "data.frame"
  of the selected data, a data frame of the recession
  information, and other information about the analysis.
}
\description{
  Identify groundwater rises
}
\examples{
\dontrun{
library(USGSwsData)
data(GlacialRidge)
with(GlacialRidge, rise(G12, datetime, MPelev=1126.42, STAID="G12"))
}
}
\keyword{recession}

