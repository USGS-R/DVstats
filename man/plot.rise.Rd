\name{plot.rise}
\alias{plot.rise}
\title{Plot an Object}
\usage{
\method{plot}{rise}(x, which = "All", set.up = TRUE, ...)
}
\arguments{
  \item{x}{an object of class "rise."}

  \item{which}{either "All" for the entire hydrogaph, "by
  year" for calendar year hydrographs, or the calendar year
  to plot.}

  \item{set.up}{set up the graphics page? Set to
  \code{FALSE} if the graphics page has been set up with a
  call to \code{setPage}.}

  \item{\dots}{not used, required for other methods.}
}
\value{
The object \code{x} is returned invisibly
}
\description{
Plot an object of class "rise" from a groundwater rise
analysis.
}
\section{Side Effect}{
  The object \code{x} is plotted
}
\seealso{
\code{\link{rise}}
}
\keyword{hplot}

