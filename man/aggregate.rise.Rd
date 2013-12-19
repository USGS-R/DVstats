\name{aggregate.rise}
\alias{aggregate.rise}
\title{Compute Recharge}
\usage{
  \method{aggregate}{rise} (x, by = "months", SY = 0.25,
    ...)
}
\arguments{
  \item{x}{an object of class "rise."}

  \item{by}{the time period to aggregate by. See
  \bold{Details}.}

  \item{SY}{the specific yield requied to give results in
  recharge rather than rise.}

  \item{\dots}{not used, required for other methods.}
}
\value{
  The recharge for each period specified in \code{by}. The
  units are the same as for \code{x}.
}
\description{
  Computes recharge from an object of class "rise" from a
  groundwater rise analysis.
}
\details{
  The argument \code{by} can be either a character
  indicating the period, or a list created by
  \code{setSeasons}. If a character , thne must be
  "months," "years," "calendar years," "water years," or
  "climate years." May be abbreviated; and "years" is the
  same as "calendar years."
}
\examples{
\dontrun{
library(USGSwsData)
data(GlacialRidge)
G12.rise <- with(GlacialRidge, rise(G12, datetime, MPelev=1126.42, STAID="G12"))
# monthly summary of recharge in feet
aggregate(G12.rise)
}
}
\seealso{
  \code{\link{rise}}, \code{\link{setSeasons}}
}
\keyword{recharge}

