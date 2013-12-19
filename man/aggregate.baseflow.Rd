\name{aggregate.baseflow}
\alias{aggregate.baseflow}
\title{Baseflow Summary}
\usage{
  \method{aggregate}{baseflow} (x, by = "months",
    index = FALSE, ...)
}
\arguments{
  \item{x}{an object of class "baseflow."}

  \item{by}{the time period to aggregate by. See
  \bold{Details}.}

  \item{index}{compute the baseflow index (proportion of
  baseflow to total flow) rather than baseflow?}

  \item{\dots}{not used, required for other methods.}
}
\value{
  The baseflow for each period specified in \code{by}. The
  units are the same as for \code{x}.
}
\description{
  Computes baseflow statistics for user-specified periods
  of time.
}
\details{
  The aregument \code{by} can be either a character
  indicating the period, or a list created by
  \code{setSeasons}. If a character , then must be
  "months," "years," "calendar years," "water years,"
  "climate years," or "total." May be abbreviated; and
  "years" is the same as "calendar years."
}
\examples{
\dontrun{
library(USGSwsData)
data(GlacialRidge)
G12.hysep <- with(ChoptankFlow, hysep(Flow, datetime, da=113,
 STAID="01491000"))
# monthly summary of recharge in feet
aggregate(G12.hysep)
}
}
\seealso{
  \code{\link{part}}, \code{\link{hysep}},
  \code{\link{bfi}}, \code{\link{setSeasons}}
}
\keyword{baseflow}

