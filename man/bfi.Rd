% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bfi.R
\name{bfi}
\alias{bfi}
\title{Baseflow Separation}
\usage{
bfi(Flow, Dates, Start = NULL, End = NULL, by = "water year", f = 0.9,
  N = 5L, STAID = "Unknown")
}
\arguments{
\item{Flow}{the daily streamflow to be separated missing values are not permitted
within the time specified by \code{Start} and \code{end}.}

\item{Dates}{the date for each \code{x}, should be of class "Date." Missing values
are not permitted.}

\item{Start}{the start date for the analysis, can be either a character string or
class "Date."}

\item{End}{the end date for the analysis, can be either a character string or
class "Date."}

\item{by}{string describing how to construct turning points. See
\bold{Details}.}

\item{f}{the factor for identifying turning points.}

\item{N}{the time perod for period for calculating the turning points.}

\item{STAID}{the station identifier for the data.}
}
\value{
an object of class "baseflow" and inherits class "data.frame" of the selected data, a data
frame of the baseflow information, and other information about the analysis.
}
\description{
Extract baseflow from a daily streamflow record using the method proposed by
the Institute of Hydrology (1980a,b).
}
\details{
The original description of constructing turning points proposed dividing
water years into 5-day increments and selecting the minimum flow for each period.
Gustard and others (1992) describe using continuous 5-day increments rather than restarting
on the water year.

The \code{bfi} program developed by Wahl and Wahl available at 
\url{https://www.usbr.gov/pmts/hydraulics_lab/twahl/bfi/} uses the continuous record
for constructing the \code{N-day} periods. The argument \code{by} can select either
water year, continuous or calendar year records.
}
\examples{

\dontrun{
library(smwrData)
data(ChoptankFlow)
# Process by calendar year as that is the retrieval range
ChopBFI <- with(ChoptankFlow, bfi(Flow, datetime, by="calendar year",
STAID="01491000"))
head(ChopBFI, 20)
}
}
\references{
Gustard, A., Bullock, A., and Dixon, J.M., 1992,
Low flow estimation in the United Kingdom: Institue of Hydrology 
Report No. 108, 88 p. and appendixes.

Institute of Hydrology, 1980, Low flow studies: 
Wallingford, Oxon, United Kingdom, Report No. I, 41 p.

Institute of Hydrology, 1980, Low flow studies: Wallingford, Oxon, 
United Kingdom, Report No. 3, p. 12- 19.
}
\keyword{baseflow}
