\name{flowDurClasses}
\alias{flowDurClasses}
\title{Flow Duration Classes}
\usage{
flowDurClasses(x, classes = 35)
}
\arguments{
  \item{x}{the flow data to tabulate into class-limit flow
  values.}

  \item{classes}{either the number of classes or the
  class-limits.}
}
\value{
A data.frame containing the class limits, column LCL; the
number of cases of each value of \code{x} in each class,
column Cases; the percentage in each class, column Pct; and
the percent exceedence for each class, column Exceed.
}
\description{
Compute tables of class-limit flow values, class
}
\references{
USGS SWSTATS?
}

