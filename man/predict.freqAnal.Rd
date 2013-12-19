\name{predict.freqAnal}
\alias{predict.freqAnal}
\title{Frequency Analysis Predictions}
\usage{
  \method{predict}{freqAnal} (object,
    probs = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.96, 0.98, 0.99),
    non.e = TRUE, ...)
}
\arguments{
  \item{object}{an object of class "freqAnal."}

  \item{probs}{the probability values to compute.}

  \item{non.e}{treat \code{probs} as non-exceedance
  values?}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  The object \code{x} is returned invisibly.
}
\description{
  Compute values for selected probability values.
}

