\name{confirm.recess}
\alias{confirm.recess}
\title{Confirm Streamflow Recessions}
\usage{
  \method{confirm}{recess} (x, all = FALSE, before = 1,
    after = 5, ...)
}
\arguments{
  \item{x}{an object of class "recess."}

  \item{all}{accept all recessions without review?}

  \item{before}{begin review plot \code{before} days prior
  to recession.}

  \item{after}{end review plot \code{after} days after the
  end of the recession.}

  \item{\dots}{not used, required for other methods.}
}
\value{
  The object \code{x} is returned with updated recessions
}
\description{
  This function processes the output of function
  \code{recess} and requires the user to accept or reject
  the recession.
}

