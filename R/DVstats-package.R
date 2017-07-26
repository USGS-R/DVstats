#'Functions for manipulating daily values
#'
#'This package has specialized functions for managing or
#'manipulating hydrologic daily-value data.
#'
#'\tabular{ll}{ Package: \tab DVstats\cr 
#'Type: \tab Package\cr 
#'License: \tab CC0\cr 
#'Depends: \tab methods, lubridate, smwrBase, smwrGraphs, 
#' smwrStats, smwrQW, evd, stats, stats4, robust\cr } 
#'This package contains functions that manage or manipipulate  
#'hydrologic daily-value data. The functions in this package
#'are listed below, grouped by a general description of the task.
#'
#'Functions to compute baseflow from the streamflow record and
#'functions that summarize those computations.\cr
#'\code{\link{bfi}}\cr
#'\code{\link{hysep}}\cr
#'\code{\link{part}}\cr
#'\code{\link{aggregate.baseflow}}\cr
#'\code{\link{plot.baseflow}}\cr
#'\code{\link{print.baseflow}}\cr
#'
#'Functions that analyze groundwater record, primarily for the
#'estimation of recharge.\cr
#'\code{\link{fall}}\cr
#'\code{\link{rise}}\cr
#'\code{\link{wtf}}\cr
#'\code{\link{aggregate.rise}}\cr
#'\code{\link{confirm.fall}}\cr
#'\code{\link{plot.fall}}\cr
#'\code{\link{plot.rise}}\cr
#'\code{\link{print.fall}}\cr
#'\code{\link{print.rise}}\cr
#'\code{subset.fall}\cr
#'
#'Functions to compute or analyze flow-duration statistics.\cr
#'\code{\link{baseDur}}\cr
#'\code{\link{consistentFDC}}\cr
#'\code{\link{flowDurClasses}}\cr
#'\code{\link{estFDC}}\cr
#'\code{\link{QPPQ}}\cr
#'
#'Functions used in the statisitcal analysis of daily values, such
#'as N-day duration statistics.\cr
#'\code{\link{dvStat}}\cr
#'\code{\link{freqAnal}}\cr
#'\code{\link{freqReport}}\cr
#'\code{\link{plot.freqAnal}}\cr
#'\code{\link{predict.freqAnal}}\cr
#'\code{\link{print.freqAnal}}\cr
#'
#'Functions for streamflow recession analysis.\cr
#'\code{\link{join}}\cr
#'\code{\link{recess}}\cr
#'\code{\link{rora}}\cr
#'\code{\link{aggregate.rora}}\cr
#'\code{\link{confirm.recess}}\cr
#'\code{\link{plot.recess}}\cr
#'\code{\link{plot.rora}}\cr
#'\code{\link{print.recess}}\cr
#'
#'Support and miscellaneous functions.\cr
#'\code{\link{antecedentRR}}\cr
#'\code{\link{extend}}\cr
#'\code{\link{hmean}}\cr
#'\code{\link{Lognormal3}}\cr
#'\code{\link{seasonYear}}\cr
#'\code{\link{seasonYearMD}}\cr
#'#'\code{\link{setSeasons}}\cr
#'
#' @name DVstats-package
#' @aliases DVstats-package DVstats
#' @docType package
#' @author Dave Lorenz 
#'
#' @seealso \code{\link[smwrData:smwrData-package]{smwrData}}
#' @references Need those for the various routines!
#' @keywords package
#' @import methods
#' @import lubridate
#' @import smwrBase
#' @import smwrGraphs
#' @import smwrStats
#' @import evd
#' @import stats
NULL
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and
                        is subject to revision. It is being provided to meet
                        the need for timely best science. The information
                        has not received final approval by the U.S. Geological
                        Survey (USGS) and is provided on the condition that
                        neither the USGS nor the U.S. Government shall be held
                        liable for any damages resulting from the authorized
                        or unauthorized use of the information.
                        
                        ****Orphaned Package****
                        This package is looking for a new maintainer. For more information, 
                        see: https://owi.usgs.gov/R/packages.html#orphan")
}