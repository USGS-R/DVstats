#'Functions for manipulating daily values
#'
#'This package has specialized functions for managing or
#'manipulating daily hydrologic data.
#'
#'\tabular{ll}{ Package: \tab DVstats\cr 
#'Type: \tab Package\cr 
#'Version: \tab 0.1\cr 
#'Date: \tab 2013-06-13\cr 
#'License: \tab File LICENSE\cr 
#'Depends: \tab lubridate, USGSwsBase, USGSwsGraphs, 
#' USGSwsStats, robust\cr } 
#'This package contains functions that manage or manipipulate daily 
#'hydrologic data.
#'
#'@name DVstats-package
#'@aliases DVstats-package DVstats
#'@docType package
#'@author Dave Lorenz <lorenz@@usgs.gov>
#'
#'Maintainer: Dave Lorenz <lorenz@@usgs.gov>
#'@seealso \code{\link[USGSwsData:USGSwsData-package]{USGSwsData}}
#'@references Lorenz, D.L., in preparation, DVstats---an R package for managing
#'daily hydrologic data, version 0.1
#'@keywords package
setGeneric("confirm", USGSwsStats::confirm)
