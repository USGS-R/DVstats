#'Subset an Object
#'
#'Extracts or subsets an object of class "fall." 
#'
#'The values for \code{i} can be either a single numeric index or a single 
#'character string. If \code{i} is numeric, then extract that element number.
#'If \code{i} is cahracter, then extract that Index number.
#'
#' @rdname subset.fall
#' @param x an object of class "fall." 
#' @param i index specifying the recession to extract. See \bold{Details}.
#' @param \dots not used, required for other methods.
#' @return A data frame of the specified recession containing these columns:\cr
#'\describe{
#'\item{Dates}{The date of the observation}
#'\item{GWLeve}{The ground water level value}
#'\item{Index}{The recession index number}
#'\item{Seqno}{The sequence number for each day of the recession}
#'}
#'Note that the sequence number of the recession begins at 0 becuase that value
#'represents the peak and the recession starts the following day.
#' @seealso \code{\link{fall}}
#' @keywords manip
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(GlacialRidge)
#'# Select the first recession of at least 10 days
#'with(GlacialRidge, fall(G12, datetime, MPelev=1126.42))[1]
#'}
#' @export
#' @method [ fall
"[.fall" <- function(x, i, ...) {
  ## x is a fall object
  ## 
  Data <- x$Data
  if(is.numeric(i)) { # convert to the correct value
    i <- x$Recessions$Index[i]
  } else {
    i <- as.numeric(i)
  }
  Data <- Data[Data$Index == i,] 
  Data$Seqno <- eventSeq(Data$Index) - 1
  return(Data)
}
