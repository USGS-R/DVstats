#' Water Table Rises
#'
#' Identify hypothetical groundwater rises based on extrapolated antecedent 
#'recession rates in hydrographs of daily values.
#'
#' @param x the daily value data to be summarized. Missing values are not permitted
#'within the time specified by \code{Start} and \code{end}.
#' @param Dates the date for each \code{x}, should be of class "Date." Missing values
#'are not permitted.
#' @param Start the start date for the analysis, can be either a character string or
#'class "Date."
#' @param End the end date for the analysis, can be either a character string or
#'class "Date."
#' @param MPelev the measuring point elevation. Required if \code{x} is a depth below land
#'surface or measuring point.
#' @param STAID the station identifier for the data.
#' @return an object of class "rise" and inherits class "data.frame" of the selected data, a data
#'frame of the recession information, and other information about the analysis.
#' @note Healy and Cook (2002) descibe the water-table fluctuation method for estimating
#'recharge.
#'
#' The antecedent recession is modeled as log-linear recession, the projected recession is a
#'fixed fraction of the current recession. The fraction is based on the last 8 days of the recession
#'or extrapolated if the recession is less than 8 days and at least 4 days in length. If the recession
#'is less than 4 days in length, the previous recession rate is carried forward. For any rises
#'occuring before the first recession of at least 4 days, the recession rate is 0, which 
#'replicates the \code{rise} function.
#' @references Healy, R.W., and Cook, P.G., 2002, Using ground-water levels to estimate 
#'recharge. Hydrogeology Journal, v. 10, p. 91--109.
#'
#' @keywords recession
#' @examples
#'
#'\dontrun{
#'library(smwrData)
#'data(GlacialRidge)
#'with(GlacialRidge, wtf(G12, datetime, MPelev=1126.42, STAID="G12"))
#'}
#'@export
wtf <- function(x, Dates, Start=NULL, End=NULL, MPelev=NULL,
                 STAID="Unknown") {
  ## Preliminaries
  if(is.null(Start))
    Start <- Dates[1L]
  else if(is.character(Start))
    Start <- as.Date(Start)
  if(is.null(End))
     End <- Dates[length(Dates)]
  else if(is.character(End))
    End <- as.Date(End)
  sel <- (Dates >= Start) & (Dates <= End)
  Dates <- Dates[sel]
  GWlevel <- x[sel]
  if(any(is.na(x)))
    stop("Missing values between ", Start, " and ", End)
  if(any(diff(as.double(Dates)) != 1))
    stop("Date data are not continuous between start and end")
  if(!is.null(MPelev))
    GWlevel <- MPelev - GWlevel
  ## Compute the rise and fall
  retval <- rise(x=GWlevel, Dates=Dates, Start=Start, End=End, STAID=STAID)
  Fall <- fall(x=GWlevel,  Dates=Dates, Start=Start, End=End, min.duration=4, STAID=STAID)
  # As it happens, the recession rate is nearly the ratio between the area under
  # the actual recession and the area under straight-line recession for 8 days!
  ## Extract the recessions and process to get the decrease and the extinction
  ## rate (the fraction for subsequent days)
  FallR <- antecedentRR(Fall)
  ## Now step through the rise data, compute the recession and the hypothetical rise
  # Intial values for the decrement and rate
  Dec <- 0
  Fr <- 1
  FallR.next <- 1L
  # Check for recession beginning on first day
  if(retval$Dates[1L] == FallR$Date[FallR.next]) {
    Dec <- FallR$InitDec[FallR.next]
    Fr <- FallR$DecRate[FallR.next]
    FallR.next <- 2L
  }
  for(i in seq(2L, nrow(retval))) {
    # Check for updates to Dec and Fr
    if(retval$Dates[i] == FallR$Date[FallR.next]) {
      Dec <- FallR$InitDec[FallR.next]
      Fr <- FallR$DecRate[FallR.next]
      FallR.next <- pmin(FallR.next + 1L, nrow(FallR)) # protection! 
    }
    # check for rise and re-evaluate
    if(retval$EventRise[i] > 0L) {
      Dec <- Dec * Fr
      retval$HypoRecess[i] <- retval$HypoRecess[i-1L] - Dec
      retval$Rise[i] <- retval$Rise[i] + Dec
    }
  }
  return(retval)
}
