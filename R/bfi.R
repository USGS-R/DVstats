#' Baseflow Separation
#'
#' Extract baseflow from a daily streamflow record using the method proposed by
#'the Institute of Hydrology (1980a,b).
#'
#'  The original description of constructing turning points proposed dividing
#'water years into 5-day increments and selecting the minimum flow for each period.
#'Gustard and others (1992) describe using continuous 5-day increments rather than restarting
#'on the water year.
#'
#'The \code{bfi} program developed by Wahl and Wahl available at 
#'\url{https://www.usbr.gov/pmts/hydraulics_lab/twahl/bfi/} uses the continuous record
#'for constructing the \code{N-day} periods. The argument \code{by} can select either
#'water year, continuous or calendar year records.
#'
#' @param Flow the daily streamflow to be separated missing values are not permitted
#'within the time specified by \code{Start} and \code{end}.
#' @param Dates the date for each \code{x}, should be of class "Date." Missing values
#'are not permitted.
#' @param Start the start date for the analysis, can be either a character string or
#'class "Date."
#' @param End the end date for the analysis, can be either a character string or
#'class "Date."
#' @param by string describing how to construct turning points. See
#'\bold{Details}.
#' @param f the factor for identifying turning points.
#' @param N the time perod for period for calculating the turning points.
#' @param STAID the station identifier for the data.
#' @references Gustard, A., Bullock, A., and Dixon, J.M., 1992,
#'Low flow estimation in the United Kingdom: Institue of Hydrology 
#'Report No. 108, 88 p. and appendixes.
#'
#'Institute of Hydrology, 1980, Low flow studies: 
#'Wallingford, Oxon, United Kingdom, Report No. I, 41 p.
#'
#'Institute of Hydrology, 1980, Low flow studies: Wallingford, Oxon, 
#'United Kingdom, Report No. 3, p. 12- 19.
#'
#' @return an object of class "baseflow" and inherits class "data.frame" of the selected data, a data
#'frame of the baseflow information, and other information about the analysis.
#' @keywords baseflow
#' @examples
#'
#'\dontrun{
#'library(smwrData)
#'data(ChoptankFlow)
#'# Process by calendar year as that is the retrieval range
#'ChopBFI <- with(ChoptankFlow, bfi(Flow, datetime, by="calendar year",
#'STAID="01491000"))
#'head(ChopBFI, 20)
#'}
#'@export
bfi <- function(Flow, Dates, Start=NULL, End=NULL, by="water year",
                f=0.9, N=5L, STAID="Unknown") {
  ## Start of code: initial processing
  STAID <- as.character(STAID[1L])
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
  Flow <- pmax(Flow[sel], 10^(-2.5)) # Convert 0 to a small number
  if(any(is.na(Flow)))
    stop("Missing values between ", Start, " and ", End)
  if(any(diff(as.double(Dates)) != 1))
    stop("Date data are not continuous between Start and End")
  by <- match.arg(by, c("water year", "calendar year", "continuous"))
  if(by == "calendar year") {
    ## Cut pts
    Cut <- c(seq(0, 360, by=N,), 366)
    ## Process data by years
    Yr <- year(Dates)
    dayno <- yday # from lubridate
  } else if(by == "water year") {
    Cut <- c(seq(0, 360, by=N,), 366)
    Yr <- waterYear(Dates, numeric=TRUE)
    dayno <- function(x) {
      Jul <- as.integer(x)
      lWY <- waterYear(x, numeric=TRUE) - 1L
      baseWY <- as.integer(as.Date(ISOdate(lWY, 9, 30)))
      return(Jul - baseWY)
    }
  } else { # for now continuous
    Cut <- c(seq(0, length(Flow) + N, by=N))
    Yr <- rep(1L, length(Flow))
    dayno <- function(x) seq(1L, by=1L, length.out=length(x))
  }
  DF <-  data.frame(Dates=Dates, Q=Flow)
  ret <- by(DF, Yr, function(DF) {
    Jul <- dayno(DF$Dates)
    Grp <- cut(Jul, Cut, labels=FALSE)
    retval <- tapply(DF$Q, Grp, function(x) {
      Min <- min(x)
      Wch <- which(Min == x)[1L]
      return(c(Min, Wch))
    }) # end of lapply
    retval <- do.call('rbind', retval)
    retval[,2L] <- retval[, 2L]+Cut[unique(Grp)]
    return(retval)}
    )
  ## Note: if the first year is a partial year, then need to subtract 
  ##  the [starting day number] from the pointer (ret[,2]) for that first 
  ##  year. The correction is applied secondarily.
  Yrtbl <- cumsum(c(0, table(Yr))) # The number to add to each pointer
  for(i in seq(length(ret)))
    ret[[i]][, 2L] <- ret[[i]][, 2L] + Yrtbl[i]
  Jstrt <- dayno(Dates[1L])
  if(Jstrt > 1L)
    ret[[1L]][, 2L] <- ret[[1L]][, 2L] - Jstrt + 1L
  ## The matrix ret are the minima and the index to the value
  ret <- do.call('rbind', ret)
  ## Now calculate the turning points
  TP <- rep(FALSE, nrow(ret))
  for(i in seq(2L, nrow(ret)-1L)) {
    if(ret[i, 1L] == 0) {
      TP[i] <- TRUE
    } else if(ret[i - 1L, 1L] == 0) {
      TP[i] <- f*ret[i, 1L] <= ret[i+1L, 1L]
    } else if(ret[i + 1L, 1L] == 0) {
      TP[i] <- f*ret[i, 1L] <= ret[i-1L, 1L]
    } else {
      TP[i] <- f*ret[i, 1L] <= min(ret[i-1L, 1L], ret[i+1L, 1L])
    }
  }
  TPdat <- ret[TP,]
  BaseQ <- rep(NA, length=length(Flow))
  for(i in seq(1L, nrow(TPdat)-1L)) {
    Rng <- seq(TPdat[i, 2L], TPdat[i+1L, 2L])
    if(TPdat[i, 1L] == 0 || TPdat[i+1L, 1L] == 0) { # Use linear interpolation
      BaseQ[Rng] <- pmin(Flow[Rng], seq(TPdat[i, 1L], TPdat[i+1L, 1L],
                                          length.out=TPdat[i+1L, 2L] - TPdat[i, 2L]+1L))
    } else 
      BaseQ[Rng] <- pmin(Flow[Rng], exp(seq(log(TPdat[i, 1L]), log(TPdat[i+1L, 1L]),
                                            length.out=TPdat[i+1L, 2L] - TPdat[i, 2L]+1L)))
  }
  TurnPt <- rep(" ", length(Flow))
  TurnPt[TPdat[,2]] <- "*"
  retval <- data.frame(Dates=Dates, BaseQ=BaseQ, Flow=Flow, TurnPt=TurnPt)
  if(!is.null(STAID))
    attr(retval, "STAID") <- STAID
  attr(retval, "type") <- "bfi"
  class(retval) <- c("baseflow", "data.frame")
  return(retval)
}
  
