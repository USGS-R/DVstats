#' Estimate recharge
#'
#' Estiamte recharge by the method of hydrograph recession displacement.
#' 
#' @param Dates the date of the flow, can be ewither character or class "Date."
#' @param Flow the mean daily flow for the corresponding date.
#' @param Start the starting year for the analysis. If \code{NULL}, then
#'begin with the first year in \code{Dates}.
#' @param End the ending year for the analysis. If \code{NULL}, then
#'end with the last year in \code{Dates}.
#' @param da the drainage area of the basin.
#' @param recessIndex the recession index, in days per common log cycle. 
#'This is typcially estimated using \code{recess}.
#' @param minQ the value to use for the minimum value in \code{flow}. Any
#'value in \code{Flow} less than the value for \code{minQ} is set to the
#'value for \code{minQ}.
#' @param incAnteRec a value to add to the base antecedent recession time, 
#'in days. In general, this should always be 0. 
#' @param STAID a character string to be used as the station identifier. This is
#'used only for documentation when printing or plotting.
#' @return An object of class rora, which has these components.\cr
#'iyr, the year of the observed streamflow. \cr
#'imon, the month of the observed streamflow. \cr
#'idy, the day of the observed streamflow. \cr
#'flow, the observed streamflow. \cr
#'Nobs, the number of observed values of streamflow. \cr
#'iyearst, the starting year of the recharge analysis. \cr
#'iyearen, the ending year of the recharge analysis. \cr
#'minQ, the value of minQ. \cr
#'idiff, the value of incAnteRec. \cr
#'k, the value of recessIndex. \cr
#'te, the time to end day of the recession following a peak in streamflow, in days. \cr
#'ta, the time at the critical time after the previous peak. \cr
#'qp, the streamflow of the peak. \cr
#'qa, the streamflow the critical time after the previous peak. \cr
#'qb, the streamflow at the critical time that would have occurred in the 
#'absence of the current and any subsequent peaks. \cr
#'qc, the streamflow at the critical time that would have occurred 
#'in the absence of any subsequent peaks. \cr
#'c, the average value for the current peak calculated from the difference 
#'between the flow during recession and flow computed from the recession 
#'index for each day between the peak and the critical time. \cr
#'delq, the difference in flow between the hypothetical flow at the 
#'critical time after the current peak and the hypothetical flow at 
#'the critical time after the previous peak. \cr
#'rech, the estimated recharge for the peak, in inches. \cr
#'year, the year of the peak. \cr
#'mon, the month of the peak. \cr
#'day, the day of the peak. \cr
#'npeaks, the number of peaks. \cr
#'itbase, the antecedent flow base time, in days. This is the minimum time 
#'from a peak to when the flow can be considered to be ground-water discharge.\cr
#'ierr, the error code. \cr
#'STAID, the station identifier. 
#'
#' @useDynLib DVstats roraf
#' @export
rora <- function(Dates, Flow, Start=NULL, End=NULL, da, recessIndex,
                 minQ=0.01, incAnteRec=0, STAID=NULL) {
  rmnas <- !is.na(Flow)
  Flow <- Flow[rmnas]
  Dates <- Dates[rmnas]
  iyr <- as.POSIXlt(Dates)
  imon <- iyr$mon + 1L
  idy <- iyr$mday
  iyr <- iyr$year + 1900L
  N <- length(iyr)
  if(is.null(Start))
    Start <- min(iyr)
  if(is.null(End))
    End <- max(iyr)
  retval <- .Fortran("roraf",
                     iyr=as.integer(iyr),
                     imon=as.integer(imon),
                     idy=as.integer(idy),
                     flow=as.double(Flow),
                     Nobs=as.integer(N),
                     da=as.double(da),
                     iyearst=as.integer(Start),
                     iyearen=as.integer(End),
                     minQ=as.double(minQ),
                     idiff=as.integer(incAnteRec),
                     k=as.double(recessIndex),
                     te=integer(N),
                     ta=numeric(N),
                     qp=numeric(N),
                     qa=numeric(N),
                     qb=numeric(N),
                     qc=numeric(N),
                     c=numeric(N),
                     delq=numeric(N),
                     rech=numeric(N),
                     year=integer(N),
                     mon=integer(N),
                     day=integer(N),
                     npeaks=integer(1),
                     itbase=integer(1),
                     ierr=integer(1))
  if(retval$ierr < 0)
    warning(paste("Error code from rora: ", retval$ierr))
  if(retval$ierr > 0)
    stop(paste("Error code from rora: ", retval$ierr))
  Npeaks <- retval$npeaks
  length(retval$te) <- Npeaks
  length(retval$ta) <- Npeaks
  length(retval$qp) <- Npeaks
  length(retval$qa) <- Npeaks
  length(retval$qb) <- Npeaks
  length(retval$qc) <- Npeaks
  length(retval$c) <- Npeaks
  length(retval$delq) <- Npeaks
  length(retval$rech) <- Npeaks
  length(retval$year) <- Npeaks
  length(retval$mon) <- Npeaks
  length(retval$day) <- Npeaks
  if(!is.null(STAID))
    retval$STAID <- STAID
  oldClass(retval) <- "rora"
  return(retval)
}
