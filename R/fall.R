#' Recessions
#'
#' Identify groundwater recessions
#'
#' @param x the daily value data to be summarized. Missing values are not permitted
#'within the time specified by \code{Start} and \code{End}.
#' @param Dates the date for each \code{x}, should be of class "Date." Missing values
#'are not permitted.
#' @param Start the start date for the analysis, can be either a character string or
#'class "Date."
#' @param End the end date for the analysis, can be either a character string or
#'class "Date."
#' @param by the months to search for recessions, can be either month numbers or abbreviations.
#'The default is use all months.
#' @param MPelev the measuring point elevation. Required if \code{x} is a depth below land
#'surface or other measuring point.
#' @param min.duration the minimum length for a recession to be included in the output.
#' @param STAID the station identifier for the data.
#' @return an object of class "fall" containing a data frame of the selected data, a data
#'frame of the recession information, and other information about the analysis.
#' @keywords recession
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(GlacialRidge)
#'with(GlacialRidge, fall(G12, datetime, MPelev=1126.42))
#'}
#' @export
fall <- function(x, Dates, Start=NULL, End=NULL, by=NULL, MPelev=NULL,
                 min.duration=10, STAID="Unknown") {
  ##
  ## Internal functions needed for this routine:
  eventSel <- function(x, seqno, minseq=10) {
    ## Select the starting and ending value of sequences longer than a
    ## specified minimum
    NR=length(x)
    if(NR < minseq) 
      return(NULL)		
    if(seqno[1] == 0)
      return(NULL)
    return(c(Start=x[1L], End=x[NR], Length=NR))
  }
  recessSel <- function(x, recess, duration) {
    x2 <- seq(along=x)
    x2 <- tapply(x2, recess, function(y, x, rec, dur)
      eventSel(x[y], rec[y], dur), x=x, rec=recess, dur=duration)
    x2.recno <- as.integer(names(x2)[!sapply(x2, is.null)])
    if(length(x2.recno) == 0L)
      stop("No recessions detected")
    x2 <- as.data.frame(do.call(rbind, x2))
    names(x2) <- c("StartWL", "EndWL", "Length")
    x2$Index <- x2.recno
    return(x2)
  }
  ## start of code: initial processing
  STAID <- as.character(STAID[1L])
  if(is.null(by))
    months2Sel <- month.abb
  if(is.numeric(by))
    months2Sel <- month.abb[by]
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
  x <- x[sel]
  if(any(is.na(x)))
    stop("Missing values between ", Start, " and ", End)
  if(any(diff(as.double(Dates)) != 1))
    stop("Date data are not continuous between start and end")
  if(!is.null(MPelev))
    x <- MPelev - x
  ## compute the Recession index, must include point 0 for fall
  Recess <- eventNum(c(NA, diff(x)) <= 0 & c(NA, diff(as.double(Dates))) == 1.0, reset=TRUE)
  Recess <- pmax(Recess, na2miss(shiftData(Recess, -1), 0))
  Sel <- recessSel(x, Recess, duration=min.duration + 1) # account for point 0
  ## add first estimates of recession indexes
  Sel$Length <- Sel$Length - 1 # subtract for point 0 (peak)
  Index <- Sel$Index
  Len <- Sel$Length
  Date <- as.Date(rep(0, length(Index)), origin=as.Date("1970-01-01"))
  RRate <- double(length(Index))
  keep <- rep(FALSE, length(Index))
  ## From basic structure of recessions assume that only contiguous points used
  for(i in seq(along=Index)) {
    xsel <- x[Recess == Index[i]]
    Date[i] <- Dates[Recess == Index[i]][2L] # first day of recession
    ## Test if month in selected months and x not all equal
    if(months(Date[i], abbreviate=TRUE) %in% months2Sel && diff(range(xsel)) > 0.001) { # keep it
      keep[i] <- TRUE
      RRate[i] <- diff(range(xsel))/(Len[i] - 1)
    }
  }
  Recessions <- cbind(Sel, Date=Date, RRate=RRate)[keep,]
  row.names(Recessions) <- as.character(seq(nrow(Recessions)))
  retval <- list(Data=data.frame(Dates=Dates, GWLevel=x, Index=Recess),
                 Recessions=Recessions, Start=Start, End=End, min.duration=min.duration,
                 STAID=STAID, months2Sel=months2Sel)
  attr(retval, "Confirmed") <- FALSE
  oldClass(retval) <- "fall"
  return(retval)
}
