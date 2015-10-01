#' Antecedent Recession Rate
#'
#' Compute the antecedent recession rate valid for the end of a groundwater recession
#'identified by \code{fall}. Primarily used as a support function for \code{wtf}.
#'
#' @param x the output from \code{fall}.
#' @return An object of class "rise" and inherits class "data.frame" of the selected data, a data
#'frame of the recession information, and other information about the analysis.
#' @note 
#' The antecedent recession is modeled as log-linear recession, the projected recession is a
#'fixed fraction of the current recession. The fraction is based on the last 8 days of the recession
#'or extrapolated if the recession is less than 8 days and at least 4 days in length. If the recession
#'is less than 4 days in length, the previous recession rate is carried forward. For any rises
#'occuring before the first recession of at least 4 days, the recession rate is 0, which 
#'replicates the \code{rise} function.
#'
#' @keywords recession
#'@export
antecedentRR <- function(x) {
  FallR <- x$Recessions
  FallR.dec <- FallR.fr <- numeric(nrow(FallR))
  for(i in seq(nrow(FallR))) {
    # Get the last 8 days of the recession
    GWL <- x$Data[x$Data$Index == FallR$Index[i],]
    if(FallR$Length[i] >= 8) {
      Rec <- Rec.o <- GWL$GWLevel[seq(FallR$Length[i]-7L, FallR$Length[i])]
    } else {
      Rec <- Rec.o <- GWL$GWLevel
      # Extrapolate
      Rec <- approx(x=seq(1, 8, length.out=length(Rec)), y=Rec, xout=1:8)$y
    }
    # normalize to ending value = 0
    Rec <- Rec - Rec[8L]
    # And compute the fraction and rate
    FallR.fr[i] <- pmin(mean(Rec)/(Rec[1L]/2), 1) # maximum fraction is 1
    # Theoretical rate, given the observed recession
    Trate <- (Rec[1L]/(pmax(FallR$Length[i], 8) - 1))*(FallR.fr[i]^pmax(FallR$Length[i], 8))
    # The observed rate for the last 2 days
    Orate <- (Rec.o[length(Rec.o) - 2] - Rec.o[length(Rec.o)])/2
    # Pick the larger recession rate that does not exceed the average for the recession
    # This approach tries to find the best approximation to the tail end of
    # the recession to carry forward, by eliminating very large or very small tails
    FallR.dec[i] <- pmin(pmax(Trate, Orate), FallR$RRate[i])
  }
  return(cbind(FallR, InitDec=FallR.dec, DecRate=FallR.fr))
}
