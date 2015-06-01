#' Print an Object
#' 
#' Prints the results of a frequency analysis.
#'
#' @param x an object of class "freqAnal."
#' @param digits the number of digits to print for numeric values.
#' @param common.logs report the log statistics in terms of common logs?
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' 
#' @method print freqAnal
#' @export
print.freqAnal <- function(x, digits=4, common.logs=TRUE, ...) {
  ## Coding history:
  ##    2013Aug01 DLLorenz Initial coding.
  ##
  cat("Frequency analysis for ", x$STAID, "\n", sep="")
  if(x$desc != "") {
    cat("Statistics for ", x$desc, "\n", sep="")
  }
  if((Ngt0 <- length(x$x)) == 0L) {
    cat("All data are zero values, no analysis possible\n")
  } else if(Ngt0 < 6L) {
    cat("Fewer than 6 non-zero values, all estimates based on the observed data\n")
    print(x$x, digits=digits)
  } else {
    cat("\nDescriptive statistics based on ", Ngt0, " non-zero values\n", sep="")
    print(x$x, digits=digits)
    cat("     Sample mean: ", signif(x$mnx, digits),
        "\nSample std. dev.: ", signif(x$sdx, digits), "\n\n", sep="")
  }
  if(is.null(x$lp3)) {
    cat("    Log-Pearson type III statistics could not be computed.\n\n")
  } else { # Print the results
    cat("    Log-Pearson type III analysis.\n")
    if(common.logs) {
      cf <-  1/log(10)
      cat("              Mean common logs = ", signif(x$lp3$mnlx*cf, digits),
          "\nStandard deviation common logs = ", signif(x$lp3$sdlx*cf, digits),
          "\n          Skewness common logs = ", signif(x$lp3$glx, digits), "\n", 
          sep="")
    } else
      cat("              Mean natural logs = ", signif(x$lp3$mnlx, digits),
          "\nStandard deviation natural logs = ", signif(x$lp3$sdlx, digits),
          "\n          Skewness natural logs = ", signif(x$lp3$glx, digits), "\n", 
          sep="")
    if(!is.na(x$lp3$logLik)) {
      if(x$lp3$logLik == -Inf) {
        if(x$lp3$glx > 0) {
          cat("At least one observed value less than the lower end of distribution.\n")
        } else if(x$lp3$glx < 0)
          cat("At least one observed value greater than the upper end of distribution.\n")
      }
    }
    cat("PPCC:", signif(x$lp3$PPCC, digits),
        "\nEstimated data:\n     Mean: ", signif(mean(x$lp3$Ex), digits),
        "\nStd. Dev.: ", signif(sd(x$lp3$Ex), digits), "\n\n", sep="")
  }
  if(is.null(x$ln3)) {
    cat("    3-parameter Log-normal statistics could not be computed.\n\n")
  } else { # Print the results
    cat("    3-parameter Log-normal analysis.\n")
    if(common.logs) {
      cf <-  1/log(10)
      cat("              Mean common logs = ", signif(x$ln3$mnlx*cf, digits),
          "\nStandard deviation common logs = ", signif(x$ln3$sdlx*cf, digits),
          "\n               Offset (lambda) = ", signif(x$ln3$lamlx, digits), "\n", 
          sep="")
    } else
      cat("              Mean natural logs = ", signif(x$ln3$mnlx, digits),
          "\nStandard deviation natural logs = ", signif(x$ln3$sdlx, digits),
          "\n               Offset (lambda) = ", signif(x$ln3$lamlx, digits), "\n", 
          sep="")
    if(x$ln3$PPCC == 0) {
      cat("At least one estimated value less than zero, fitted statistics not printed.\n\n")
    } else
    cat("PPCC:", signif(x$ln3$PPCC, digits),
        "\nEstimated data:\n     Mean: ", signif(mean(x$ln3$Ex), digits),
        "\nStd. Dev.: ", signif(sd(x$ln3$Ex), digits), "\n\n", sep="")
  }
  if(is.null(x$lgev)) {
    cat("    Log-generalized extreme value statistics could not be computed.\n\n")
  } else { # Print the results
    cat("    Log-generalized extreme value analysis.\n")
      cat("Location = ", signif(x$lgev$loclx, digits),
          "\n   Scale = ", signif(x$lgev$sclx, digits),
          "\n   Shape = ", signif(x$lgev$shlx, digits), "\n", 
          sep="")
    cat("PPCC:", signif(x$lgev$PPCC, digits),
        "\nEstimated data:\n     Mean: ", signif(mean(x$lgev$Ex), digits),
        "\nStd. Dev.: ", signif(sd(x$lgev$Ex), digits), "\n\n", sep="")
  }
  method <- c(emp="emprical", lp3="log-Pearson type III",
              ln3="3-parameter log-normal", 
              lgev="log-generalized extreme value")
  cat("The frequency analysis estimates will be made using the ",
      method[x$method], " method.\n", sep="")
  if(x$N0 > 1L) {
    cat("The frequency analysis estimates will be conditionally ajusted for ",
        x$N0, " zero values.\n\n", sep="")
  } else if(x$N0 > 0L) {
    cat("The frequency analysis estimates will be conditionally ajusted for 1 zero value.\n\n", sep="")
  } else
    cat("No zero values in these data, no conditional adjustment needed.\n\n")
  
  invisible(x)
}
