#' Create Frequency Analysis Report
#' 
#' Create a 2-page pdf file report of a frequency analysis. The 
#'report contains the text output from \code{print(x)}, the esitmated
#'values of selected probailities of exceedance, and a diagnostic plot
#'for each fitted distribution.
#'
#' @param x and object of class "freqAnal."
#' @param file the output file base name; the .pdf suffix 
#'is appended to make the actual file name. if missing, then the
#'name of \code{x} is used as the base name.
#' @return The actual file name is returned invisibly.
#' @export
freqReport <- function(x, file) {
  ## Coding history:
  ##    2013Aug02 DLLorenz Original
  ##
  if(missing(file))
    file <- deparse(substitute(x))
  retval <- setPDF(basename=file)
  plot.new()
  ## Draw the text
  par(mar=c(0,0,0,0), usr=c(0,1,0,1))
  txt <- capture.output(x)
  ## Append predicted values
  txt <- c(txt, "\nEstimated values for selected probabilities of exceedance",
           capture.output(predict(x)))
  text(0, 1, paste(txt, collapse="\n"), family="mono", adj=c(0,1))
  ## 3 diagnostic plots
  if(x$method != "emp") {
    AA.lo <- setLayout(num.cols=1L, num.rows=3L)
    anals <- c("lp3", "ln3", "lgev")
    method <- c(lp3="log-Pearson type III",
                ln3="3-parameter log-normal", 
                lgev="log-generalized extreme value")
    for(i in seq(3)) {
      setGraph(i, AA.lo)
      if(!is.null(x[[anals[i]]])) {
        xest <- x[[anals[i]]]$Ex
        meth <- paste("Estimated values using", method[anals[i]])
        xyPlot(xest, x$x, xtitle=meth, ytitle="Observed values",
               yaxis.log=TRUE, xaxis.log=TRUE)
        refLine(coefficients=c(0,1))
        if(anals[i] == x$method)
          addTitle("  Selected Method")
      }
    }
  }
  ## All done, close the graph
  dev.off(retval[[1]])
  invisible(paste(retval[[2]], ".pdf", sep=""))
}
