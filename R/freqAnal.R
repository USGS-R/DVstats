#' Frequency Analysis
#' 
#' Compute an extreme-value frequency analysis. Three distributions are fit:
#'log-Pearson type III, 3-parameter log-normal, and the log-generalized
#'extreme value. The analyses are used primarily for streamflow data.
#'
#' Zero values are removed from \code{x} and the statistics are computed
#'on the conditional adjustment for the zero values. If there are fewer than
#'six non-zero values or all non-zero values are co-equal, then no 
#'distributions are fit and the empirical distribution is used. 
#'Any value in \code{x} less than zero generates an error.
#'
#' The choice for \code{method} must be one of "Auto," "lp3," "ln3,"
#'"lgev," or "PPCC" only the first two characters are required.
#'The methods "lp3," "ln3," and "lgev" select the specific frequency 
#'analysis for the method to use. The method "PPCC" selects 
#'the maximum PPCC to determine the method. The default method, "Auto,"
#'Preferentially selects the log-Pearson type III method if the PPCC of
#'that method is greater than 0.985 and if that fails, selects the 
#'3-parametter log-normal if the PPCC of that method is greater than 
#'0.985 and if that fails then the maximum PPCC is used to select the method.
#'
#' @param x the observed data. Missing values are removed before computing the
#'descriptive statistics. See \bold{Details}.
#' @param id an optional identifier for each value in \code{x}. If 
#'supplied, must match length of \code{x}.
#' @param desc a description of the statistic.
#' @param STAID the station identifier
#' @param method a character string indicating the method to use
#'for computing the final estimates. See \bold{Details}.
#' @return An object of class "freqAnal" that can be used to compute specific 
#'estimates.
#' @note The 3-parameter lognormal and log-generalized extreme value will
#'occasionally fail to fit the observed data and no estimate can be made.
#'If either of these are selected by the user, then the selected method 
#'will default to the log-Pearson type III method.
#'
#'The log-Pearson type III method will occasionally fail to fit the observed
#'data, but an estimate can be made. No warning is created, but the printed
#'report indicates that observed values exceed the range of the distribution.
#'
#'The 3-parameter lognormal can estimate values less than zero if \code{lambda}
#'is less than zero. No warning is created, but the printed
#'report will include a note if estimated values correponding to observed
#'values are less than 0. Any predicted values less than zero are converted to 
#'zero.
#'
#' @examples
#'\dontrun{
#'library(USGSwsData)
#'data(ConecuhFlows)
#'with(ConecuhFlows, freqAnal(Flow, Year, desc="Annual Flows", 
#'  STAID="02371500"))
#'}
#' @importFrom stats4 mle
#' @export
freqAnal <- function(x, id, desc="", STAID="Unknown", method="Auto") {
  ## Coding history:
  ##    2013Aug01 DLLorenz Initial coding.
  ##
  ## Functions needed for MLE of 3-param normal:
  ln3.fit <- function(x, mu, sig, lam) {
    sig=exp(sig)
    lam=min(x) - exp(lam) 
    likes <- log(dln3(x, mu, sig, lam))
    return(-sum(likes))
  }
  
  ln3.mle <- function(x) {
    lx <- log(x)
    mu <- mean(lx)
    sd <- sd(lx)
    fit <- mle(ln3.fit, start=list(mu=mu, sig=log(sd), lam=log(min(x))),
               fixed=list(x=x), nobs=length(x))@details
    if(fit$convergence != 0)
      return(fit)
    fit$par[2L] <- exp(fit$par[2L])
    fit$par[3L] <- min(x) - exp(fit$par[3L])
    return(fit)
  }
  ##
  ## Remove any NAs
  if(!missing(id))
    names(x) <- as.character(id)
  x <- x[!is.na(x)]
  if(any(x < 0))
    stop("Values less than 0 are not allowed in x")
  ## Set method
  method <- match.arg(method, c("Auto", "lp3", "ln3",
                                "lgev", "PPCC"))
  ## Process 0 values
  N0 <- sum(x == 0)
  x <- x[x > 0]
  mnx <- mean(x)
  sdx <- sd(x)
  logx <- log(x)
  Ngt0 <- length(x)
  retval <- list(x=sort(x), mnx=mnx, sdx=sdx,N0=N0, desc=desc, 
                 STAID=STAID, method="emp")
  class(retval) <- "freqAnal"
  if(Ngt0 < 6L) {
    warning("Must have at least 6 non zero values")
    return(retval)
  }
  if(sdx < 1e-9) {
    warning("Variance of non-zero values is zero")
    return(retval)
  }
  ## Log-Pearson type III:
  ## Compute the basic stats
  mnlx <- mean(logx)
  sdlx <- sd(logx)
  glx <- skew(logx)
  ## Compute the logLikelihood
  LL <- sum(log(dpearsonIII(logx, mnlx, sdlx, glx)))
  attr(LL, "df") <- 3L
  attr(LL, "nobs") <- Ngt0
  class(LL) <- "logLik"
  ## Compute the Prob Plot Cor Coef
  Ex <- qlpearsonIII(ppoints(Ngt0, 0.44), mnlx, sdlx, glx)
  PPCC <- cor(sort(logx), log(Ex))
  # Pack it up
  retval$lp3 <- list(mnlx=mnlx, sdlx=sdlx, glx=glx, logLik=LL, 
              PPCC=PPCC, Ex=Ex)
  # 3-parameter log-normal:
  # Estimate the parameters
  fit <- try(ln3.mle(x), silent=TRUE)
  if(class(fit) != "try-error" && fit$convergence == 0) {
    mnlx <-fit$par[1L]
    sdlx <- fit$par[2L]
    lamlx <- fit$par[3L]
    # The log-likelihood
    LL <- -fit$value
    attr(LL, "df") <- 3L
    attr(LL, "nobs") <- Ngt0
    class(LL) <- "logLik"
    ## Compute the Prob Plot Cor Coef
    Ex <- qln3(ppoints(Ngt0, 0.44), mnlx, sdlx, lamlx)
    ## Eliminate the possible estimation of negative values, caused
    ## by large negative lambda
    if(all(Ex > 0)) {
      PPCC <- cor(sort(logx), log(Ex))
    } else {
      PPCC <- 0
      Ex[Ex <= 0] <- NA_real_
    }
      retval$ln3 <- list(mnlx=mnlx, sdlx=sdlx, lamlx=lamlx, logLik=LL, 
                         PPCC=PPCC, Ex=Ex)
  }
  ## The log-gev
  # Estimate the parameters
  fit <- try(fgev(logx), silent=TRUE)
  if(class(fit) != "try-error" && fit$convergence == "successful") {
    loclx <- fit$estimate[1L]
    sclx <- fit$estimate[2L]
    shlx <- fit$estimate[3L]
    # The log-likelihood
    LL <- fit$deviance/-2
    attr(LL, "df") <- 3L
    attr(LL, "nobs") <- Ngt0
    class(LL) <- "logLik"
    ## Compute the Prob Plot Cor Coef
    Ex <- exp(qgev(ppoints(Ngt0, 0.44), loclx, sclx, shlx))
    PPCC <- cor(sort(logx), log(Ex))
    retval$lgev <- list(loclx=loclx, sclx=sclx, shlx=shlx, logLik=LL, 
                        PPCC=PPCC, Ex=Ex)
  }
  ## Pick the method
  if(method == "Auto") {
    if(retval$lp3$PPCC > 0.985) {
      method ="lp3"
    } else if(!is.null(retval$ln3) && retval$ln3$PPCC > 0.985) {
      method ="ln3"
    } else
      method="PPCC"
  }
  if(method == "PPCC") {
    method <- "lp3"
    current <- retval$lp3$PPCC
    if(!is.null(new <- retval$ln3$PPCC)){
      if(new > current) {
        current <- new
        method <- "ln3"
      }
    }
    if(!is.null(new <- retval$lgev$PPCC)){
      if(new > current)
        method <- "lgev"
    }
  }
  ## Check to make sure its valid
  if(is.null(retval[[method]])) {
    warning("The ", method, " is not valid, using lp3.")
    method <- "lp3"
  }
  retval$method <- method
  return(retval)
}
