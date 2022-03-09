#' The probability mass function (PMF) for Discrete Transmuted Generalized Inverse Weibull (DTGIW) distribution.
#'
#' This function calculated the PMF of the DTGIW distribution.
#'
#' The PMF of the DTGIW distribution is shown in Theorem 1 based on the research paper in references.
#'
#' @param x vector of quantiles.
#' @param alpha shape parameter#1.
#' @param beta  scale parameter.
#' @param lambda shape pameter#2.
#' @param theta the transmuted parameter.
#' @param log logical(TRUE or FALSE); if log=FALSE, then return the PMF; if log=TRUE, then return the natural logarithms of the PMF.

#' @references  Atchanut Rattanalertnusorn and Sirinapa Aryuyuen (2021).
#' The zero-truncated discrete transmuted generalized inverse Weibull distribution and its applications,
#' Songklanakarin Journal of Science and Technology (SJST), Volume 43 No.4 (July - August 2021), pp. 1140 - 1151. DOI: 10.14456/sjst-psu.2021.149
#'
#' @return the PMF of DTGIW distribution
#' @export
#'
#' @examples
#' x <- c(0:10)
#' dDTGIW(x,3.45,0.7,1.05,0)
dDTGIW <- function(x,alpha,beta,lambda,theta,log=FALSE){
  m<- length(x)
  if (m>=1){
    p <- c()
    for(j in 1:m){
      A <- exp(-lambda*((beta*x[j]+beta)^(-alpha)))
      B <- exp(-lambda*((beta*x[j])^(-alpha)))
      C <- exp(-2*lambda*((beta*x[j]+beta)^(-alpha)))
      D <- exp(-2*lambda*((beta*x[j])^(-alpha)))
      p[j]<- (1+theta)*(A-B)-theta*(C-D)
    }
    if(log==FALSE){
      return(p)
    }else{
      return(log(p))
    }

  }else{
    errmsg <- c("Error in first argument x, that is NULL or empty quantile")
    return(errmsg)
  }
}#end dDTGIW
