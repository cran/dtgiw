#' The cumulative distribution function (CDF) for Discrete Transmuted Generalized Inverse Weibull (DTGIW) distribution.
#'
#' This function calculated the CDF of the DTGIW distribution.
#'
#' The PMF of DTGIW distribution is shown in Theorem 1. based on the research paper in references.
#' For discrete random variables, the CDF of DTGIW distribution can be calculated by summation of the PMF.
#'
#'
#' @param q vector of quantiles.
#' @param alpha shape parameter#1.
#' @param beta scale parameter.
#' @param lambda shape pameter#2.
#' @param theta the transmuted parameter.
#' @param lower.tail logical; if TRUE (default), probabilities are Prob of X less than or equal to x. Otherwise, Prob of X greater than x.
#' @param log.p logical(TRUE or FALSE); if log.p=FALSE, then return the CDF; if log.p=TRUE, then return the natural logarithms of the CDF.
#'
#' @references  Atchanut Rattanalertnusorn and Sirinapa Aryuyuen (2021).
#' The zero-truncated discrete transmuted generalized inverse Weibull distribution and its applications,
#' Songklanakarin Journal of Science and Technology (SJST), Volume 43 No.4 (July - August 2021), pp. 1140 - 1151. DOI: 10.14456/sjst-psu.2021.149

#' @return the cdf of DTGIW distribution
#' @export
#'
#' @examples
#' x <- c(0:10)
#' pDTGIW(x,3.45,0.7,1.05,0)
#'
pDTGIW <- function(q,alpha,beta,lambda,theta,lower.tail=TRUE,log.p=FALSE){
  m<- length(q)
  if (m>=1){
    p <- c()
    for(j in 1:m){
      A <- exp(-lambda*((beta*q[j]+beta)^(-alpha)))
      p[j]<- A*(1+theta-theta*A)
    }
    if(lower.tail==TRUE & log.p==FALSE){
      return(p)
    }else if(lower.tail==TRUE & log.p==TRUE){
      return(log(p))
    }else if(lower.tail==FALSE & log.p==FALSE){
      return(1-p)
    }else if(lower.tail==FALSE & log.p==TRUE){
      return(log(1-p))
    }
  }else{
    errmsg <- c("Error in first argument, that is, NULL or empty quantile")
    return(errmsg)
  }
}#end pDTGIW
