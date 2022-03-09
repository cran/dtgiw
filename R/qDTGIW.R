#' The quantile function for Discrete Transmuted Generalized Inverse Weibull (DTGIW) distribution.
#'
#' This function calculated the quantile values of the DTGIW distribution.
#'
#' The R script calculated the quantile values of the DTGIW distribution is shown based on the research paper in references.
#'
#' @param p vector of probabilities
#' @param alpha shape parameter#1.
#' @param beta scale parameter.
#' @param lambda shape pameter#2.
#' @param theta the transmuted parameter.
#' @param lower.tail logical; if TRUE (default), probabilities are Prob of X less than or equal to x. Otherwise, Prob of X greater than x.
#' @param log.p logical(TRUE or FALSE); if log.p=FALSE, then return the cdf; if log.p=TRUE, then return the natural logarithms of the cdf.
#'
#' @references Atchanut Rattanalertnusorn and Sirinapa Aryuyuen (2021).
#' The zero-truncated discrete transmuted generalized inverse Weibull distribution and its applications,
#' Songklanakarin Journal of Science and Technology (SJST), Volume 43 No.4 (July - August 2021), pp. 1140 - 1151. DOI: 10.14456/sjst-psu.2021.149.
#'
#' @return the quantile values of DTGIW distribution
#' @export
#'
#' @examples
#' x <- c(0:10)
#' p<- pDTGIW(x,3.45,0.7,1.05,0)
#' qDTGIW(p,3.45,0.7,1.05,0)
#'
qDTGIW <- function(p,alpha,beta,lambda,theta,lower.tail = TRUE, log.p = FALSE){
  n<-length(p)
  x<-numeric(n)
  for (i in 1:n){
    k<-0
    if(p[i]>pDTGIW(k,alpha,beta,lambda,theta)){
      while( p[i]>pDTGIW(k,alpha,beta,lambda,theta)) #cdf of DTGIW
      {
        k<-k+1
      }
    }
    x[i]<-k
  }
  #return(x)
  if(lower.tail==TRUE & log.p==FALSE){
    return(x)
  }else if(lower.tail==TRUE & log.p==TRUE){
    return(log(x))
  }else if(lower.tail==FALSE & log.p==FALSE){
    return(x)
  }else if(lower.tail==FALSE & log.p==TRUE){
    return(log(x))
  }
}  #end qDTGIW
