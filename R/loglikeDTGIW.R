#' Negative Log-Likelihood value of DTGIW distribution.
#'
#' The function for calculating negative log-likelihood value of DTGIW distribution.

#' @param x a vector of quantile
#' @param alpha shape parameter#1
#' @param beta scale parameter
#' @param lambda shape pameter#2
#' @param theta the transmuted parameter
#'
#' @return the negative log-likelihood value of DTGIW distribution
#' @export
#'
#' @references Atchanut Rattanalertnusorn and Sirinapa Aryuyuen (2021).
#' The zero-truncated discrete transmuted generalized inverse Weibull distribution and its applications,
#' Songklanakarin Journal of Science and Technology (SJST), Volume 43 No.4 (July - August 2021), pp. 1140 - 1151 <DOI: 10.14456/sjst-psu.2021.149>.

#' @examples
#' x <- rDTGIW(n=20,3.45,0.7,1.05,0)
#' loglikeDTGIW(x,3.45,0.7,1.05,0)
loglikeDTGIW <- function(x,alpha,beta,lambda,theta){
  ngloglike<- -sum(dDTGIW(x,alpha,beta,lambda,theta,log=TRUE))
  return(ngloglike)
}
