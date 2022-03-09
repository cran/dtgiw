#' The random generating function for Discrete Transmuted Generalized Inverse Weibull(DTGIW) distribution.
#'
#' This function generates random numbers for the DTGIW distribution.
#'
#' The R script generates the n random values of the DTGIW distribution is shown based on the research paper in references.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param alpha shape parameter#1.
#' @param beta scale parameter.
#' @param lambda shape pameter#2.
#' @param theta the transmuted parameter.
#'
#' @references Atchanut Rattanalertnusorn and Sirinapa Aryuyuen (2021).
#' The zero-truncated discrete transmuted generalized inverse Weibull distribution and its applications,
#' Songklanakarin Journal of Science and Technology (SJST), Volume 43 No.4 (July - August 2021), pp. 1140 - 1151. DOI: 10.14456/sjst-psu.2021.149.
#'
#' @return  the n random number of DTGIW distribution.
#' @export
#'
#' @importFrom stats runif
#'
#' @examples
#' rDTGIW(n=100,3.45,0.7,1.05,0)
#'
rDTGIW<-function(n,alpha,beta,lambda,theta){
  x<-numeric()
  u<-runif(n)
  x<-qDTGIW(u,alpha,beta,lambda,theta)
  return(x)
}
