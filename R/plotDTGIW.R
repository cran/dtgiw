#' Plot Discrete Transmuted Generalized Inverse Weibull(DTGIW) distribution.
#'
#' This function for the plot of DTGIW distribution.
#'
#' @param x a vector of quantile
#' @param fx  probability mass function
#' @param alpha shape parameter#1.
#' @param beta scale parameter.
#' @param lambda shape pameter#2.
#' @param theta the transmuted parameter.
#' @importFrom graphics plot points title
#'
#' @return the figure of DTGIW distribution
#' @export
#'
#' @examples
#' x <- c(0:10)
#' fx<- dDTGIW(x,3.45,0.7,1.05,0)
#' plotDTGIW(x,fx,alpha=3.45,beta=0.7,lambda=1.05,theta=0)
#'
plotDTGIW <- function(x,fx,alpha=3.45,beta=0.7,lambda=1.05,theta=0){
 plot(x,fx,type="h",xlab="", ylab="",
     main=expression(paste((a)(alpha,beta,lambda,theta),"=(3.45,0.7,1.05,0)")),
     col="blue",pch=16,lwd=5,ylim=c(0,1))
 points(x,y=fx,col="black",lwd=5)
 title(xlab="x",ylab="p(x)")
}

