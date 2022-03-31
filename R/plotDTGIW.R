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
#' @importFrom graphics plot points title legend
#'
#' @return the figure of DTGIW distribution
#' @export
#'
#' @examples
#' x <- c(0:10)
#' fx<- dDTGIW(x,3.45,0.7,1.05,0)
#' plotDTGIW(x,fx,alpha=3.45,beta=0.7,lambda=1.05,theta=0)
#' fx2 <- dDTGIW(x,2.50,0.5,1.00,0)
#' plotDTGIW(x,fx2,alpha=2.50,beta=0.5,lambda=1.00,theta=0)
#'
plotDTGIW <- function(x,fx,alpha=3.45,beta=0.7,lambda=1.05,theta=0){
  a<- alpha; b<- beta; l<-lambda; t<- theta;
  parameters <- c(a,b,l,t)
  plot(x,fx,type="h",xlab="", ylab="",col="blue",pch=16,lwd=5,ylim=c(0,1))
  points(x,y=fx,col="black",lwd=5)
  title(xlab="x",ylab="p(x)",main="Plot of DTGIW(alpha,beta,lambda,theta)")
  ledtxt1 <- c("alpha","beta","lambda","theta")
  ledtxt2 <- as.character(parameters)
  legend("topright",legend = paste(ledtxt1,"=",ledtxt2),
        title = "Parameters")

}

