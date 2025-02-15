% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rDTGIW.R
\name{rDTGIW}
\alias{rDTGIW}
\title{The random generating function for Discrete Transmuted Generalized Inverse Weibull(DTGIW) distribution.}
\usage{
rDTGIW(n, alpha, beta, lambda, theta)
}
\arguments{
\item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}

\item{alpha}{shape parameter#1.}

\item{beta}{scale parameter.}

\item{lambda}{shape pameter#2.}

\item{theta}{the transmuted parameter.}
}
\value{
the n random number of DTGIW distribution.
}
\description{
This function generates random numbers for the DTGIW distribution.
}
\details{
The R script generates the n random values of the DTGIW distribution is shown based on the research paper in references.
}
\examples{
rDTGIW(n=100,3.45,0.7,1.05,0)

}
\references{
Atchanut Rattanalertnusorn and Sirinapa Aryuyuen (2021).
The zero-truncated discrete transmuted generalized inverse Weibull distribution and its applications,
Songklanakarin Journal of Science and Technology (SJST), Volume 43 No.4 (July - August 2021), pp. 1140 - 1151. DOI: 10.14456/sjst-psu.2021.149.
}
