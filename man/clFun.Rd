\name{clFun}
\alias{clFun}
\title{ Function to compute upper or lower confidence limits for the specified control chart }
\description{
Returns a vector of length equal to the number of points to be plotted in the chart corrensponding to the required confidence limit,
}
\usage{
clFun(x, sg, nSigma, cl, type = "xbar")
}

\arguments{
  \item{x}{ data vector }
  \item{sg}{ subgroup id for variable chart, subgroups dimension for variables chart, moving range interval in mr chart }
  \item{nSigma}{ number of standard deviations from the center line statistic}
  \item{cl}{ "u" or "l" respectively for upper or lower limit}
  \item{type}{ type of control chart}
}
\details{
It allows subgroup of not constant dimension.
}
\value{
a vector of length equal to the number of points in the chart representing either lower o upper confidence limit
}
\references{ Montgomery, Statistical Quality Control }
\author{ Andrea Spano' and Giorgio Specicato }
\note{ Internal function}
\seealso{ \code{\link{spc}}, \code{\link{plot.spc}}}
\examples{
#simple example
x=rnorm(30,m=10,s=1)
sg=sort(rep(1:10,3))
limiti=clFun(x=x,sg=sg,type="r",cl="u",nSigma=3)
#4.125743 4.125743 4.125743 4.125743 4.125743 4.125743 4.125743 4.125743
#4.125743 4.125743
}
\keyword{ methods }
\keyword{ internal }
