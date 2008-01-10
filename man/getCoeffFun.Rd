\name{getCoeffFun}
\alias{getCoeffFun}
\title{ Wrapper function to getParameterFun }
\description{
For a given subgroup size and parameter specification returns the corrensponding value of the parameter
}
\usage{
getCoeffFun(sampleSize, coeff)
}

\arguments{
  \item{sampleSize}{ A vector of subgroups size }
  \item{coeff}{ a string that specifies the required parameters }
}
\details{Internal function}
\value{
A vector of coefficients
}
\references{ Montgomery, Statistical Quality Control }
\author{ Giorgio Spedicato and Andrea Spano'}
\note{ It is a wrapper for getCoeffFun. It handles missing values, returning NA if subgroup dimension is NA
}
\seealso{ \code{\link{getParameterFun}}}
\examples{
##Simple example

sgDim=c(10,5,8,10)
print(getCoeffFun(sgDim,"d3"))
# 0.797 0.864 0.820 0.797
}
\keyword{ internal }
\keyword{ methods}
