\name{rFun}
\alias{rFun}
\title{ Function to compute range values}
\description{
 This function gives the absolute value of the range of a data vector (max - min). It handles missing data.
}
\usage{
rFun(x)
}
\arguments{
  \item{x}{data vector }
}
\details{
X may contain NA values
}
\value{
The range numeric value is returned. It may be NA if all elements in input data are NA
}
\references{ Internal function }
\author{ Andrea Spano' and Giorgio Spedicato }
\note{ This is mainly an internal function used by centerFun an clFun
}
\seealso{  \code{\link{centerFun}}, \code{\link{clFun}} }
\examples{x=c(1,2,3,NA, NA)
print(rFun(x))
# 2
}
\keyword{ methods }
\keyword{ internal }
