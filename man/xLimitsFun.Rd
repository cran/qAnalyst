\name{xLimitsFun}
\alias{xLimitsFun}

\title{ Internal function to return x variable confidence limits plotting coordinates}
\description{
It receives a vector of x coordinates and returns another vector of x coordinates
}
\usage{
xLimitsFun(x)
}

\arguments{
  \item{x}{ it is a vector of coordinates }
}
\details{
This is an internal function
}
\value{
The object returned is a vector of coordinates
}
\references{ Internal function }
\author{ Andrea Spano' }
\note{ Used by spc
}
\seealso{  \code{\link{spc}}}
\examples{

#only for illustrative purpuoses
x=c(1,2,3,4)
output=xLimitsFun(x)
# 1 1 2 2 3 3 4 4
}
\keyword{ methods }
\keyword{ internal }
