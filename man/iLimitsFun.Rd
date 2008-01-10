\name{iLimitsFun}
\alias{iLimitsFun}
\title{ Internal function used to plot variables confidence limits }
\description{
It receives x groups coordinates. Then it returns a vector of length 2*length(x) whose elements are the x values shifted rightward and leftward by 0.5
}
\usage{
iLimitsFun(i)
}

\arguments{
  \item{i}{ x groups coordinates}
}
\details{
  This is an internal function, that helps to plot variable confidence limits.
}
\value{
	It returns a vector of length 2*length(x)

}
\references{ Internal function }
\author{ Giorgi Spedicato and Andrea Spano' }
\note{ This is used by spc object plotting function
}
\seealso{  \code{\link{spc}}}
\examples{

x=c(2,5,8,4,7)
iLimitsFun(x)
# 1.5 4.5 4.5 7.5 7.5 3.5 3.5 6.5 6.5 7.5
  
}
\keyword{ internal }
\keyword{ methods }
