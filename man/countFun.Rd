\name{countFun}
\alias{countFun}
\title{ Function to count number of non missing values }
\description{counts the number of non missing values in a vector or within groups 
}
\usage{countFun(x)}
\arguments{
  \item{x}{ data vector }
}
\details{
internal function
}
\value{
an integer whose value is the number of non - missing values
}
\references{ Internal function }
\author{ Andrea Spano' and Giorgio Spedicato }
\note{ This is mainly an internal function used by centerFun}
\seealso{ \code{\link{centerFun}}}
\examples{## The function is currently defined as
a=c(NA,NA,2,4,6,1,NA)
print(countFun(a))
#4}
\keyword{ internal }
\keyword{ methods }
