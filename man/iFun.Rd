\name{iFun}
\alias{iFun}

\title{ iFun }
\description{
  Internal function to create a sequence from 1 to n, where n=length(x)
}
\usage{
iFun(x)
}
\arguments{
  \item{x}{ a numeric vector }
}
\details{
 This is a simple internal function, used for plotting purpuoses
}
\value{
a sequnce vector
}
\references{ Internal function }
\author{ Giorgio Spedicato and Andrea Spano' }
\note{ Internal function}
\seealso{ \code{\link{spc}}, \code{\link{plot.spc}} }
\examples{
#very simple example
x=c(4,5,8,2,3)
output=iFun(x)
print(output)
# 1 2 3 4 5

}
\keyword{ methods }
\keyword{ internal }
