\name{centerFun}
\alias{centerFun}
\title{ Function to calculate center line values }
\description{
This function returns the center line of the given chart tipe.
}
\usage{
centerFun(x, sg, type = "xbar")
}

\arguments{
  \item{x}{ data vector }
  \item{sg}{ subgroups id for variables chart, subgroups dimension for attributes chart, interval dimension for mr chart }
  \item{type}{ chart type }
}
\details{
This function calculates the center line value. Formulas used are taken from quoted Montgomery's book.
}
\value{
  A numeric value for the center line
}
\references{ Montgomery, Statistical Quality Control }
\author{ Andrea Spano' and Giorgio Spedicato }
\note{ 
This function is called internally by spc
}
\seealso{ \code{\link{spc}}, \code{\link{clFun}} }
\examples{
#random data
set.seed(100)
x=rpois(100,5)
sg=rep(1,100)
print(centerFun(x=x,sg=sg,type="c"))
#5.15
}
\keyword{ methods }
\keyword{ internal }
