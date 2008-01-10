\name{getParameterFun}
\alias{getParameterFun}

\title{ Function to get control chart coefficients }
\description{
  
Returns the value of a parameter given subgroup size and parameter name.
}
\usage{
getParameterFun(n, parameter)
}

\arguments{
  \item{n}{ subgroups length }
  \item{parameter}{ required parameter string }
}
\details{
If sub group size is less than 25 than tabulated values are used. Otherwise, Montogmery SQC book's formulas are used.
}
\value{
Value of the requested parameter given sample size
}
\references{ Montgomery, Statistical Quality Control }
\author{ Giorgio Spedicato and Andrea Spano'}
\note{}
\seealso{ \code{\link{getCoeffFun}} }
\examples{
#a simple example
print(getParameterFun(5,"d2"))
}
\keyword{ methods }
\keyword{ internal }
