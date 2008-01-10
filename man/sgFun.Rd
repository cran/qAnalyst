\name{sgFun}
\alias{sgFun}

\title{ Function to create subgroups}
\description{
sgFun creates suitable subgroups according to chart type }
\usage{
sgFun(x, sg, type)
}

\arguments{
  \item{x}{ data vector }
  \item{sg}{ eithet subgroup vector of the same length of x or constant value}
  \item{type}{ chart type }
}
\details{
If sg is an integer,  it represents subgroup dimension. In this case subgroup are created as a sequence along x. Last subgroup may not have same dimention as previous subgroup. 

If sg is a vector, subgroup can have different dimension. 

In mr chart sg parameter represent the size of the mooving window.
}
\value{
A vector of subgroup id
}
\references{ Internal function }
\author{ Giorgio Spedicato and Andrea Spano'}
\note{ Internal function
}
\seealso{  \code{\link{spc}}}
\examples{
x=c(4,5,6,7,8,3)
print(sgFun(x=x,sg=2,type="xbar"))
}

\keyword{ methods }
\keyword{ internal }
