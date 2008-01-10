\name{statsFun}
\alias{statsFun}

\title{ Function to calculate control chart general statistics }
\description{
This function receives data information and received chart informations. It returns a list whose elements are general statistics and chart - specific statistics.
}
\usage{
statsFun(x, sg, type = type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data vector }
  \item{sg}{ subgroup vector. In mr chart is the moving range interval }
  \item{type}{ chart type }
}
\details{
The type of chart is compulsory
}
\value{

  \item{numTot }{total number of elements in x}
  \item{numNNmissing }{total number of non - missing elements in x }
  \item{numMissing }{total number of missing elements in x}
  \item{nGroupsX }{total number of groups in x }

  \item{meanX }{mean of X}
  \item{minX}{min of X}
  \item{maxX}{max of X}

  \item{sdTotX }{total standard deviation of X}
  \item{sdWithinX}{sd within of X}
  \item{sdBetweenX}{sd between of X}
  \item{meanRangeX}{mean range of X}
  
}
\references{ Internal function}
\author{ Giorgio Spedicato}
\note{ Return a list used by spc.
\section{Warning }{used formula should be validated}
}
\seealso{  \code{\link{spc}}, \code{\link{print.spc}} }
\examples{#simple example
set.seed(100)
x=c(rnorm(30,m=10,s=1), rnorm(30,m=12,s=1), rnorm(30,m=10,s=4))
sg=sort(rep(1:30,3))
statsList=statsFun(x=x, sg=sg, type = "xbar")
str(statsList)
}
\keyword{ methods }
\keyword{ internal }
