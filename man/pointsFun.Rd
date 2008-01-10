\name{pointsFun}
\alias{pointsFun}
\title{ Function to obtain y coordinates of the statistics to be plotted on chosen chart type }
\description{
It receives the data vector, the subgroup id , charts parametres and it returns charts statistics
}
\usage{
pointsFun(x, sg, type = "xbar")
}
\arguments{
  \item{x}{ data vector }
  \item{sg}{ subgroup id of same length of x, in mr chart is the length of moving range interval }
  \item{type}{ type of chart, xbar default }
}
\details{
This is mainly an internal function
}
\value{
it returns a vector of the same length of the numbers of subgroups containing the chart statistics corresponding to each group }
\references{ Internal function }
\author{ Andrea Spano' and Giorgio Spedicato }
\note{ This is mainly an internal function
}
\seealso{  \code{\link{spc}}, \code{\link{clFun}} }
\examples{

##
## The function is currently defined as

x=rnorm(30,m=10,s=1)
sg=1:30
points=pointsFun(x=x,sg=sg,type="xbar")
print(points)
#due to random generation data will be different
#10.306926  9.955528 10.520426 10.884503  9.723296  9.368019 11.614712 10.233007 
#7.869427 10.387983 10.480655  8.320567  9.692349  9.968961 10.198713  9.239214 
#11.009421  9.566865  9.571260 10.168357 10.149711 10.288794 10.652557  9.438960 
#9.321480 10.332380  9.433116  9.816703  9.590358  9.216149 
}
\keyword{ methods }
\keyword{ internal }
