\name{ylabFun}
\alias{ylabFun}

\title{ Function to make the title of y axis label }
\description{
Internal function to generate the y axis label
}
\usage{
ylabFun(xName, type = "xbar")
}

\arguments{
  \item{xName}{ is the name of the analyzed variable }
  \item{type}{ type of chart to be plotted, default "xbar" }
}
\details{
This is an internal function
}
\value{
  A string that concatenates chartType of xNames
}
\references{ Internal function }
\author{ Giorgio Spedicato }
\note{ Internal function
}
\seealso{  \code{\link{spc}}, \code{\link{plot.spc}} }
\examples{
type="i"
xName="weight"
print(ylabFun(xName,type))
#"Individual values of weight"
}
\keyword{ methods}
\keyword{ internal}
