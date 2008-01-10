\name{switchFun}
\alias{switchFun}
\title{ Function to correct evident mispellings }
\description{
  The purpuose of this function is to return the correct parse of the command given by the user. It is an internal function.
}
\usage{
switchFun(type, argument)
}

\arguments{
  \item{type}{ The user input }
  \item{argument}{ The context of the input }
}
\details{At the moment it is used only to dectect chart type.}
\value{A character item containing the correct codification, if identificable.}
\references{ Internal function }
\author{ Giorgio Spedicato }
\note{ This is an internal function}
\seealso{ \code{\link{spc}}}
\examples{
print(switchFun("x-bar", "chart"))
}
\keyword{ methods }
\keyword{ internal }
