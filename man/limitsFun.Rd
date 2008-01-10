\name{limitsFun}
\alias{limitsFun}
\title{ Function to determine y-axis limits }
\description{It receives the points to be plotted vector and returns a two elements vector containing the lower and upper limits y-axis limits. They are determined to obtain pretty plotting}
\usage{limitsFun(list)
}
\arguments{\item{list}{ a vector or list of points }
}
\details{The vector / list given in input is unlisted before calculations start. The limits range given in output is 1.05 times the range between the points given in input.
}
\value{It returns a vector whose first element is the lower limit and the upper limit.
}
\references{ This is an internal function }
\author{ Andrea Spano' }
\note{ It is used by plot.spc
}
\seealso{ \code{\link{spc}} }
\examples{##use of this internal function
x=c(2,5,8,4,7)
limits=limitsFun(x)
# 1.7 8.3
}
\keyword{ methods }
\keyword{ internal }
