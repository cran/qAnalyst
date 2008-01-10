\name{plot.spc}
\alias{plot.spc}
\title{Plots spc class objects}
\description{Plots spc class objects
}

\usage{
\method{plot}{spc}(x, \dots)
}



\arguments{
\item{x}{ An object of class spc}
\item{...}{furter arguments to be added}
}
\details{It makes use of lattice graphics.}
\value{No values are returned}
\references{ Internal function }
\author{ Giorgio Spedicato and Andrea Spano' }
\note{ An spc object is required}
\seealso{  \code{\link{spc}}, \code{\link{print}} }
\examples{#See xbar example
set.seed(100)
x=c(rnorm(30,m=10,s=1), rnorm(30,m=12,s=1), rnorm(30,m=10,s=4))
sg=sort(rep(1:30,3))
go=spc(x=x,sg=sg,type="xbar")
plot(go)
}
\keyword{ methods }

