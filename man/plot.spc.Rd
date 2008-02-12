\name{plot.spc}
\alias{plot.spc}
\title{Plot method for spc class objects}
\description{\code{plot.spc} implements generic plot method for spc class objects.  Plot of spc object is performed by trellis graphics.
}
\usage{
\method{plot}{spc}(x, \dots)
}
\arguments{
\item{x}{ An object of class spc}
\item{...}{furter arguments to be added}
}
\details{\code{plot.spc} requires lattice library.}
\value{No values are returned}
\references{ Lattice package help manual }
\author{ Giorgio Spedicato and Andrea Spano' }
\note{ An spc object is required}
\seealso{  \code{\link{spc}}, \code{\link{print}} }
\examples{
#brakeCap
data(brakeCap)
x=brakeCap$hardness
sg=brakeCap$subgroup
go=spc(x=x,sg=sg,type="xbar")
plot(go)
}
\keyword{ methods }

