\name{summary.spc}
\alias{summary.spc}
\title{Function to print spc object statistics }
\description{This function receives an spc object and prints general statistics, the chart elements coordinates, performed test details.
}

\usage{
\method{summary}{spc}(object, \dots)
}



\arguments{
\item{object}{ an object of class spc }
\item{...}{furter arguments to be added}
}
\details{  This function extracts information from the lists embedded internally in the SPC object given. 
}
\value{
This function does not return any value 
}
\references{ Final user function }
\author{ Giorgio Spedicato }
\note{ Default printed informations are general statistics.
}
\seealso{ \code{\link{spc}}, \code{\link{plot.spc}}, \code{\link{print.spc}} }
\examples{set.seed(100)
x=c(rnorm(30,m=10,s=1), rnorm(30,m=12,s=1), rnorm(30,m=10,s=4))
sg=sort(rep(1:30,3))
#1 LA + SEMPLICE
go=spc(x=x,sg=sg,type="xbar", testType=2, k=0,nSigma=0, p=0)
plot.spc(go)
summary.spc(go)
rm(go)
}
\keyword{methods}

