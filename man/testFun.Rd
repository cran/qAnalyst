\name{testFun}
\alias{testFun}
\title{ Function to perform control chart tests}
\description{This function executes control charts test as performed in Minitab
}
\usage{testFun(x, sg, type, testType, k, p, nSigma)
}

\arguments{
  \item{x}{ input data vector }
  \item{sg}{ subgroups id }
  \item{type}{ type of chart }
  \item{testType}{ a vector (eventually a scalar) containing the code of the desidered tests to be performed }
  \item{k}{ k is a parameter specific for the test. It is compulsory to be specified as a vector of length equal to length(testType) }
  \item{p}{ p is a parameter specific for the test. it is compulsory to be specified as a vector of length equal to length(testType) }

  \item{nSigma}{ standard deviation of confidence line, required in some test.}
}

\details{This items list follows the description presented in the quoted Minitab manual
\itemize
{
\item{1}{test 1: k points on k beyond zone A (nSigma=3), default values: k=1=p nSigma=3}
\item{2}{test 2:  k points in a row on same side of centre line, default values: k=p=9 }
\item{3}{test 3:  k points in a row all increasing all decreasing, default values: k=p=6 }
\item{4}{test 4:  at least k on p points in a row all up and down, default values: k=p=14 }
\item{5}{test 5: k out of p point in a row beyond zone A, default values: k=2 p=3 nSigma = 3}
\item{6}{test 6:  k out of p points in a row in zone B (nSigma=2) or beyond (one side of center line), default values: k=4 p = 5 nSigma=2 }
\item{7}{test 7:  k points on p in a row within zone C (both sides of centre line), p=k=15 nSigma=1 }
\item{8}{test 8:  k points in a row beyond zone C (nSigma=1) (both sides of central line), default values: p=k=8 nSigma=1 }
}

If the parameter vectors have zero elements, they are converted into default values for the test. To avoid confusion on parameters assignment the length of parameter vectors shall be equal.
Some checks on the internal coerence of the given parameters are performed.

}


\value{A binary vector whose dimension is equal to the points plotted on the control chart is returned. A value of 1 means that the i-th point failed the performed test, a value of 0 means that the i-th points did not failed the performed test.

}
\references{ Ryan Joiner Minitab Handbook 4th }
\author{ Giorgio Spedicato and Andrea Spano' }
\note{ This function is experimental and for internal use. It is called via testWrapperFun.}
\seealso{ \code{\link{spc}} }
\examples{
#example of test 3
x=c(1,0,3,2,3,5,6,8,7,6)
sg=1
#i chart used for simplicity
outTest=testFun(x=x,type="i",sg=sg, testType=3,k=4,p=4,nSigma=3)
print(outTest)
# 0 0 0 0 0 0 1 1 0 0 ok
}

\keyword{ methods }
\keyword{ internal}
