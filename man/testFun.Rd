\name{testFun}
\alias{testFun}
\title{ Internal function to perform control chart tests}
\description{\code{testFun} executes control charts test.
}
\usage{testFun(x, sg, type, testType, k, p, nSigma, mu = NA, sigma = NA)
}

\arguments{
  \item{x}{ input data vector.}
  \item{sg}{ subgroups id, vector or integer.}
  \item{type}{ string denoting chart type.}
  \item{testType}{A vector or a scalar of test codes to be performed. Eight tests are implemented corrensponding to codes 1-8. Default performed test is 1.
		
		test 1: At least \code{k} out of \code{p} points in a row beyond Zone A (outside the control limits). Default values: \code{k = p = 1, nSigma = 3}.

    test 2: At least \code{k} out of \code{p} points in a row on one side of central line. Default values: \code{k = p = 9}.

    test 3: At least \code{k} out of \code{p} points in a row all increasing or all decreasing. Default values: \code{k = p = 6}.

    test 4: At least \code{k} out of \code{p} points in a row all up and down. Default values: \code{k = p = 14}.

    test 5: At least \code{k} out of \code{p} points in a row in Zone A or beyond (> (2/3)*nSigma*sigma from central line; same side of central line). Default values: \code{k = 2, p = 3, nSigma = 3}.

    test 6: At least \code{k} out of \code{p} points in a row in Zone B or beyond (> (1/3)*nSigma*sigma from central line; same side of central line). Default values: \code{k = 4, p = 5, nSigma = 3}.

    test 7: At least \code{k} out of \code{p} points in a row in Zone C (both sides of central line). Default values: \code{k = p = 15, nSigma = 3}

    test 8: At least \code{k} out of \code{p} points in a row with no one in Zone C. Default values: \code{k = p = 8,  nSigma = 3}

  }
  
  \item{k}{Parameter specific for the test. It is compulsory to be specified as a vector of length equal to length(testType). See details}
  \item{p}{Parameter specific for the test. it is compulsory to be specified as a vector of length equal to length(testType). See details}
  \item{nSigma}{standard deviation of confidence line, required in some test.}
  \item{mu}{A scalar containing the process mean. Its use will be different depending from chart type. For more details see \link{spc}.}
  \item{sigma}{A scalar containing the process within samples standard deviation. For more details see \link{spc}.}

}



\value{A binary vector whose dimension is equal to the points plotted on the control chart is returned. A value of 1 means that the i-th point failed the performed test, a value of 0 means that the i-th points did not failed the performed test.

}
\references{ Montgomery, Statistical Quality Control.}
\author{Andrea Spano'}
\note{This function is for internal use. For more details see \link{spc}.}
\seealso{ \code{\link{spc}} }
\examples{
# example of test 3
x = c(1,0,3,2,3,5,6,8,7,6)
sg = 2
#i chart used for simplicity
outTest = testFun(x=x, type="i", sg=sg, testType=3, k=4, p=4, nSigma=3)
print(outTest)
# 0 0 0 0 0 0 1 1 0 0 ok
}

\keyword{ methods }
\keyword{ internal}
