\name{testFun}
\alias{testFun}
\title{ Internal function to perform control chart tests}
\description{\code{testFun} executes control charts test.
}
\usage{testFun(x, sg, type, testType, k, p, nSigma)
}

\arguments{
  \item{x}{ input data vector.}
  \item{sg}{ subgroups id, vector or integer.}
  \item{type}{ string denoting chart type.}
  \item{testType}{A vector or a scalar of test codes to be performed. Eight tests are implemented corrensponding to codes 1-8. Default performed test is 1.
		
		test 1: \code{k} points out of \code{p} points beyond \code{nSigma*sd} from center line. Default values are \code{k=1 p=1 nSigma=3}
	
		
		test 2: \code{k} points out of \code{p} points in a row on same side of center line. Default values are: \code{k=9 p=9}


		test 3: \code{k} points out of \code{p} points in a row either all increasing or decreasing. Default values: \code{k=6 p=6}
		

		test 4: \code{k} points out of \code{p} points in a row alternating up and down. Default values: \code{k= 14 p=14}


		test 5: \code{k} points out of \code{p} points in a row beyond \code{nSigma*sd} from center line. Default values are: \code{k=2 p=3 nSigma = 3} 


		test 6: \code{k} points out of \code{p} points in a row beyond \code{nSigma*sd} from center line (one side of center line). Default values: \code{k=4 p = 5 nSigma=2}


		test 7: \code{k} points out of \code{p} points in a row within nSigma*sd (both sides of centre line). Default values are: \code{k = 15 p=15 nSigma=1} 


		test 8: \code{k} points out of \code{p} points in a row beyond nSigma*sd (both sides of central line). Default values are \code{k=8 p = 8  nSigma=1} 

  }
  \item{k}{Parameter specific for the test. It is compulsory to be specified as a vector of length equal to length(testType). See details}
  \item{p}{Parameter specific for the test. it is compulsory to be specified as a vector of length equal to length(testType). See details}
  \item{nSigma}{ standard deviation of confidence line, required in some test.}
}



\value{A binary vector whose dimension is equal to the points plotted on the control chart is returned. A value of 1 means that the i-th point failed the performed test, a value of 0 means that the i-th points did not failed the performed test.

}
\references{ Montgomery, Statistical Quality Control.}
\author{Andrea Spano'}
\note{ This function is for internal use.}
\seealso{ \code{\link{spc}} }
\examples{
#example of test 3
x=c(1,0,3,2,3,5,6,8,7,6)
sg=2
#i chart used for simplicity
outTest=testFun(x=x,type="i",sg=sg, testType=3,k=4,p=4,nSigma=3)
print(outTest)
# 0 0 0 0 0 0 1 1 0 0 ok
}

\keyword{ methods }
\keyword{ internal}
