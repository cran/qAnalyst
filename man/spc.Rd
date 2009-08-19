\name{spc}
\alias{spc}
\title{ Function to create spc object }
\description{
\code{spc} function creates spc class objects from data. Generic methods \code{plot}, \code{print}, \code{summary} are avaiable for SPC class objects.
}
\usage{
spc(x, sg, type = "xbar", name = deparse(substitute(x)), testType = 1, k = NA, p = NA, nSigma = NA)
}

\arguments{
  \item{x}{ data vector }

  \item{sg}{ Subgroup variable. This argument has a different meaning according to chart type.

	\code{xbar}, \code{r} and \code{s} charts: \code{sg} represents rational sub-group of observations. It can be specified either as a vector, of the same length of \code{x} of ordered subgroup id, or as a constant value that will be replicated along x. Subgroup dimension must be greather than one.

	\code{i} and \code{mr} charts: \code{sg} represents the width of the window over which moving range are computed. Must be a scalar. Default value is set to two.

	\code{p}, \code{np} and \code{u} charts: \code{sg} represents sample size of observation. It can be specified either as a vector, of the same length of \code{x} or as a constant value that will be replicated along x.

	
	\code{c} chart: \code{sg} is not required

}



  \item{type}{Chart type. String identifying chart type. Charts can be of type: "xbar", "r", "s", "i", "mr", "p", "np", "u", "c".}

  \item{name}{ Name of x variable as it appears in the charts. By default the name of the given x variable }

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

  \item{k}{A vector or a scalar of parameters to be used by tests. If k is not specified, tests are performed with k default values. If k is specified, its length must be of the same length as testType.}

  \item{p}{A vector or a scalar of parameters to be used by tests. If p is not specified, tests are performed with p default values. If p is specified, its length must be of the same length as testType.}

  \item{nSigma}{A vector or a scalar of parameters to be used by tests. If nSigma is not specified, tests are performed with nSigma default values. If nSigma is specified, its length must be of the same length as testType.}


}

\details{
\code{spc} function performs coherence tests on subgroups dimension and chart name. Then it calculates graphical parameters, statistics values and test results and stores this values in lists.

}
\value{An object of class spc}
\references{ Montgomery, Statistical Quality Control }
\author{ Andrea Spano' }
\note{No notes}
\seealso{  \code{\link{plot.spc}}, \code{\link{print.spc}}}

\examples{

#i-chart, moving range to estimate st. dev. is equal to 2 points with testType=1, 
data(rawWeight)
ichart=spc(x=rawWeight$rawWeight, sg=2, type="i", name="weight", testType=1)
plot(ichart)
summary(ichart)

}
\keyword{ methods}

