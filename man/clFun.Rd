\name{clFun}
\alias{clFun}
\title{ Internal function to compute upper or lower confidence limits for the specified control chart }
\description{
\code{clFun} calculates upper and lower confidence limits for control charts. Confidence limits are retuned as a vector.
}
\usage{
clFun(x, sg, nSigma, cl, type = "xbar", mu = NA, sigma = NA)
}

\arguments{
  \item{x}{ data vector }
  \item{sg}{ subgroup id for variable chart, subgroups dimension for variables chart, moving window interval in i / mr chart. A detailed description of sg field is in \code{\link{spc}} function help. }
  \item{nSigma}{ number of standard deviations from the center line}
  \item{cl}{ "u" or "l" respectively for upper or lower confidence limit}
  \item{type}{ type of control chart}
  \item{mu}{A scalar containing the process mean. Its use will be different depending from chart type. For more details see \link{spc}.}
  \item{sigma}{A scalar containing the process within samples standard deviation. For more details see \link{spc}.}
}
\details{
clFun allows to handle unequal sample size subgroups that makes confidence limits to vary. A warning is thrown if subgroup dimension is less than 2 when variables chart for groups are calculated.
}
\value{
A vector of length equal to the number of points in the chart representing either lower o upper confidence limit.
}
\references{ Montgomery, Statistical Quality Control }
\author{ Andrea Spano' and Giorgio Specicato }
\note{ Internal function}
\seealso{ \code{\link{spc}}, \code{\link{plot.spc}}}
\examples{
#brakeCap dataset
data(brakeCap)
clFun(x=brakeCap$hardness, sg=brakeCap$subgroup, nSigma=2, cl="u", type="xbar")
}
\keyword{ methods }
\keyword{ internal }
