\name{capabilityNormal}
\alias{capabilityNormal}
\title{ Capability object creator }
\description{
 \code{capabilityNormal} function creates capability class object for normal data given specification limits. \code{plot}, \code{print}, \code{summary} methods are avaible for capability class objects.
}
\usage{
capabilityNormal(x, sg=length(x), lsl = NULL, usl = NULL, target = NULL, name = deparse(substitute(x)), toler=6, histPars=c(NA,NA))}
\arguments{
  \item{x}{ a numeric data vector. }
  \item{sg}{ Subgroup specification. It represents rational sub-group of observations. It can be specified either as a vector, of the same length of \code{x} of ordered subgroup id, or as a constant indicating subgroup dimension. Default is sg=length(x), that means there are no subgroups.}
  \item{lsl}{ lower specification limit. }
  \item{usl}{ upper specification limit. }
  \item{target}{ process target. }
  \item{name}{ name of the variable, default taken by "x" name. }
  \item{toler}{width of tolerance specification. Default is 6, that is six sigma}
  \item{histPars}{a vector of two elements that allows user to specify historical mean and standard deviation respectively}
}
\details{
  Either lsl or usl must be specified. \code{capabilityNormal} is a wrapper of many internal functions, that check user specifications consistency and perform calculation of statistics. Results are stored into a capability object.
}
\value{ A capability object list is returned }
\references{ Bothe (1997), Measuring Process Capability, McGraw Hill }
\author{ Giorgio Spedicato }
\note{ It creates the core capability objects}
\seealso{ \code{\link{plot.capability}}, \code{\link{print.capability}}, \code{\link{summary.capability}} }
\examples{
data(brakeCap)
capObj=capabilityNormal(x=brakeCap$hardness,sg=brakeCap$subgroup, lsl=39,usl=41, target=40,  name="HARDNESS")
summary(capObj)
  }
\keyword{ methods }
