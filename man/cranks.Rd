\name{cranks}
\alias{cranks}
\docType{data}
\title{ Cranks data set}
\description{
This dataset comes from an engines assembly department, where the deviation from an ideal position of a parts of the crankshaft are measured throught working days.
}
\usage{data(cranks)}
\format{
  A data frame with 125 observations on the following 2 variables.
  \describe{
    \item{\code{crankshaft}}{Distance in millimeters from the actuual point of the crankshaft to the baseline position}
    \item{\code{workingDay}}{working day of the relevations. This is the subgroups id.}
  }
}
\details{
Subgroups dimension is not equal
}
\source{
Industrial data
}
\references{This dataset can be used to show x-bar and s-bar chart capabilities}
\examples{
data(cranks)
xbarchart=spc(x=cranks$crankshaft, sg=cranks$workingDay, type="xbar", name="crankshaft")
rbarchart=spc(x=cranks$crankshaft, sg=cranks$workingDay, type="r", name="crankshaft")
plot(xbarchart)
plot(rbarchart)
}
\keyword{datasets}
