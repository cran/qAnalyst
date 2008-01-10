\name{toyCarsDefects}
\alias{toyCarsDefects}
\docType{data}
\title{ Toys car defects}
\description{
Manufactured toys car are sampled. Sample size and the total number of defectives on the sampled cars is reported.
}
\usage{data(toyCarsDefects)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{defects}}{number of defects found}
    \item{\code{sampled}}{sample size}
  }
}
\details{}
\source{
Industrial data
}
\references{
This dataset can be shown by a u - chart
}
\examples{
data(toyCarsDefects)
head(toyCarsDefects)
chart=spc(x=toyCarsDefects$defects, sg=toyCarsDefects$sampled, name="defects", type="u")
plot(chart)
}
\keyword{datasets}
