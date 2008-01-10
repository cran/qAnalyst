\name{rawWeight}
\alias{rawWeight}
\docType{data}
\title{ Raw material bars weigth}
\description{
This dataset contains individual measumements of batch of raw material.
}
\usage{data(rawWeight)}
\format{
  A data frame with 45 observations on the following 2 variables.
  \describe{
    \item{\code{rawWeight}}{weight of each bar. Measured in pounds}
    \item{\code{id}}{id number of measumement}
  }
}
\details{}
\source{
Industrial data.
}
\references{
This dataset can be used to shown examples of individual charts.
}
\examples{
data(rawWeight)
head(rawWeight)
ichart=spc(x=rawWeight$rawWeight,sg=1,type="i",name="weight")
plot(ichart)
mrchart=spc(x=rawWeight$rawWeight,sg=1,type="mr",name="weight")
plot(mrchart)
}
\keyword{datasets}
