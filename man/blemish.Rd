\name{blemish}
\alias{blemish}
\docType{data}
\title{ Blemishes on sampled quare of fabric}
\description{
This data set comes from a linen manifacturing. 100 square yard of fabric are sampled and blemish are counted.
}
\usage{data(blemish)}
\format{
  A data frame with 40 observations on the following 2 variables.
  \describe{
    \item{\code{blemish}}{number of blemishes counted}
    \item{\code{linenSampled}}{sample id}
  }
}
\details{
Data from this sample are suitable to be projected into a C control chart.
}
\source{
Industrial data.
}
\references{
This type of data can be used withing attribute chart presentation.
}
\examples{
data(blemish)
cchart=spc(x=blemish$blemish, sg=1, type="c", name="blemish")
plot(cchart)
}
\keyword{datasets}
