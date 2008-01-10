\name{qAnalyst-package}
\alias{qAnalyst-package}
\alias{qAnalyst}
\docType{package}
\title{Package to create, plot and study control charts by variables, attributes, individual values}
\description{Creates xbar, s, r, i, mr , p, np, c, u chart}
\details{
\tabular{ll}{
Package: \tab qAnalyst\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2008-01-10\cr
License: \tab GPL -2\cr
}

Function \code{spc} creates objects of class \code{spc}. 
spc could be created for chart of type xbar, r, s, i, mr, p, np, c and u 
Generic methods: plot, print and summary exist for objects of class spc.

}
\author{
Andrea Spano' and Giorgio Spedicato
Maintainer: Who to complain to <andrea.spano@quantide.com>

}
\references{
Montgomery, Statistical Quality Control
}
\keyword{ package }
\examples{

#a simple x bar chart
#rnorm data creation
x=c(rnorm(30,m=10,s=1), rnorm(30,m=12,s=1), rnorm(30,m=10,s=4))
sg=sort(rep(1:30,3))
go<-spc(x=x,sg=sg,type="xbar")
plot(go)
print(go)
summary(go)
rm(go)

#second example. Toys dataset
data(toys)
toyWeightxbar=spc(toys$weight, sg=toys$day, type="xbar", "toy car weight")
plot(toyWeightxbar)
summary(toyWeightxbar)
print(toyWeightxbar)

}
