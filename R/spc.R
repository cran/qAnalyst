`spc` <-
function(x,sg=NULL,type="xbar", name=deparse(substitute(x)), testType=1, k=NA, p=NA, nSigma=NA)
{

#checks whereter the specified chart exists (and is implemented)
type=switchFun(argument=type,type="chart")
if(!is.element(type,c("xbar","s","r", "i","mr", "p", "np", "c", "u"))) stop("Error! Unrecognized chart")
#chart
#bug
if((is.null(sg)) && (!type=="c")) stop("Error! Sg is always required with the exception of c charts")

#checks if x is numeric

if (!is.numeric(x)) stop("Error! x is not numeric")

#changes subgroups
#only for i-mr
#sg externally is moving range groups length (e.g. 2) but internally
#is moving range difference (e.g. 1)
#bug
if (is.element(type,c("i", "mr"))){ 


sg=sg-1
if (sg<1) stop("Error! width of the moving range window must be at least equal to 2")

}
sg=sgFun(x=x,sg=sg,type=type)
#check if variables chart exist sg<2
if (is.element(type,c("xbar","s","r"))) {
	sgSize = as.numeric(tapply(x, sg, countFun))
	sgSizeTest=ifelse(sgSize>1, 1,0)
	if (sum(sgSizeTest)<length(sgSizeTest)) stop("Error! At least one subgroup with dimension lower than 2 in a variable chart for subgroups", call.=FALSE, immediate.=TRUE)}

#chart name
xName = name

#graphical elements

points = pointsFun(x=x, sg=sg, type = type)
i = iFun(points)
center = centerFun(x=x, sg=sg, type = type)

ucl3 = clFun(x = x, sg = sg, nSigma = 3, cl = "u" ,type = type)
lcl3 = clFun(x = x, sg = sg, nSigma = 3, cl = "l" ,type = type)
ucl2 = clFun(x = x, sg = sg, nSigma = 2, cl = "u" ,type = type)
lcl2 = clFun(x = x, sg = sg, nSigma = 2, cl = "l" ,type = type)
ucl1 = clFun(x = x, sg = sg, nSigma = 1, cl = "u" ,type = type)
lcl1 = clFun(x = x, sg = sg, nSigma = 1, cl = "l" ,type = type)


iForLimits = iLimitsFun(i)
ucl3ForLimits = xLimitsFun(ucl3)
lcl3ForLimits = xLimitsFun(lcl3)

#executing test
if (is.null(testType))
	{
		resultsOfTest=list()
		resultsOfTest$colorSet="#40f907" #very greej = col9
		resultsOfTest$testMatrix=NULL
	}
else	
	{
		resultsOfTest=testFun(x=x,sg=sg, type=type,  testType=testType, nSigma=nSigma, k=k,p=p)
	}

#ylim 
ylim = limitsFun(list(points, ucl3, lcl3))

#ylab 
ylab = ylabFun(xName, type = type)

#xlab text (depending individuals or groups)
#bug
xlab=ifelse(is.element(type,c("i","mr")),"index", "subgroups")

#statistical utility launch

statisticsList=statsSpcFun(x=x,sg=sg, type=type)
#creating sublists
#lista con nome, tipo carte e statisticsList

general=list()

general$chartType=type
general$xName=xName

#statisticsList
general$numTot=statisticsList$numTot
general$numNNmissing=statisticsList$numNNmissing
general$numMissing=statisticsList$numMissing
general$nGroupsX=statisticsList$nGroupsX

general$meanX=statisticsList$meanX
general$minX=statisticsList$minX
general$maxX=statisticsList$maxX

general$sdTotX=statisticsList$sdTotX
general$sdWithinX=statisticsList$sdWithinX
general$sdBetweenX=statisticsList$sdBetweenX
general$meanRangeX=statisticsList$meanRangeX
#
#greating graphPars
graphPars=list()

graphPars$xlab=xlab
graphPars$ylab=ylab

graphPars$points=as.numeric(points)
graphPars$i=i
graphPars$center=center
#graphPars$sgSize=sgSize
graphPars$ylim=ylim
graphPars$ucl3=ucl3
graphPars$lcl3=lcl3
graphPars$ucl2=ucl2
graphPars$lcl2=lcl2
graphPars$ucl1=ucl1
graphPars$lcl1=lcl1

graphPars$iForLimits=iForLimits
graphPars$ucl3ForLimits=ucl3ForLimits
graphPars$lcl3ForLimits=lcl3ForLimits
#colors are graphpars
graphPars$colors=resultsOfTest$colorSet

#creates list of testResults
testResults=list()
testResults$testOutput=resultsOfTest$testMatrix

#final binding
spcObj=list(general=general, graphPars=graphPars, testResults=testResults, call=match.call())
class(spcObj) = "spc"
invisible(spcObj)
}
