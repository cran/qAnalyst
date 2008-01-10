`spc` <-
function(x,sg=NULL,type="xbar", name=deparse(substitute(x)), testType=NA, k=NA, p=NA, nSigma=NA)
{

#checks whereter the specified chart exists (and is implemented)
type=switchFun(type=type,argument="chart")

if(!is.element(type,c("xbar","s","r", "i","mr", "p", "np", "c", "u"))) stop("Error! Unrecognized chart")

#chart
if((is.null(sg)) && (!type=="c")) stop("Error! sg is not needed only in c chart")

sg=sgFun(x=x,sg=sg,type=type)

#nome della carta (elemento restituito ok)
xName = name
#points (elemento restituito)
points = pointsFun(x=x, sg=sg, type = type)
i = iFun(points)
center = centerFun(x=x, sg=sg, type = type)
#limits (elementi restituiti)
ucl3 = clFun(x = x, sg = sg, nSigma = 3, cl = "u" ,type = type)
lcl3 = clFun(x = x, sg = sg, nSigma = 3, cl = "l" ,type = type)
ucl2 = clFun(x = x, sg = sg, nSigma = 2, cl = "u" ,type = type)
lcl2 = clFun(x = x, sg = sg, nSigma = 2, cl = "l" ,type = type)
ucl1 = clFun(x = x, sg = sg, nSigma = 1, cl = "u" ,type = type)
lcl1 = clFun(x = x, sg = sg, nSigma = 1, cl = "l" ,type = type)

#shift limits (elementi restituiti)
iForLimits = iLimitsFun(i)
ucl3ForLimits = xLimitsFun(ucl3)
lcl3ForLimits = xLimitsFun(lcl3)

#Esecuzione Test e salvataggio (elementi restituiti) (a seconda che testCode sia o non sia nullo)
if (is.na(testType))
	{
		risultatiTest=list()
		risultatiTest$colorSet="#40f907" #verde positivo = col9
		risultatiTest$testMatrix=NULL
	}
else	
	{
		risultatiTest=testFun(x=x,sg=sg, type=type,  testType=testType, nSigma=nSigma, k=k,p=p)
	}

#ylim (elementi restituiti)
ylim = limitsFun(list(points, ucl3, lcl3))

#ylab (elementi restituiti)
ylab = ylabFun(xName, type = type)

#xlab text (depending individuals or groups)

xlab=ifelse(is.element(type,c("i","mr")),"individual values", "groups")

#lancio dell'utilita statistica

statistiche=statsFun(x=x,sg=sg, type=type)
#creazione delle sottoliste resituite
#lista con nome, tipo carte e statistiche

general=list()

general$chartType=type
general$xName=xName

#statistiche
general$numTot=statistiche$numTot
general$numNNmissing=statistiche$numNNmissing
general$numMissing=statistiche$numMissing
general$nGroupsX=statistiche$nGroupsX

general$meanX=statistiche$meanX
general$minX=statistiche$minX
general$maxX=statistiche$maxX

general$sdTotX=statistiche$sdTotX
general$sdWithinX=statistiche$sdWithinX
general$sdBetweenX=statistiche$sdBetweenX
general$meanRangeX=statistiche$meanRangeX
#
#una lista graphPars per i parametri grafici
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
# i colori sono comunque un argomento grafico anche se calcolati in
graphPars$colors=risultatiTest$colorSet

#creazione della lista dei risultati dei test
testResults=list()
testResults$testOutput=risultatiTest$testMatrix

#bindaggio e restutyzione dei risultati
spcObj=list(general=general, graphPars=graphPars, testResults=testResults, call=match.call())
class(spcObj) = "spc"
invisible(spcObj)
}
