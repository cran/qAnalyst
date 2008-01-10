`summary.spc` <-
function(object, ...)
{
########################################
#titolazione
########################################
spcObj = object
titolo=paste(spcObj$general$chartType, " chart of ", spcObj$general$xName, sep="")
print(titolo)

#########################################
#stampa statistiche generali
##########################################
cat("\n",spcObj$general$xName, " main stats", "\n", "------------------------------------------","\n")

namesGenStats=c("Total observations", "complete observations", "missing observations", "number of groups",
"Mean", "min", "max","total std. dev.", "within std. dev.", "between std. dev.", "average range")
#prepares general statistics table
genStats=c(spcObj$general$numTot, spcObj$general$numNNmissing, spcObj$general$numMissing, spcObj$general$nGroupsX,
spcObj$general$meanX, spcObj$general$minX, spcObj$general$maxX,
spcObj$general$sdTotX, spcObj$general$sdWithinX, spcObj$general$sdBetweenX, spcObj$general$meanRangeX)
genStats=as.matrix(genStats)
#naming and printing
colnames(genStats)=c("value")
rownames(genStats)=c(namesGenStats)
print(genStats)

########################################
#control chart test results
########################################

cat("\n", "Control chart tests results", "\n", "------------------------------------------","\n")
if (!is.null(spcObj$testResults$testOutput))
{
	cat("\n", "Matrix of points failing required tests", "\n")
		#caso un test effettuato e un punto fallito
		if (class(spcObj$testResults$testOutput)=="numeric")
			print(spcObj$testResults$testOutput)
		else
		{
		if (dim(spcObj$testResults$testOutput)[1]==0)
			cat("\n","All tests successful","\n")
		else
			print (spcObj$testResults$testOutput)
		}
}
else
{
	cat("No tests have been required on analyzed data set", "\n")
}

############################################
#control chart graphical elements coordinates
############################################

cat("\n","Control chart elements table", "\n", "------------------------------------------", "\n")
centro=rep(spcObj$graphPars$center,spcObj$general$nGroupsX)
punti=as.vector(spcObj$graphPars$points)
lcl3s=as.vector(spcObj$graphPars$lcl3)
lcl2s=as.vector(spcObj$graphPars$lcl2)
lcl1s=as.vector(spcObj$graphPars$lcl1)
ucl1s=as.vector(spcObj$graphPars$ucl1)
ucl2s=as.vector(spcObj$graphPars$ucl2)
ucl3s=as.vector(spcObj$graphPars$ucl3)

dfTableCarta=cbind(punti,lcl3s, lcl2s, lcl1s, centro, ucl1s, ucl2s, ucl3s)
dfTableCarta=as.data.frame(dfTableCarta)
names(dfTableCarta)=c("points", "lcl3s", "lcl2s", "lcl1s", "center line", "ucl1s", "ucl2s", "ucl3s")
print(dfTableCarta)
invisible(NULL)
}
