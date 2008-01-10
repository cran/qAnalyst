print.spc <-
function(x, ...)
{
spcObj = x
titolo=paste(spcObj$general$chartType, " chart of ", spcObj$general$xName, sep="")
print(titolo)
#stampa statistiche generali

cat("\n",spcObj$general$xName, " main stats", "\n", "------------------------------------------","\n")

namesGenStats=c("Total observations", "complete observations", "missing observations", "number of groups",
		"Mean", "min", "max","total std. dev.", "within std. dev.", "between std. dev.", "average range")
			
genStats=c(spcObj$general$numTot, spcObj$general$numNNmissing, spcObj$general$numMissing, spcObj$general$nGroupsX,
		spcObj$general$meanX, spcObj$general$minX, spcObj$general$maxX,
		spcObj$general$sdTotX, spcObj$general$sdWithinX, spcObj$general$sdBetweenX, spcObj$general$meanRangeX)
#¢rea una matrice
genStats=as.matrix(genStats)
colnames(genStats)=c("value")
rownames(genStats)=c(namesGenStats)
print(genStats)
}
