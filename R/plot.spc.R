`plot.spc` <-
function(x,...)
{
spcObj = x
#graphical settings
# hStrip, cexStrip, cexScales, cexAxes=3 
hStrip=2
cexStrip=2
cexAxes=2
cexScales=1.5  # Modified by pgo for graph readability
cexPoints=1.5
texcexStrip=paste(spcObj$general$chartType,"chart of ", spcObj$general$xName)
options(warn=-1)
#create the lattice object

# pgo: Modification to show the labels in y axis
limSupY=max(spcObj$graphPars$ucl3ForLimits,na.rm=TRUE)
limInfY=min(spcObj$graphPars$lcl3ForLimits,na.rm=TRUE)

extPointsInf=min(c(spcObj$graphPars$points,limInfY),na.rm=TRUE) # Points outside limits (<LCL)
extPointsSup=max(c(spcObj$graphPars$points,limSupY),na.rm=TRUE) # Points outside limits (<UCL)

difLimY=limSupY-limInfY

scaleValsY=unique( round(c(seq.int(from=limInfY,to=limSupY,length.out=7),spcObj$graphPars$center,extPointsInf,extPointsSup),digits=max(c(1,trunc(-log10(difLimY))+2))) )

# pgo: end modification

trellis.par.set('layout.heights', list (strip = hStrip))
pl = xyplot(spcObj$graphPars$points~spcObj$graphPars$i | texcexStrip, ylim = spcObj$graphPars$ylim,
ylab = list(spcObj$graphPars$ylab, cex = cexAxes), xlab = list(spcObj$graphPars$xlab, cex = cexAxes),
strip = strip.custom (par.strip.text = list(cex = cexStrip, col = "blue")),
scales = list(cex=cexScales, x = list(relation = "free"),y=list(at=scaleValsY,labels=as.character(scaleValsY))), # pgo: added "labels" and "at" to show the labels in y axis

panel = function(x, y, ...)
{
panel.lines(spcObj$graphPars$iForLimits, spcObj$graphPars$ucl3ForLimits, col = "grey", lwd = 6)
panel.lines(spcObj$graphPars$iForLimits, spcObj$graphPars$lcl3ForLimits, col = "grey", lwd = 6)
panel.abline(h = spcObj$graphPars$center, col = "grey", lwd = 10)
panel.lines(x, y, col = "blue", lwd = 3, ...)
panel.xyplot(x, y, col = spcObj$graphPars$color, pch = 16, cex = cexPoints,...)
})
#print the object via lattice
print(pl)
invisible(NULL)
}

