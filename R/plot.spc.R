`plot.spc` <-
function(x,...)
{
spcObj = x
#parametri grafici
hStrip=1.5
tStrip=1.5
tAxes=2
tScales=2
textStrip=paste(spcObj$general$chartType,"chart of ", spcObj$general$xName)
options(warn=-1)
#print the object

trellis.par.set('layout.heights', list (strip = hStrip))
pl = xyplot(spcObj$graphPars$points~spcObj$graphPars$i | textStrip, ylim = spcObj$graphPars$ylim,
ylab = list(spcObj$graphPars$ylab, cex = tAxes), xlab = list(spcObj$graphPars$xlab, cex = tAxes),
strip = strip.custom (par.strip.text = list(cex = tStrip, col = "blue")),
scales = list(cex=tScales, x = list(relation = "free")),

panel = function(x, y, ...)
{
panel.lines(spcObj$graphPars$iForLimits, spcObj$graphPars$ucl3ForLimits, col = "grey", lwd = 3)
panel.lines(spcObj$graphPars$iForLimits, spcObj$graphPars$lcl3ForLimits, col = "grey", lwd = 3)
panel.abline(h = spcObj$graphPars$center, col = "grey", lwd = 3)
panel.lines(x, y, col = "blue", lwd = 3, ...)
panel.xyplot(x, y, col = spcObj$graphPars$color, pch = 16, cex = 1,...)
})
#stampa l'oggetto
print(pl)
invisible(NULL)
}

