`centerFun` <-
function(x, sg, type = "xbar")
{
dfTemp=cbind(x,sg)
#xbar central line
if(type == "xbar")
{
center = mean(x, na.rm = TRUE)
}

#r chart
if(type=="r")
{
rangesCampionari= tapply(x,sg,rFun)
rangesCampionari[is.nan(rangesCampionari)]=NA
center=mean(rangesCampionari,na.rm=TRUE)
}
#s chart
if(type=="s")
{
sdCampionari= tapply(x,sg,sd,na.rm=TRUE)
sdCampionari[is.nan(sdCampionari)]=NA
sgSize = tapply(x,sg,countFun)
# montgomery pp 189
numFormula=sum(sdCampionari^2*(sgSize-1),na.rm=TRUE)
m=length(unique(sg))
denFormula=sum(sgSize)-m
center=(numFormula/denFormula)^0.5
}
#i chart
if(type=="i")
{
center=mean(na.omit(x))
}
#mr chart
if(type=="mr")
{
#sg is range length
center=mean(na.omit(mrangeFun(x,sg=sg)))
d2=getCoeffFun(sg+1, "d2")
#come vuole
center=center/d2
}
#p chart
if(type=="p")
{
sumX=sum(na.omit(x))
#in attributes chart sg is a vector contsaining
#sample dimension
sumN=sum(na.omit(sg))
pLine=sumX/sumN
center=pLine
}
#np chart
if(type=="np")
{
#central line equal mean(x). No references on montgomery
#x equal
center=mean(na.omit(x))
}
#c chart
if(type=="c")
{
center=mean(na.omit(x))
}
#u chart
if(type=="u")
{
sumX=sum(na.omit(x))
#checks
sumN=sum(na.omit(sg))
uLine=sumX/sumN
center=uLine
}

return(center)
}

