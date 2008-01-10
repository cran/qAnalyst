`centerFun` <-
function(x, sg, type = "xbar")
{
dfTemp=cbind(x,sg)
#xbar linea centrale
if(type == "xbar")
{
center = mean(x, na.rm = TRUE)
}
#Montgomery nn da' stimatorie linea centrale per dimensione variabile
#verificare dopo la bonta della formula
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
#formula montgomery pp 189
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
#preso da minitab
#sg è l'ampiezza del range
center=mean(na.omit(mrangeFun(x,sg=sg)))
d2=getCoeffFun(sg+1, "d2")
#come vuole minitab
center=center/d2
}
#p chart
if(type=="p")
{
sumX=sum(na.omit(x))
#controlla: ok perche nelle carte per attributi sg risulta un vettore contente
#la dimensione del difetto
sumN=sum(na.omit(sg))
pLine=sumX/sumN
center=pLine
}
#np chart
if(type=="np")
{
#pongo la linea centrale come mean(x). Sul montgomery nn esiste
#controllare anche su minitab. Ma pare che su internet la linea
#sia costante. Quindi x ora mean
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
#controlla
sumN=sum(na.omit(sg))
uLine=sumX/sumN
center=uLine
}

return(center)
}

