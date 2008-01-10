`statsFun` <-
function(x,sg,type=type)
{
#generali
numTot=length(x) #output
numNNmissing=length(na.omit(x)) #output
numMissing=numTot-numNNmissing #outpuit
#medie e varianze totali
meanX=mean(x,na.rm=TRUE) #output
varTotX=var(x,na.rm=TRUE)
sdTotX=sqrt(varTotX) #output
devTotX=(varTotX)*(countFun(x)-1) # la varianza stimata e del csampione
#minimi e massimi
minX=min(x,na.rm=TRUE) #output
maxX=max(x,na.rm=TRUE) #output
#pone la varWithin uguale a zero inizialmente. E' definita solo per il moving range
sdWithinX=0 #output inizialmente posto a zero perche e deifinita per i sottogruppi
sdBetweenX=sdTotX # se la carte e per sottogruppi questo parametro sara opportunamente modificato, cosi come il precedente
rangeMedioX=NA #calcolata nelle funzioni specifiche
sdBetween=sdTotX

# i gruppi possono essere un vettore oppure un numero #riverificare
if (length(sg)>1)
{
if (!is.element(type, c("i","mr", "c", "p", "np", "u")))
{
nGroupsX=length(unique(sg)) # il numero dei gruppi e uguale a quello degli unici gruppi
}
else
{
nGroupsX=length(sg) #se e una carta di controllo per attributi allora viene posta a zero
}
}
else
{
nGroupsX=sg #carte per valori individuali
}
#statistiche per le carte per variabili
if(!is.element(type,c("i","mr", "c", "p", "np", "u")))
{
#queste formule devono essere discusse
vettoreMedie = tapply(x, sg, mean, na.rm = TRUE)
vettoreNi = tapply(x, sg, countFun)
vettoreVar = tapply(x, sg, var, na.rm=TRUE)

#se qualche gruppo ha dimensione 1 la sua dev. std e 0
vettoreVar[is.infinite(vettoreVar)]=0
vettoreVar[is.nan(vettoreVar)]=0
vettoreDevianze=(vettoreVar)*(vettoreNi-1)

#varWithin=somma devianze interne ai gruppi / (n-k)
varWithin=sum(vettoreDevianze)/(numNNmissing-nGroupsX)
sdWithinX=sqrt(varWithin) #restituire

#var Between = per differenza
varBetween=max(0,varTotX-varWithin)
sdBetweenX=sqrt(varBetween) #restituire
vettoreRanges=tapply(x,sg,rFun)
rangeMedioX=mean(vettoreRanges, na.rm=TRUE) #restituire
}
#le carte per attributi riportano il numero di difetti per dimensione del sottogruppo
# non si possono estratte informazioni sulla variabilita between
if (is.element(type,c("np", "p","u")))
{

#le statistiche dei due fenomeni sono uguali perch<c3><a9> in input sono dati sempre dati binomiali
if(type=="np" || type=="p")
{
#queste formule devono essere discusse
#selle carte np il numero di difetti x e una bin si distribuisce come una binomiale
probs=x/sg
varianzeGruppi=sg*probs*(1-probs)
#classiche scomposizioni basate sull'assunzione di binomialita all'interno dei sottogruppi
#vanno verificate perch<e non sommano ad 1
varWithin=mean(varianzeGruppi,na.rm=TRUE)
varBetween=var((sg*probs),na.rm=TRUE)
sdWithinX=sqrt(varWithin)
sdBetweenX=sqrt(varBetween)
#poniamo il range medio a maxX-minX
rangeMedioX=maxX-minX
}
else #carte u
{
#queste formule devono essere discusse e verificate
#ipotesi poissoniana
varianzeGruppi=x #e(x) = var(x)=ni>*lamda (teorico)
varWithin=mean(varianzeGruppi,na.rm=TRUE)
varBetween=var(x,na.rm=TRUE)
sdWithinX=sqrt(varWithin)
sdBetweenX=sqrt(varBetween)
#poniamo il range medio a maxX-minX
rangeMedioX=maxX-minX
}
}


#carte a valori individuali
if (is.element(type,c("i", "mr","c")))
{
movingrange=mean(mrangeFun(x),na.rm=TRUE)
rangeMedioX=movingrange
}

statisticsList=list()
#generali
statisticsList$numTot=numTot
statisticsList$numNNmissing=numNNmissing
statisticsList$numMissing=numMissing
statisticsList$nGroupsX=nGroupsX
#posizione
statisticsList$meanX=meanX
statisticsList$minX=minX
statisticsList$maxX=maxX
#variabilita
statisticsList$sdTotX=sdTotX
statisticsList$sdWithinX=sdWithinX
statisticsList$sdBetweenX=sdBetweenX
#rangeMedioX è italiano!!!
statisticsList$meanRangeX=rangeMedioX
invisible(statisticsList)
}

