`clFun` <-
function(x, sg, nSigma, cl ,type = "xbar")
{
#da' uno stop se la linea richiesta non esiste
if(!is.element(cl, c("u", "l"))) {stop("Error! cl must be either u or l")}
#Xbar Chart
if(type == "xbar")
{
xbar = tapply(x, sg, mean, na.rm = TRUE)
#tiene conto dei valori mancanti
xbar[is.nan(xbar)]=NA

xbarbar = mean(x, na.rm = TRUE)
sgSize = as.numeric(tapply(x, sg, countFun))
#attenzione ai limiti di confidenza. Se la dimensione media dei
#sottogruppi sta sotto 7 allora usiamo R per stimare la variabilita altrimenti usiamo S
sgMean=mean(sgSize,na.rm=TRUE)
#caso uno: uso R per la variabilita
if (sgMean<7)
{
r = tapply(x, sg, rFun)
rbar = mean(r,na.rm=TRUE)
d2 = getCoeffFun(sgSize, "d2")
#restituisce il vettore dei limits
if(cl == "u") clout = xbarbar + (nSigma*rbar)/(d2*sqrt(sgSize))
if(cl == "l") clout = xbarbar - (nSigma*rbar)/(d2*sqrt(sgSize))
}
#caso due: uso S perla  variabilita
else
{
sbar=centerFun(x=x,sg=sg,type="s")
A3 = getCoeffFun(sgSize, "A3")
if(cl == "u") clout = xbarbar + (nSigma*(A3/3))*sbar
if(cl == "l") clout = xbarbar - (nSigma*(A3/3))*sbar
}

}
#R Chart
if(type=="r")
{
xbar = tapply(x, sg, mean, na.rm = TRUE)
xbar[is.nan(xbar)]=NA
r = tapply(x, sg, rFun)
#verificare stimatore rbar numerosita campionaria variabile
r[is.nan(r)]=NA
rbar = mean(r,na.rm=TRUE)
xbarbar = mean(x, na.rm = TRUE)
sgSize = tapply(x, sg, countFun)
#da Montgomery pp 157 in fondo sostituendo a 3 nSigma
d3 = getCoeffFun(sgSize, "d3")
d2 = getCoeffFun(sgSize, "d2")

D4 = (1+nSigma*d3/d2)
D3 = (1-nSigma*d3/d2)
UCL=D4*rbar
LCL=ifelse(D3*rbar>0,D3*rbar,0)

#limits
if(cl == "u") clout = UCL
if(cl == "l") clout = LCL

}
#S Chart
if(type=="s")
{
#bisogna innanziutto stimare SBAR. Riprendo codice da central line
sdCampionari= tapply(x,sg,sd,na.rm=TRUE)
sdCampionari[is.na(sdCampionari)]=NA
sgSize = tapply(x,sg,countFun)
#formula montgomery pp 189
numFormula=sum(sdCampionari^2*(sgSize-1),na.rm=TRUE)
m=length(unique(sg))
denFormula=sum(sgSize,na.rm=TRUE)-m
sbar=(numFormula/denFormula)^0.5
#Montgomery da' formule solo per 3 sigma. riadatto le costanti
# B3 e B4 sostituendo a tre nSigma
c4 = getCoeffFun(sgSize, "c4")
costSup=(1+(nSigma/c4)*(sqrt(1-c4^2)))
costInf=(1-(nSigma/c4)*(sqrt(1-c4^2)))
#limits
if(cl == "u") { clout=sbar*costSup; clout=ifelse(clout>0,clout,0)}
if(cl == "l") { clout=sbar*costInf; clout=ifelse(clout>0,clout,0)}
}
if(type=="i")
{
#ricalcola la linea centrale e il moving range
center=mean(na.omit(x))
#MR=centerFun(x=x,sg=sg,type="mr",interval=interval) old
#calcola i limiti centrali
MR=centerFun(x=x,sg=sg,type="mr") 
UCL=center+nSigma*MR
LCL=center-nSigma*MR
if(cl == "u") clout=rep(UCL,length(x))
if(cl == "l") clout=rep(LCL,length(x))
}
if(type=="mr")
{
#ricalcola la linea centrale
#vedere sopra per quando si usava interval
MR=centerFun(x=x,sg=sg,type="mr")
#calcola i limiti centrali
d2 = getCoeffFun(sg+1, "d2")
d3 = getCoeffFun(sg+1, "d3")

UCL=MR*d2+nSigma*d3*MR
LCL=MR*d2-nSigma*d3*MR
LCL=ifelse(LCL<0,0,LCL)

if(cl == "u") clout=rep(UCL,length(x))
if(cl == "l") clout=rep(LCL,length(x))

}
if(type=="p")
{
#calcola la linea centrale
pbar=centerFun(x=x,sg=sg,type="p")
#calcola i limiti centrali
UCL=pbar+nSigma*sqrt((pbar*(1-pbar))/sg)
LCL=pbar-nSigma*sqrt((pbar*(1-pbar))/sg)
#introduce anche un controllo x la non negativita
LCL=ifelse(LCL<0,0,LCL)
if(cl == "u") clout=UCL
if(cl == "l") clout=LCL
}
if(type=="np")
{
#ricalcola la linea centrale. necessita sia di pbar
#che di npbar
pbar=centerFun(x=x,sg=sg,type="p")
npbar=centerFun(x=x,sg=sg,type="np")
#calcola i limiti centrali
UCL=npbar+nSigma*sqrt(sg*pbar*(1-pbar))
LCL=npbar-nSigma*sqrt(sg*pbar*(1-pbar))
#introduce anche un controllo x la non negativita
LCL=ifelse(LCL<0,0,LCL)
if(cl == "u") clout=UCL
if(cl == "l") clout=LCL
}
if(type=="c")
{
cline=centerFun(x=x,sg=sg,type="c")
UCL=cline+nSigma*sqrt(cline)
LCL=cline-nSigma*sqrt(cline)
LCL=ifelse(LCL<0,0,LCL)
if(cl == "u") clout=rep(UCL,length(x))
if(cl == "l") clout=rep(LCL,length(x))
}
if(type=="u")
{
cline=centerFun(x=x,sg=sg,type="u")
UCL=cline+nSigma*sqrt(cline/sg)
LCL=cline-nSigma*sqrt(cline/sg)
LCL=ifelse(LCL<0,0,LCL)
if(cl == "u") clout=UCL
if(cl == "l") clout=LCL
}
#restituisce il vettore dei limiti
return(clout)
}

