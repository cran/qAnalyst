`clFun` <-
function(x, sg, nSigma, cl ,type = "xbar")
{
#if not "u" or "l" stops
if(!is.element(cl, c("u", "l"))) {stop("Error! cl must be either u or l")}
#Xbar Chart
if(type == "xbar")
{
xbar = tapply(x, sg, mean, na.rm = TRUE)
#considers missing value
xbar[is.nan(xbar)]=NA
xbarbar = mean(x, na.rm = TRUE)
#waring is sg<2
sgSize = as.numeric(tapply(x, sg, countFun))
#care confidence limits
#sg below 7 R else sg
sgMean=mean(sgSize,na.rm=TRUE)
#sg below 8
if (sgMean<7)
{
r = tapply(x, sg, rFun)
rbar = mean(r,na.rm=TRUE)
d2 = getCoeffFun(sgSize, "d2")
#returns limits vecotr
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
#check estimator r bar variable sample dim
r[is.nan(r)]=NA
rbar = mean(r,na.rm=TRUE)
xbarbar = mean(x, na.rm = TRUE)
sgSize = tapply(x, sg, countFun)
#da Montgomery pp 157  where 3 = nSigma
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
#SBAR estimation
sdCampionari= tapply(x,sg,sd,na.rm=TRUE)
sdCampionari[is.na(sdCampionari)]=NA
sgSize = tapply(x,sg,countFun)
#formula montgomery pp 189
numFormula=sum(sdCampionari^2*(sgSize-1),na.rm=TRUE)
m=length(unique(sg))
denFormula=sum(sgSize,na.rm=TRUE)-m
sbar=(numFormula/denFormula)^0.5
#Montgomery uses 3 sigma
# here we use B3 and B4
c4 = getCoeffFun(sgSize, "c4")
costSup=(1+(nSigma/c4)*(sqrt(1-c4^2)))
costInf=(1-(nSigma/c4)*(sqrt(1-c4^2)))
#limits
if(cl == "u") { clout=sbar*costSup; clout=ifelse(clout>0,clout,0)}
if(cl == "l") { clout=sbar*costInf; clout=ifelse(clout>0,clout,0)}
}
if(type=="i")
{
#recalculates moving range
center=mean(na.omit(x))
#MR=centerFun(x=x,sg=sg,type="mr",interval=interval) old
#calculates center line
MR=centerFun(x=x,sg=sg,type="mr") 
UCL=center+nSigma*MR
LCL=center-nSigma*MR
if(cl == "u") clout=rep(UCL,length(x))
if(cl == "l") clout=rep(LCL,length(x))
}
if(type=="mr")
{
#recalculates center kline
#see when interval is used
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
#calculates center line
pbar=centerFun(x=x,sg=sg,type="p")

UCL=pbar+nSigma*sqrt((pbar*(1-pbar))/sg)
LCL=pbar-nSigma*sqrt((pbar*(1-pbar))/sg)
#non negativity
LCL=ifelse(LCL<0,0,LCL)
if(cl == "u") clout=UCL
if(cl == "l") clout=LCL
}
if(type=="np")
{
#calculates pbar
#for npbar
pbar=centerFun(x=x,sg=sg,type="p")
npbar=centerFun(x=x,sg=sg,type="np")
#calculates UCL
UCL=npbar+nSigma*sqrt(sg*pbar*(1-pbar))
LCL=npbar-nSigma*sqrt(sg*pbar*(1-pbar))
#nn negativity
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
#outs results
return(clout)
}

