`mrangeFun` <-
function(x=x, sg=1)
{
if (length(x) <=sg) stop("too wide specified lag interval")
out1=diff(x,lag=sg)
#ci mette il valore assoluto
out1=abs(out1)
#tiene conto che l'output deve avere tanti NA quanto <c3><a8> il lag iniziale
initials=rep(NA,sg)
#li concatena e re
out=c(initials,out1)
return(out)
}

