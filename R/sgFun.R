`sgFun` <-
function(x,sg,type)
{
num=length(x)
lensg=length(sg)
#controllo dimensione
if(lensg>1 && (length(x)!=lensg)) stop("if sg is a vector, it shall have the same dimension of x")
#controllo NA
if (lensg==1 && is.na(sg)) stop("sg must be specified")

#carte a valori individuali tranne mr
if (is.element(type,c("i", "mr")))
{
	#sg coingice con il moving range restituito uguale
	sg=sg
}
#
if (type=="c")
{
	#i sottogruppi sono valori individuali
	sg=rep(1,length(x))
}
#carte per attributi
if(is.element(type,c("p", "np", "u")))
{
if (lensg==1) sg=rep(sg,num)
}
#carte a valori continui
if(is.element(type,c("xbar", "r", "s")))
{
if (lensg==1)
{
	
	dimens=sg
	#quanti sono i sottogruppi
	ns=ceiling(num/dimens)
	#replica da uno a ns ciascuno max dimens per un totale di num elementi
	sg=rep(1:ns, each=dimens, len=num)
}
}

return(sg)
}
