boxcoxFun<-function(x)
{
	#check if all values are positive
	checkPositivity=ifelse(x>0,TRUE,FALSE)
	if (!all(checkPositivity)) stop("Error! All x must be positive") 
	#creates bc obje by bctrans1 of alr3 package
	bcObj=bctrans1(X=x,family="box.cox")
	lambda=coef(bcObj)
	y=powtran(bcObj, lambda=coef(bcObj), family=bcObj$family,modified=FALSE)
	y=y[!is.na(y)]
	outList=list(type="boxcox", parameters=lambda, original=x,transformed=as.numeric(y))
	class(outList)="transformation"
	invisible(outList)
}
