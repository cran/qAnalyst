`testFun` <- function(x, sg, type, testType , k, p,  nSigma)
{
###########################################################
#verifies that test are between one and eight
if (!all(is.element(testType, 1:8)))
{
stop ("Error! Test must be between 1 and 8")
}

###########################################################
#uncheck down if all parameters must be specified. They are passed as c(NA) default. Lenght c(NA)==1
#if ((all(c(length(k)==1, length(p)==1, length(nSigma)==1))) && any(c(is.na(k), is.na(p), is.na(nSigma)))) stop("Error! Parameters must be specified")

howManyTests=length(testType)

#if more than one test and parameters unspecificed (one dimension vector NA)
#return length(testType) vector of parameters

if ((howManyTests>1) && all(c(length(nSigma), length(k), length(p)) == 1 )) {
	filler=rep(NA,howManyTests)
	#fills nSigma
	filler[1]=nSigma
	nSigma=filler
	#fills k
	filler[1]=k
	k=filler
	#fills p
	filler[1]=p
	p=filler
}

#checks parameters coherence
k = ifelse(is.na(k), 0, k)
p = ifelse(is.na(p), 0, p)
nSigma = ifelse(is.na(nSigma), 0, nSigma)

#if more than one tests, parameters shall be of same length
if (!all(c(length(nSigma), length(k), length(p)) == length(testType))) 
{
stop("Error! testType, k, p and nSigma must be of same length")
}
############################################################
#Ordering
ord = order(testType, decreasing = TRUE)
testType = testType[ord]
k=k[ord]
p=p[ord]
nSigma=nSigma[ord]
############################################################
#Color Legend
col9="#40f907" #brilliant green (all ok)
col8="#3f8c27" #green dark
col7="#4b6111" #green olive
col6="#b8f11f" #green ugly
col5="#d2af23" #green yellow
col4="#fdd600" #yellow
col3="#f8650d" #orange
col2="#7b0303" #dark red
col1="#fa0909" #red (very wrong)
colorLegend= c(col1, col2, col3, col4,col5, col6, col7, col8, col9)
################################################################
#Mat of all passed tests ... initial state
points = pointsFun (x = x, sg = sg, type = type)
#initial matrix to archive results
matrixTest = matrix(0, ncol = length(testType), nrow = length(points))
############################################################
############################################################
############################################################

#Define internal function
.testFun = function(x, sg, testType, type , k, p, nSigma, points = points){
#############################################################
#procedure di inizializzazione generali
n = length(points)
center = centerFun (x = x, sg= sg, type = type)
center= rep(center,n)
############################################################
# test 1: k points on k beyond zone A
# default values k=1 p=1 nSigma=3
############################################################
if ( testType == 1)
{
nSigma1=ifelse(nSigma==0,3,nSigma)
upper =  clFun (x=x, sg=sg, nSigma = nSigma1, cl="u" ,type = type)
lower =  clFun (x=x, sg=sg, nSigma = nSigma1, cl="l" ,type = type)
k = ifelse(k == 0, 1, k)
p = ifelse(p == 0, k, p)

if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p shall be not lower than k")}
out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
testUpper = ifelse ( sum(ifelse(points[tested] > upper[tested], 1, 0)) >= k, 1, 0)
testLower = ifelse( sum(ifelse(points[tested] < lower[tested], 1, 0)) >=k, 1, 0)
test = max(testUpper, testLower)
out[i] = test
}

}
############################################################
# test 2: k points in a row on same side of centre line
# default values  k=9 p=9 nSigma = 0
############################################################
if (testType == 2)
{

k = ifelse(k == 0, 9, k)
p = ifelse(p == 0, k, p)
if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p shall be not lower than k")}
out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
testUpper = ifelse ( sum(ifelse(points[tested] > center[tested], 1, 0)) >= k, 1, 0)
testLower = ifelse( sum(ifelse(points[tested] < center[tested], 1, 0)) >=k, 1, 0)
test = max(testUpper, testLower)
out[i] = test
}

}
############################################################
# test 3: k points on p in a row all increasing all decreasing
# default values k=p=6
############################################################
if (testType == 3)
{
k = ifelse(k == 0, 6, k)
p = ifelse(p == 0, k, p)
if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p shall be not lower than k")}
out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
test = ifelse(abs(sum(sign(diff(points[tested]))))==k-1,1,0)
out[i] = test
}

}
############################################################
# test 4: at least k on p points in a row all up and down
#Default k = 14, p = 14
############################################################
if (testType ==4)
{
k = ifelse(k == 0, 14,k)
p = ifelse(p == 0, 14,p)
if(p <5){stop("Error! p must be greater than 4")}
if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p must be greater or equal to k")}

out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
zerone = rep.int(c(0,1), n)
length(zerone) = length(tested)-1
onezero = rep.int(c(1,0), n)
length(onezero) = length(tested)-1
test = ifelse(diff(points[tested]) > 0 , 1, 0)
test = ifelse(sum(test - zerone) == 0 | sum(test - onezero) == 0 , 1, 0)
out[i] = test
}

}
############################################################
# test 5: k out of p point in a row beyond zone A (nSigma=3)
# default k=2 p=3 nSigma = 3
############################################################
if (testType == 5)
{
nSigma5=ifelse(nSigma==0,3,nSigma)
k = ifelse(k == 0, 2, k)
p = ifelse(p == 0, k+1, p)
upper =  clFun (x=x, sg=sg, nSigma = nSigma5, cl="u" ,type = type)
lower =  clFun (x=x, sg=sg, nSigma = nSigma5, cl="l" ,type = type)

if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p shall be not lower than k")}
out = rep(0, n)


for( i in p:n)
{
tested = ((i-p+1):i)

testUpper = ifelse ( sum(ifelse(points[tested] > upper[tested], 1, 0)) >= k, 1, 0)
testLower = ifelse( sum(ifelse(points[tested] < lower[tested], 1, 0)) >=k, 1, 0)
test = max(testUpper, testLower)
out[i] = test
}
}
############################################################
# test 6 k out of p points in a row in zone B or beyond
# (one side of center line)
#  default k=4 p = 5 nSigma=2
############################################################
if (testType == 6)
{
nSigma6=ifelse(nSigma==0,2,nSigma)
k = ifelse(k == 0, 4, k)
p = ifelse(p == 0, k+1, p)
upper =  clFun (x=x, sg=sg, nSigma = nSigma6, cl="u" ,type = type)
lower =  clFun (x=x, sg=sg, nSigma = nSigma6, cl="l" ,type = type)

if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! be not lower than k")}
out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
testUpper = ifelse(sum(ifelse(points[tested] > upper[tested], 1, 0)) >= k, 1, 0)
testLower = ifelse(sum(ifelse(points[tested] < lower[tested], 1, 0)) >=k, 1, 0)
test = max(testUpper, testLower)
out[i] = test
}
}

############################################################
# test 7
#k points on p in a row within zone C (both sides of centre line)
# p=k=15 nSigma=1
############################################################
if (testType==7)
{
#nSigma per questo test dovrebbe essere 1! Il codice e uguale a sopra
nSigma7=ifelse(nSigma==0,1,nSigma)
upper =  clFun (x=x, sg=sg, nSigma = nSigma7, cl="u" ,type = type)
lower =  clFun (x=x, sg=sg, nSigma = nSigma7, cl="l" ,type = type)
k = ifelse(k == 0, 15, k)
p = ifelse(p == 0, k, p)

if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p shall be not lower than k")}
out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
test = sum(ifelse(((points[tested] >= lower[tested]) &  (points[tested] <= upper[tested])) , 1, 0))
test = ifelse(test >= k , 1, 0)
out[i] = test
}
}
############################################################
# test 8
# k points in a row beyond zone C (both sides of central line)
#  p=k=8 nSigma=1
############################################################
if (testType==8)
{
nSigma8=ifelse(nSigma==0,1,nSigma)
upper =  clFun (x=x, sg=sg, nSigma = nSigma8, cl="u" ,type = type)
lower =  clFun (x=x, sg=sg, nSigma = nSigma8, cl="l" ,type = type)
k = ifelse(k == 0, 8, k)
p = ifelse(p == 0, k, p)
if(p > n){stop("Error! p must be lower or equal than n")}
if(p < k){stop("Error! p shall be not lower than k")}
out = rep(0, n)

for( i in p:n)
{
tested = ((i-p+1):i)
test = sum(ifelse(points[tested] > upper[tested] | points[tested] < lower[tested] , 1, 0))
test = ifelse(test >= k , 1, 0)
out[i] = test
}

}
############################################################
#where NA 0
out=ifelse(is.na(out),0,out)
invisible(out)
}
############################################################
#End Define internal function
############################################################
############################################################
############################################################

#execute each of defined test in testType and saves results  in tcolumns of matrixTest
for (i in 1:length(testType)){
matrixTest[,i] = .testFun(x, type = type, testType = testType[i], k=k[i], p=p[i] , sg = sg, nSigma=nSigma[i], points = points)
}
#coluns naming
dimnames(matrixTest) = list(NULL, paste("Test", testType, sep = ""))
#creates a matrix with each rows containigns test codes
colMat = matrix(testType, ncol = length(testType), nrow = length(points), byrow = TRUE)
colMat = matrixTest*colMat
colMat[colMat == 0] = 9
#where a test does not fail code equal 9
colVec = apply(colMat, 1, min)
#the color index of colorLegend is defined as minimum color for each rows in mtatrixTest
color = character(length(points))
for ( i in 1:length(points)){
color[i] = colorLegend[colVec[i]]
}

#creates response matrix
matrixTest = cbind(index = 1:length(points), matrixTest)[apply(matrixTest, 1, max) > 0,]
#list(color, matrixTest)
responseList=list(colorSet=color,testMatrix=matrixTest)
return(responseList)
}






