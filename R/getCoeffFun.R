`getCoeffFun` <-
function(sampleSize, coeff)
{
n = length(sampleSize)
out = numeric(n)

for(i in 1:n)
{
#se ci sono solo 1 valore pone NA
if (sampleSize[i]<2)
out[i]=NA
else
out[i] = getParameterFun(sampleSize[i],coeff)
}
out
}

