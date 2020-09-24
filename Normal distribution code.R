df=read.table(file.choose(), stringsAsFactors = FALSE,header = FALSE,sep = ";")
df
population=10000000
x=rchisq(population, 2)
hist(x, col='blue', freq= FALSE, main=('Histogram for chi-distributions with 2 degrees of freedom'))

xBarVec=c()
population1=rnorm(10000000, mean=2, sd=2)
xbarGenerator = function(sampleSize, number_of_samples)
{
  for (i in 1:number_of_samples)
  {
    theSample = sample(population1, sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}

xbars=xbarGenerator(50,10000)
length(xbars)
hist(xbars, col='red', main='Distribution of the sample mean: n = 50')
sd(xbars)
summary(xbars)

