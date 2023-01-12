#Question 2 - description 
#To decide on how to market their new Apple juice concentrate product, the marketing manager
#creates advertising that emphasizes convenience in city 1 and quality in city 2 (for the data sets A2
#and A3 emphasis was on convenience and price and quality and price, respectively). The number
#of packages sold weekly was recorded for each city.

mydata = read.table("F:\\sta3030 - Project 1\\A1.txt",header= TRUE)
mydata

#summary stats for city1
city1=mydata$City1
summary(city1)
city2 =mydata$City2
summary(city2)

#combine the two samples into one 20 piece sample
combined = c(city1,city2)

#create bootstraps of the combined and then store the means of the bootstraps in the bootstrap mean vectors
city1B <-vector(mode="numeric", length = 5000)
city2B <-vector(mode="numeric", length = 5000)
cityDiff <-vector(mode="numeric", length = 5000)
for(i in 1:5000){
  samp = sample(combined, size = 20, replace = TRUE)
  city1B[i] = mean(samp[1:10])
  city2B[i] = mean(samp[11:20])
  cityDiff[i] = city1B[i]-city2B[i]
}

sortCityDiff =sort(cityDiff)

#2A - hypothesis testing
#H0: u1-u2=0; H1:u1-u2 doesn't equal 0
# therefore sampling error is: smean1-smean2

samplingErr = mean(city1)-mean(city2)-0
#-samplingErr < mean(city1)-mean(city2)-0 <samplingErr
#because smean(1-2)-u(1-2)~bmean(1-2)-smean(1-2) => this is the bootstrap assumption
#therefore -samplingErr<bmean(1-2)<samplingErr
#pval test is Pr[bmean(1-2)<-samplingErr] + Pr[bmean(1-2)>samplingErr]
#find bmean less than and greater than -samplingErr & samplingErr

#following returns the indexes of these values
greaterThan = match((samplingErr*-1), sortCityDiff)
lessThan = match(samplingErr, sortCityDiff)
lessThan
greaterThan

pval = ((5000-greaterThan)+(lessThan))/5000
pval

#2B
#95% confint - find index of (5000*.025) & (5000*.975)
#lowBound<bmean(1-2)<uppBound
#lowBound<smean(1-2)- u(1-2)<uppBound
#-uppBound<u(1-2)-smean(1-2)<-lowBound
#therefore: -uppBound+smean(1-2)<u(1-2)<-lowBound+smean(1-2)
lowBound = sortCityDiff[5000*.025]
uppBound = sortCityDiff[5000*.975]
PlowBound = (uppBound*-1)+(mean(city1)-mean(city2))
PuppBound = (lowBound*-1)+(mean(city1)-mean(city2))
confi = cat("(",PlowBound,",",PuppBound,")")

#2C - testing equality of variances
#H0: var1=Var2, H1:var1 doesnt = var2
#calc fstats and save only the fstats
cityFs <-vector(mode="numeric", length = 5000)
for(i in 1:5000){
  samp = sample(combined, size = 20, replace = TRUE)
  city1v= c(samp[1:10])  
  varCity1v = var(city1v) #calculate variance of bootstrap of city1
  city2v= c(samp[11:20])
  varCity2v=var(city2v)   #calculate variance of bootstrap of city2
  cityFs[i] = varCity1v/varCity2v  #calculate Fstat and retain that
}

sampleFstat = (var(city1))/(var(city2))
sortedFs = sort(cityFs)
hist(sortedFs)
bound = 0
for(i in 1:5000){
  if(sortedFs[i]>=sampleFstat){
    bound = i
    break
  }
}
bound
pvalV= (5000-bound)/5000





