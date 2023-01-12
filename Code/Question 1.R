#nzmfez001 - Q1 - index 118
#           Q2 & 3 A1 aov5

mydata = read.table("C:\\Users\\student\\Documents\\sta3030\\incexp.txt", header = TRUE)
age = mydata$AGE[118:(118+99)]
hhinc = mydata$HHINCOME[118:(118+29)]

#check length of samples
length(age)
length(hhinc)

#1A
#Exploratory data analysis of samples
#five number summary, variance, and mean
summary(age)
boxplot(age, main="Box-and-Whisker-Plot: Sample Age")
hist(age)
sd(age)
summary(hhinc)
boxplot(hhinc)
hist(hhinc)
sd(hhinc)

#bootstrapping age
agebstr<-matrix(0,nrow=4000, ncol=100)
for(i in 1:4000){
  samp = sample(age, size=100, replace = TRUE)
  agebstr[i,] = samp
}

agebstrm = apply(agebstr, 1, mean)
agesortm = sort(agebstrm)
hist(agesortm)
boxplot(agesortm)
summary(agesortm)

#bootstrapping hhinc
hbstr<-matrix(0,nrow=4000, ncol=30)
for(i in 1:4000){
  samp = sample(age, size=30, replace = TRUE)
  hbstr[i,] = samp
}

hbstrm = apply(hbstr, 1, mean)
hsortm = sort(hbstrm)
hist(hsortm)
boxplot(hsortm)
summary(hsortm)

#1B
#constructing a 95% conf interval for age
#finding the low and upper bounds of actual bstr
lowBound = agesortm[4000*.025]
uppBound = agesortm[4000*.975]
#Pr[lowBound<XB<uppBound]
#Pr[lowBound-ageMean<Xb-ageMean<uppBound-ageBound]
#Pr[lowBound-ageMean<agemean-popMean<uppBound-ageBound]
#Pr[-uppBound+ageMean<popMean-ageMean<-lowBound+ageMean]
#Pr[-uppBound+ageMean+ageMean<popMean<-lowBound+ageMean+ageMean]
truLow=(uppBound*-1)+(2*mean(age))
truUpp =(-1*lowBound)+(2*mean(age))
cat("(",truLow,",",truUpp,")")

#1C
#H0: Uage<= 43, H1: Uage>43
#sampling error: ageMean-U
samplingError = mean(age)-43
# ageMean - Uage ~ agebstrm - ageMean
#therefore agebstm - ageMean = -0.42
#therefore agebstrm = -0.42 + ageMean
testingMean = -0.42+mean(age)
#find the position ie. index of the first instance of testingMean
match(testingMean, agesortm)
#answer to above is index 1443
#find number of bstrm greater than testing mean & pval -- not completely certain about this
(4000-1443)/4003
#interpret pval

#1D
#just construct histogram of the bootstrap
hist(agebstrm)

#90% conf int of median hhinc --- review this first
