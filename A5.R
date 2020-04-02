library(VGAM)
library("AER")
library(dplyr)
library(sjmisc)

library(foreign)
setwd("C:/Users/mizzU_000/Desktop/Duke Working/Semester 4, take 2/Econ 690/Econ690Rwork")
dat_choices = read.dta("dat_choices.dta")
dat = as.matrix(dat_choices)

choiceList = rbind(c(48,48,40,64),c(40,64,32,80),c(32,80,24,96),c(24,96,16,112),c(16,112,8,120),c(48,48,42,66),c(42,66,36,84),c(36,84,30,102),c(30,102,24,120),c(24,120,16,128),c(48,48,38,62),c(38,62,28,76),c(28,76,18,90),c(18,90,8,104),c(8,104,0,112),c(42,42,36,60),c(36,60,30,78),c(30,78,24,96),c(24,96,18,114),c(18,114,10,122),c(54,54,44,68),c(44,68,34,82),c(34,82,24,96),c(24,96,14,110),c(14,110,6,118))


crraUtility = function(c, theta, threshold=0.0000001){
  # Set threshold for being close enough to 1
  if (c==0){return(0)}
  if (abs(theta - 1) >= threshold){
    return ((c^(1-theta))/(1-theta))
  } else {
    return (log(c))
  }
}

crraUtilVecInput = function(cs, thetas, threshold=0.0000001){
  numcs = length(cs)
  numthetas = length(thetas)
  out = matrix(nrow = numcs, ncol = numthetas)
  for (c in 1:numcs){
    for (theta in 1:numthetas){
      out[c,theta] = crraUtility(cs[c],thetas[theta],threshold)
    }
  }
  return (out)}

#1.
choiceProb = function(risk, sigma = 0.5){
  x = rnorm(1,risk,sigma)
  return(as.numeric(0.5*(crraUtility(choiceList[lottery,1],x)+crraUtility(choiceList[lottery,2],x))-0.5*(crraUtility(choiceList[lottery,3],x)+crraUtility(choiceList[lottery,4],x))>=0))
}

riskParams = seq(0.1,2, by = 0.01)
choiceProbabilities = sapply(riskParams,choiceProb)
plot(riskParams,choiceProbabilities)
glm(choiceProbabilities~riskParams)
#I found the relationship to be ~0.15, implying that the left lottery choice becomes more likely as risk appetite increases (since it's positive) - this doesn't make sense. But the absolute value seems reasonable enough.
#The coefficient was ~0.75, which makes sense as that implies that when the risk aversion draw has a mean of total risk neutrality, we would predict the person to choose the safer left lottery more often, but not all the time (as we might for very risk averse people)
#These figures are very variable, as you would expect for such a high sigma



#2.
numSteps = 1000
#draws = matrix(rnorm(numSteps,theta,sigma), nrow = 1)

logLikelyhoodFunc = function(theta, sigma, y, draws = NULL){
  pr = NULL
  for(lottery in 1:nrow(choiceList)){
  simulatedYVals = NULL
  set.seed(123)
  # set here to allow the optimise function below to work
  if(is.null(draws)){draws = matrix(rnorm(numSteps,theta,sigma), nrow = 1)}
  #Unfortunately, I can't actually declare this outside, and need to use the set.seed strategy, because these draws depend on theta, which is the value being searched through in the optimise function, so could take any value
  simulatedYVals = as.numeric(0.5*(crraUtilVecInput(choiceList[lottery,1],draws)+crraUtilVecInput(choiceList[lottery,2],draws))-0.5*(crraUtilVecInput(choiceList[lottery,3],draws)+crraUtilVecInput(choiceList[lottery,4],draws))>=0)
  pr[lottery] = sum(simulatedYVals)/numSteps
  }
  pr[pr<0.00001]=0.00001
  pr[pr>0.99999]=0.99999
  return(-1*sum(y*log(pr)+(1-y)*log(1-pr)))
}

optimise(logLikelyhoodFunc, c(-5,5), sigma = 0.5, y=dat[120,])
#risk aversion = 0.49
optimise(logLikelyhoodFunc, c(-5,5), sigma = 0.5, y=dat[280,])
#risk aversion = 0.07
optimise(logLikelyhoodFunc, c(-5,5), sigma = 0.5, y=dat[1200,])
#risk aversion = -2.64
