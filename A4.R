library(foreign)
setwd("C:/Users/mizzU_000/Desktop/Duke Working/Semester 4, take 2/Econ 690/Econ690Rwork")
dat_choices = read.dta("dat_choices.dta")
dat = as.matrix(dat_choices)

# ------------- Exercise 1 -------------

crraUtility = function(c, theta, threshold=0.0000001){
  # Set threshold for being close enough to 1
  if (c==0){return(0)}
  if (abs(theta - 1) >= threshold){
    return ((c^(1-theta))/(1-theta))
  } else {
    return (log(c))
  }
}

#creating new function which takes vector inputs for c and theta and creates a matrix output of crraUtility outputs for each given c and theta
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

plotUtilFunc = function(cVal,lowTheta,highTheta,stepTheta,threshold=0.000001){
thetaVals = seq(lowTheta,highTheta,by = stepTheta)
mat = crraUtilVecInput(c(cVal),thetaVals,threshold)
p = plot(x = thetaVals,y=mat[1,1:length(thetaVals)])
return(p)
}

plotUtilFunc(50,0,2,0.01,0.0001)

# ------------- Exercise 2 -------------

# this does not work.. 
#[1] 1.125057e-24 2.891895e-23 1.667541e-21 4.939375e-19 8.119993e-15 4.659232e-25 5.142634e-24 6.885389e-23 1.623123e-21 4.939375e-19
#[11] 2.523801e-24 1.934377e-22 9.508935e-20 8.120393e-15 8.120488e-15 4.476378e-24 6.884949e-23 1.623123e-21 9.358773e-20 3.570476e-16
#[21] 3.112984e-25 1.259496e-23 1.684846e-21 3.212329e-18 4.557445e-13

choiceList = rbind(c(48,48,40,64),c(40,64,32,80),c(32,80,24,96),c(24,96,16,112),c(16,112,8,120),c(48,48,42,66),c(42,66,36,84),c(36,84,30,102),c(30,102,24,120),c(24,120,16,128),c(48,48,38,62),c(38,62,28,76),c(28,76,18,90),c(18,90,8,104),c(8,104,0,112),c(42,42,36,60),c(36,60,30,78),c(30,78,24,96),c(24,96,18,114),c(18,114,10,122),c(54,54,44,68),c(44,68,34,82),c(34,82,24,96),c(24,96,14,110),c(14,110,6,118))

firstChoice = cbind(choiceList[,1],choiceList[,2])
secondChoice = cbind(choiceList[,3],choiceList[,4])

# The below combines the two utilities in a given lottery to an overall expected utility
combUtil = function(utilFunc, utilOne, utilTwo, risk, prob = 0.5){
  #utilFunc is the utility function used, here crraUtility.
  #prob is the probability of the lottery having result utilOne
  #risk is the risk appetite
  return (prob*utilFunc(utilOne, risk) + (1-prob)*utilFunc(utilTwo, risk))
}

#The below gives the difference in expected utilities of the two lotteries
utilDiff = function(utilFunc, inputs, risk, prob = 0.5){
  #inputs here must be a vector of 4 integers illustrating the choice between 2 lotteries of 2 outcomes
  return(combUtil(utilFunc, inputs[1], inputs[2], risk, prob)-combUtil(utilFunc, inputs[3], inputs[4], risk, prob))
}

bisecFunc = function(func, inputs, lowbound=-15, upbound=15, threshold = 0.0000000001){
  if(missing(func)){func = crraUtility}
  a = c = lowbound
  b = upbound
  if(min(abs(utilDiff(func, inputs, a)), abs(utilDiff(func, inputs, b)))< threshold ){
    return(min(abs(utilDiff(func, inputs, a)), abs(utilDiff(func, inputs, b))))
  }
  #check initial inputs are valid
  if(utilDiff(func, inputs, a)*utilDiff(func, inputs, b)>=0){
    print(paste("problem:", a, b, utilDiff(func, inputs, a), utilDiff(func, inputs, b)))
    return("Invalid initial inputs")
    }  
  c = (a+b)/2
while (abs(utilDiff(func, inputs, c)) > threshold){
  if (utilDiff(func, inputs, a)*utilDiff(func, inputs, c) < 0){
    b = c } else if (utilDiff(func, inputs, b)*utilDiff(func, inputs, c) < 0){
      a = c } else {return ("bisection function failed")}
    c = (a+b)/2
  }
  return(c)
}

thetasAnswer = NULL
for (i in 1:25){
  thetasAnswer[i] = bisecFunc(crraUtility, choiceList[i,],-15,15)
}
thetasAnswer

#thetasAnswer = apply(choiceList,1,bisecFunc)

#3.
#Note: identified sets refers to the set of r (risk parameter possibilities) that each individual displays for each of 5 lotteries, depending on their turning point, which is where they switch between choosing the left lottery over the right lottery.

identifiedRSets = cbind(append(0,thetasAnswer[-length(thetasAnswer)]),thetasAnswer)
zeroIndices = seq(6,26,by=5)
identifiedRSets[zeroindices[-length(zeroindices)],1] = 0
# the case matters in R.
#> identifiedRSets[zeroindices[-length(zeroindices)],1] = 0
#Error in identifiedRSets[zeroindices[-length(zeroindices)], 1] = 0 : 
#  object 'zeroindices' not found

representativeR = 0.5*(identifiedRSets[,1]+identifiedRSets[,2])
#identifiedRSets[zeroindices-1,2] = 100
#identifiedRSets now has a row for each choice giving the set of r which would make a person choose the first lottery, with the low bound of r in column 1, and the upper bound in column 2

testIdentRs = NULL
for (i in 1:25){
  testIdentRs[i] = combUtil(crraUtility, choiceList[i,1], choiceList[i,2], representativeR[i]) < combUtil(crraUtility, choiceList[i,3], choiceList[i,4], representativeR[i])
}
testIdentRs

#4.
#Initialise output matrix
risks = matrix(nrow = nrow(dat), ncol = ncol(dat)/5)
#For each individual in the data
for(individual in 1: nrow(dat)){
  #for each of the 5 lotteries
  for(j in 1:5){
    #assuming a 0 means choosing the left gamble, and 1 the right
    #Choosing the left gamble over the right means the risk aversion of the individual is LOWER than the risk which would make someone indifferent between the gambles, i.e. thetasAnswer for that choice
    #That means we will assign the individual a representative risk from its risk bracket for each of the 5 lotteries when the individual changes between left and right
    #If an individual consistently chooses left, we assign the representative risk
    #If an individual consistently chooses right, we assign a risk larger than the risk that would make someone indifferent between the gambles in the 5th choice
    #Better would be to combine the 5 risk brackets into one representative risk, but the indivuals are too inconsistent to do this
    count = 2
    while(dat[individual,(count+(j-1)*5)]==dat[individual,(count-1+(j-1)*5)]){
      #runs until a switch from left to right or vice-versa
      count = count+1
      if(count>5){break}
    }
    if(count<6){
      #i.e. the individual switches between left and right at some point
      risks[individual,j]=representativeR[(count+(j-1)*5)]}
    else if (dat[individual,(1+(j-1)*5)]==0){
      #i.e. the individual always chooses left
      risks[individual,j]=representativeR[(1+(j-1)*5)]
    } else {
      #i.e. the individual always chooses right
      risks[individual,j]=1.5*identifiedRSets[(5+(j-1)*5),2]
    }
  }
}
AvRiskAversion = apply(risks,1,mean)

library(ggplot2)
hist(AvRiskAversion)
# hist(risks[,1])
# hist(risks[,2])
# hist(risks[,3])
# hist(risks[,4])
# hist(risks[,5])

# ------------- Exercise 3 -------------

library(VGAM)

VCombUtil = function(utilInputs, theta){
  w = 20
  #eta = rgumbel(1)
  #return(utilFunc(w+utilInputs[1],theta) + utilFunc(w+utilInputs[2],theta) + eta)
  #print(utilInputs[2])
  return(crraUtility(w+utilInputs[1],theta) + crraUtility(w+utilInputs[2],theta))
}

vLogLikelihood = function(yVec,theta){
  #theta = thetaandsigma[1]
  #sigma = thetaandsigma[2]
  logLikelihood = NULL
  for(i in 1:nrow(choiceList)){
    # this is obviously not correct.. You are using the density instead of the cdf.. 
    pr = dgumbel(VCombUtil(firstChoice[i,],theta)-VCombUtil(secondChoice[i,],theta))
  logLikelihood[i] = yVec[i]*log(1-pr)+(1-yVec[i])*log(pr)
  }
  #Why are all these negative?
  #print(logLikelihood)
  return(sum(logLikelihood))
}

#2.
gridsearch = function(yVec, thetas){
  likelys = NULL
  for (i in 1:length(thetas)){
      likelys[i] = vLogLikelihood(yVec,thetas[i])
  }
  j = which.max(likelys)
  return(thetas[j])
}

potentialThetas = seq(0,10,0.01)
gridsearch(dat[115,],potentialThetas)
#returns 5.93
gridsearch(dat[900,],potentialThetas)
#returns 5.91


# ------------- Exercise 4 -------------
# very good 
#1.
bealeFunc = function(inputs){
  x = inputs[1]
  y = inputs[2]
  return((1.5-x+x*y)^2+(2.25-x+x*y^2)^2+(2.625-x+x*y^3)^2)
}

xValues = yValues = seq(-5,5,by=0.01)
# can use
xyValues = expand.grid(xValues,yValues)
out = apply(xyValues,1,bealeFunc)
xyValues[which.min(out),]
#gives (x0,y0) = (3,0.5)

# bealeGrid = matrix(nrow = length(xValues), ncol = length(yValues))
# bealeGridMin = bealeFunc(-5,-5)
# 
# bruteForce = function(func, xs, ys, mat, minOfFunc){
#   xBest = xs[1]
#   yBest = ys[1]
#   for (i in 1:length(xs)){
#     for (j in 1:length(ys)){
#       mat[i,j] = func(c(xs[i],ys[j]))
#       if (mat[i,j]<minOfFunc){minOfFunc = mat[i,j]; xBest = xs[i]; yBest = ys[j]}
#       else {if(mat[i,j] == minOfFunc){xBest = append(xBest, xs[i]); yBest = append(yBest, ys[j])}}
#     }
#   }
#   out = c(minOfFunc, xBest, yBest)
#   return(out)
# }
# 
# bruteForce(bealeFunc, xValues, yValues, bealeGrid, bealeGridMin)
#(x0,y0) = (3, 0.5), has f(x0,y0) = 0

#2.
#This takes the partial derivative of the function by each input, holding all other inputs constant
genGradFunc = function(inputs, func){
  #Take epsilon as given
  epsilon = 0.00001
  out = NULL
  for (i in 1:length(inputs)){
    z1 = z2 = inputs
    #z2 = inputs
    z1[i] = inputs[i]+epsilon
    z2[i] = inputs[i]-epsilon
    out[i] = (func(z1) - func(z2))/(2*epsilon)
  }
  return(out)
}

genGradFunc(c(3,0.5),bealeFunc)
#Gives (0,3.825e-09)


# ------------- Exercise 5 -------------

RBan = function(inputs){
  x = inputs[1]
  y = inputs[2]
  return((1-x)^2+5*(y-x^2)^2)
}

gradRB = function(inputs){
  return(genGradFunc(inputs, RBan))
} 

SteepestDescentOptimisingFun = function(x0, func){
tolerance = 0.000001
maxIteration = 10000
alphaVec = c(1,0.1,0.001,0.0001)
x = x0
differ = tolerance + 1
currentIter = 1
#print("initial val:", x)
while (currentIter < maxIteration & abs(differ) > tolerance){
  #choose best alpha
  alpha = alphaVec[1]
  gr = genGradFunc(x, func)
  for (i in 2:length(alphaVec)){
    if (func(x - alphaVec[i]*gr) < func(x-alpha*gr)){
      alpha = alphaVec[i]
    }
  }
  newX = x - alpha*gr
  differ = func(newX) - func(x)
  x = newX
  currentIter = currentIter + 1
#print iteration and difference
#  print(paste("Iteration", currentIter, "Difference:", differ))
}
if(currentIter == maxIteration){
  print("Reached max iterations with no solution")
}
return(x)
}

RBOptiFun = function(x0){
  return(SteepestDescentOptimisingFun(x0, RBan))
}
RBOptiFun(c(0,0))
# Had to raise max iterations to 10,000 for function to work under different starting points - and it still doesn't give (1,1)
#Answer: 0.974, 0.948

# ------------- Exercise 6 -------------
# this is classic case of a numerical problem
# I would have liked to see more attempts to get parameters here

library("AER")
data("SmokeBan")
SBdata = SmokeBan
library(dplyr)
library(sjmisc)

#1.
ySmoke = as.numeric(SBdata$smoker=="yes")
XSBdata = SBdata[,-1]

SBBan = as.numeric(XSBdata$ban=="yes")
SBEducDummy = to_dummy(as.factor(XSBdata$education), var.name = "education", suffix = "label")
#Use "some college" as the reference value
SBAfam = as.numeric(XSBdata$afam=="yes")
SBHispanic = as.numeric(XSBdata$hispanic=="yes")
SBGender = as.numeric(XSBdata$gender=="female")
#Use "male" as reference value

XSmoke = cbind(rep(1,nrow(XSBdata)), SBBan, XSBdata$age, SBEducDummy[-3], SBAfam, SBHispanic, SBGender)
  
Smokeprobit = function(Bcol){
  y = as.vector(ySmoke)
  X = as.matrix(XSmoke)
  B = as.vector(Bcol)
  f = pnorm(X%*%B)
  logLikelihoodVec = (log(f)*y) + log(1-f)*(1-y)
  return(-1*(sum(logLikelihoodVec)))
  #The -1 here is so this can be minimised using optim to find the max likelihood B
}

# initB1 = as.matrix(glm(ySmoke~as.matrix(XSmoke[-1]))$coefficients)
# Smokeprobit(initB1)
# #Gives 7990
# initB2 = rep(0.01,ncol(XSmoke))
# Smokeprobit(rep(0.01,ncol(XSmoke)))
# #Gives 9301
# initB3 = rep(0,ncol(XSmoke))
# Smokeprobit(rep(0,ncol(XSmoke)))
# #Gives 6931
# 
# methodOptions = c("Nelder-Mead", "BFGS", "CG", "SANN")
# for (i in 1:length(methodOptions)){
#   optim(initB1,Smokeprobit, method = methodOptions[i])
#   optim(initB2,Smokeprobit, method = methodOptions[i])
#   optim(initB3,Smokeprobit, method = methodOptions[i])
# }

initB2 = rep(0.01,ncol(XSmoke))
solutionoptim = optim(initB2,Smokeprobit, method = "BFGS", hessian = TRUE)
#BFGS converges for all the initial B vectors I tried, and gave similar answers each time
solutionB = solutionoptim$par

#2.

Hess = solutionoptim$hessian
StandardErrors = solutionB%*%(1-diag(Hess))
#s.e.: 37377.15

# ------------- Exercise 7 -------------

VUtilFunc = function(choice, risk, prob = 0.5){
  #choice input should contain two potential outcomes
  o1 = choice[1]
  o2 = choice[2]
  w = 20
  #eps = pnrom()
  v = prob*crraUtility(w+o1, risk) + (1-prob)*crraUtility(w+o2,risk)
  return(v)
}


vlikelihood = function(yVec,thetaandsigma){
  thresh = 0.01
  theta = thetaandsigma[1]
  sigma = exp(thetaandsigma[2])
  pr = pnorm((VUtilFunc(firstChoice,theta)-VUtilFunc(secondChoice,theta)),sd=sigma)
  logLikelihood = yVec*(1-pr)+(1-yVec)*pr
  return(sum(logLikelihood))
}

library("nloptr")

#2.
thetaandsigma0 = c(2.1,0.8)
isres(thetaandsigma0,vlikelihood,lower = c(0,0), upper = c(100,100), yVec=dat[5,])
#Converges sucessfully to 99.71528 65.65006, with likelihood 12.5
# why dont you set realistic ranges to start with???
lbfgs(thetaandsigma0,vlikelihood, yVec=dat[5,])
#Converges sucessfully to 5.532317 1.645633, with likelihood 12.5. Faster convergence with more realistic answers
bobyqa(thetaandsigma0,vlikelihood, yVec=dat[5,])
#Converges sucessfully to 8.9128173 0.5352777, with likelihood 12.5. Slower convergence than neldermead, but better than isres
neldermead(thetaandsigma0,vlikelihood, yVec=dat[5,])
#Converges sucessfully to 9.450006 2.900002, with likelihood 12.5. Slower convergence than lbfgs, but better than bobyqa

#So all of these give different answers for theta and r, but find the same maximum likelihood
BestThetasandSigmas = matrix(nrow=nrow(dat_choices),ncol=2)
for (i in 1:nrow(dat_choices)){
  BestThetasandSigmas[i,] = lbfgs(thetaandsigma0,vlikelihood, yVec=dat[i,])$par
}
hist(BestThetasandSigmas[,1])

# ------------- Exercise 8 -------------

# VUtilFunc2 = function(choice, risk, sigma, prob = 0.5, threshold = 0.01){
#   #choice input should contain two potential outcomes
#   o1 = choice[1]
#   o2 = choice[2]
#   w = 20
#   eps = rnorm(1,0,sigma)
#   v = prob*crraUtility(w+o1, risk+eps) + (1-prob)*crraUtility(w+o2,risk)
#   return(v)
# }

#1.
choiceProbFunc = function(inputs,risk,sigma =0.5){
  choice1 = c(inputs[1],inputs[2])
  choice2 = c(inputs[3],inputs[4])
  v1 = VUtilFunc(choice1, risk)
  v2 = VUtilFunc(choice2, risk)
  return(pnorm(v1-v2,sigma))
}

riskparams = seq(0.1,2,by=0.01)

yChoiceProbs = matrix(nrow = nrow(choiceList),ncol = length(riskparams))
for(i in 1:nrow(choiceList)){
  for(j in 1:length(riskparams)){
      yChoiceProbs[i,j] = choiceProbFunc(inputs = choiceList[i,], risk = riskparams[j],sigma = 0.5)
  }
}

#Different for every choice in choiceList, so show the 25th choice as an example:
plot(riskparams,yChoiceProbs[25,])

#2.
# For 1st scenario, set r=0.5, sigma = 0.1
#Choice 1: u1 = 0.5*(u(48,r)+u(48,r))
#Choice 2: u1 = 0.5*(u(40,r)+u(64,r))
# Simulate eps1 and eps2 ~ N(0,sigma)
#Choice1 = u1 + eps1, same for choice2
#create ySimuated = 0 or 1, where y=0 if choice1>choice2, 1 otherwise
# Then, we use the simulated y values to generate an estimate for r and theta.
#These should be the same if everything is going correctly (we will find it isn't)

simulatedYTestFunc = function(inputs, thetaandsigma){
  #inputs here is the legth 4 vector of a comparison choice
  #print(thetaandsigma)
  r = thetaandsigma[1]
  sigma = thetaandsigma[2]
  #print(r)
  #print(sigma)
  u1 = VUtilFunc(c(inputs[1],inputs[2]),r)
  u2 = VUtilFunc(c(inputs[3],inputs[4]),r)
  eps = rnorm(2, 0, sigma)
  #eps2 = rnorm(1, 0, sigma)
  if((u1+eps[1])>(u2+eps[2])){
    # i.e. left gamble is preferred to right gamble for the given choice
    return(0)} else {
      return(1)}
}

# write a likelihood function which returns the most likely r and sigma, given y as an input

# likely = function(param){
#   y = param[1]
#   r = param[2]
#   sigma = param[3]
#   f = NULL
#   for(i in 1:nrow(choiceList)){
#     f[i] = choiceProbFunc(choiceList[i,],r,sigma)
#   }
#   logLikelihood = -1*((f*y) + (1-f)*(1-y))
#   return(sum(logLikelihood))
# }
# likelyrandsigma = function(y){
#   return(optim(c(0,0),fn=likely(y))$par)
# }
ySim = apply(choiceList,1,simulatedYTestFunc, thetaandsigma = c(0.1,0.5))
lbfgs(c(0.1,0.1),vlikelihood, yVec=ySim)$par
#estimated model: r = 0.01392636, theta = 0.07895495

simulationTest = function(givenRandSigma){
  r = givenRandSigma[1]
  sigma = givenRandSigma[2]
  estimatedrandsigma = matrix(nrow = 100,ncol = 2)
  for(j in 1:100){
    #simY = NULL
    simY = apply(choiceList,1,simulatedYTestFunc, thetaandsigma = givenRandSigma)
    estimatedrandsigma[j,] = lbfgs(c(0.1,0.1),vlikelihood, yVec=simY)$par
    #This records the 2 outputs as one row for each run of the simulation
  }
  #Now find confidence intervals for r and sigma
  out = c(mean(estimatedrandsigma[,1]),sd(estimatedrandsigma[,1]),mean(estimatedrandsigma[,2]),sd(estimatedrandsigma[,2]))
  return(out)
}

simulationTest(c(0.1,0.5))
#estimated r: 0.015176767 with sd: 0.001703276, estimated sigma: 0.079231972, with sd: 0.000420766
#These are nowhere near the r=0.1 sigma =0.5 I started with

#3.
testParams = expand.grid(c(0.5,0.8,1.5,2.8),c(0.1,0.9))
MonteResults = matrix(nrow = nrow(testParams),ncol = 4)
for(i in 1:nrow(testParams)){
  MonteResults[i,] = simulationTest(as.numeric(testParams[i,]))
}
# None of the results are close

# ------------- Exercise 9 -------------

dat_time = read.dta("dat_time.dta")
#dat = as.matrix(dat_choices)

# build list of choices given in Assignment
times = c(1,31,61,91)
rewards = c(75.31,75.63,76.25,78.13,81.25,87.50)
timesChoiceList = expand.grid(rewards,times)
timesChoiceList = rbind(timesChoiceList,timesChoiceList)
times2 = c(31,31,61,121,361,368,391,451)
finaltimes = rep(times2[1],6)
for (i in 2:length(times2)){
  finaltimes = append(finaltimes,rep(times2[i],6))
}
timesChoiceList = cbind(rep(75,48),timesChoiceList,finaltimes)

timeLikely = function(theta, params, y){
  sigma = params[1]
  B = params[2]
  del = params[3]
  #assume
  w = 20
  pr = NULL
  for(i in 1:nrow(timesChoiceList)){
    cq = timesChoiceList[i,1]
    tdelay = timesChoiceList[i,2] - 1
    dq = timesChoiceList[i,3]
    tau = timesChoiceList[i,4] - timesChoiceList[i,2]
    if(tdelay==0){
    pr[i] = crraUtility(w,theta)-crraUtility(w+cq,theta) + B*(del^tau)*(crraUtility(w+dq,theta)-crraUtility(w,theta))
  }else{
    pr[i] = B*(del^tdelay)*(crraUtility(w,theta)-crraUtility(w+cq,theta) + B*(del^(tau+tdelay))*(crraUtility(w+dq,theta)-crraUtility(w,theta)))
  }
  }
  pr = 1 - pnorm(pr,sigma)
  #the above is since we have y = 1 when epsilon is ABOVE the pr above
  likely = log(pr)*y + log(1-pr)*(1-y)
  return(-1*sum(likely))
}


estimate = function(givenRs){
  out = matrix(nrow = nrow(dat_time),ncol = 6)
  for(individual in 1:nrow(dat_time)){
    r = givenRs[individual]
    estimatedparams = matrix(nrow = 100,ncol = 3)
  for(j in 1:2){
    # Could use multiple optimising functions here, and they give different estimates. Choose optim.
    estimatedparams[j,] = optim(c(0.1,0.01,0.01),timeLikely, theta = r, y=dat_time[individual,])$par
    #This records the 3 outputs as one row for each run of the estimation
  }
  #Now find confidence intervals for sigma, beta and delta
    print(paste("finished individual ", individual))
  out[individual,] = c(mean(estimatedparams[,1]),sd(estimatedparams[,1]),mean(estimatedparams[,2]),sd(estimatedparams[,2]),mean(estimatedparams[,3]),sd(estimatedparams[,3]))
  }
  return(out)
}

#est = estimate(BestThetasandSigmas[,1])
#Too slow, takes ages, even after trying setting constraints and limiting my iterations to 2!

#3.
#don't runt the below until after the above
#glm(BestThetasandSigmas[,1]~est[,3]+est[,5])
