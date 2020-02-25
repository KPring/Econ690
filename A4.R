#library(foreign)
setwd("C:/Users/mizzU_000/Desktop/Duke Working/Semester 4, take 2/Econ 690/Econ690Rwork")
dat_choices = read.dta("dat_choices.dta")
dat = as.matrix(dat_choices)

# ------------- Exercise 1 -------------

crraUtility = function(c, theta, threshold=0.01){
  # Set threshold for being close enough to 1
  if (c==0){return(0)}
  if (abs(theta - 1) >= threshold){
    return ((c^(1-theta))/(1-theta))
  } else {
    return (log(c))
  }
}
#Why on earth is crraUtility(0,5,0.01) returning -Inf? the first if statement above should be false for theta = 5, so the result should be 0!


#creating new function which takes vector inputs for c and theta and creates a matrix output of crraUtility outputs for each given c and theta

crraUtilVecInput = function(cs, thetas, threshold=0.01){
  numcs = length(cs)
  numthetas = length(thetas)
  #out = matrix(nrow = numcs, ncol = numthetas)
  out = matrix(nrow = numcs, ncol = numthetas)
  for (c in 1:numcs){
    for (theta in 1:numthetas){
      out[c,theta] = crraUtility(cs[c],thetas[theta],threshold)
    }
  }
  return (out)}

plotUtilFunc = function(cVal,lowTheta,highTheta,stepTheta,threshold=0.01){
thetaVals = seq(lowTheta,highTheta,by = stepTheta)
mat = crraUtilVecInput(c(cVal),thetaVals,threshold)
p = plot(x = thetaVals,y=mat[1,1:length(thetaVals)])
return(p)
}

plotUtilFunc(50,0,2,0.01,0.0001)



# ------------- Exercise 2 -------------


choiceList = rbind(c(48,48,40,64),c(40,64,32,80),c(32,80,24,96),c(24,96,16,112),c(16,112,8,120),c(48,48,42,66),c(42,66,36,84),c(36,84,30,102),c(30,102,24,120),c(24,120,16,128),c(48,48,38,62),c(38,62,28,76),c(28,76,18,90),c(18,90,8,104),c(8,104,0,112),c(42,42,36,60),c(36,60,30,78),c(30,78,24,96),c(24,96,18,114),c(18,114,10,122),c(54,54,44,68),c(44,68,34,82),c(34,82,24,96),c(24,96,14,110),c(14,110,6,118))

firstChoice = cbind(choiceList[,1],choiceList[,2])
secondChoice = cbind(choiceList[,3],choiceList[,4])

#This is where the problem is, where I haven't got the right f to use for bisection, and so the result of the  below is always positive.
combUtil = function(utilFunc, utilOne, utilTwo, a, threshold=0.01){
  return ((utilFunc(utilOne, a, threshold) + utilFunc(utilTwo, a, threshold))/2)
}

utilDiff = function(utilFunc, inputs, risk, threshold = 0.01){
  #inputs here must be a vector of 4 integers illustrating the choice between 2 lotteries of 2 outcomes
  return(combUtil(utilFunc, inputs[1], inputs[2], risk, threshold)-combUtil(utilFunc, inputs[3], inputs[4], risk, threshold))
}

bisecFunc = function(func, inputs, lowbound=-5, upbound=5, threshold=0.01){
  a = c = lowbound
  b = upbound
  #check initial inputs are valid
  if(utilDiff(func, inputs, a, threshold)*utilDiff(func, inputs, b, threshold)>=0){
    return("Invalid initial inputs")}
  #c = a
while (abs(utilDiff(func, inputs, c, threshold)) > threshold){
  c = (a+b)/2
  if (utilDiff(func, inputs, a, threshold)*utilDiff(func, inputs, c, threshold) < 0){
    b = c } else if (utilDiff(func, inputs, b, threshold)*utilDiff(func, inputs, c, threshold) < 0){
      a = c } else {return ("messed up")}}
  return(c)
}

thetasAnswer = NULL
for (i in 1:25){
  thetasAnswer[i] = bisecFunc(crraUtility, choiceList[i,],-5,5,0.01)
}

abs(utilDiff(crraUtility, choiceList[15,],2.5,0.01)) > 0.01
abs(utilDiff(crraUtility, choiceList[16,],2.5,0.01)) > 0.01
thetasAnswer

# This doesn't work because my crraUtility is returning Inf when it shouldn't - see the above

#Note: identified sets refers to the set of r (risk parameter possibilities) that each individual displays for each of 5 lotteries, depending on their turning point, which is where they switch between choosing the left lottery over the right lottery.

# ------------- Exercise 3 -------------




# ------------- Exercise 4 -------------

#1.
bealeFunc = function(x,y){
  return((1.5-x+x*y)^2+(2.25-x+x*y^2)^2+(2.625-x+x*y^3)^2)
}

xValues = seq(-5,5,by=0.01)
yValues = seq(-5,5,by=0.01)

xyValues = expand.grid(xValues,yValues)

# can use out = apply(xyValues,1,bealeFunc)
# xyValues[which.max(out),]

bealeGrid = matrix(nrow = length(xValues), ncol = length(yValues))
bealeGridMin = bealeFunc(-5,-5)

bruteForce = function(func, xs, ys, mat, minOfFunc){
  xBest = xs[1]
  yBest = ys[1]
  for (i in 1:length(xs)){
    for (j in 1:length(ys)){
      mat[i,j] = func(xs[i],ys[j])
      if (mat[i,j]<minOfFunc){minOfFunc = mat[i,j]; xBest = xs[i]; yBest = ys[j]}
      else {if(mat[i,j] == minOfFunc){xBest = append(xBest, xs[i]); yBest = append(yBest, ys[j])}}
    }
  }
  out = c(minOfFunc, xBest, yBest)
  return(out)
}

bruteForce(bealeFunc, xValues, yValues, bealeGrid, bealeGridMin)
#(x0,y0) = (3, 0.5), has f(x0,y0) = 0

#2.



# ------------- Exercise 5 -------------

RBan = function(inputs){
  x = inputs[1]
  y = inputs[2]
  return((1-x)^2+5*(y-x^2)^2)
}

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

gradRB = function(inputs){
  return(genGradFunc(inputs, RBan))
} 

optimisingFun = function(x0, func){
tolerance = 0.0001
maxIteration = 100
alphaVec = c(1,0.1,0.001,0.0001)
x = x0
differ = tolerance + 1
currentIter = 1
print("initial val:", x)
while (currentIter < maxIteration & abs(differ) > tolerance){
  #choose best alpha
  alpha = alphaVec[1]
  gr = genGradFunc(x, func)
  for (i in 2:length(alphaVec)){
    if (func(x + alphaVec[i]*gr) < func(x+alpha*gr)){
      alpha = alphaVec[i]
    }
  }
newX = x - alpha*func(x)
differ = func(newX) - func(x)
x = newX
currentIter = currentIter + 1
#print iteration and difference
  print(paste("Iteration", currentIter, "Difference:", differ))
}
if(currentIter == maxIteration){
  print("Reached max iterations with no solution")
}
return(x)
}

RBOptiFun = function(x0){
  return(optimisingFun(x0, RBan))
}

# ------------- Exercise 6 -------------

library("AER")
data("SmokeBan")
SBdata = SmokeBan



# ------------- Exercise 7 -------------

VUtilFunc = function(choice, risk, prob = 0.5, threshold = 0.01){
  #choice input should contain two potential outcomes
  o1 = choice[1]
  o2 = choice[2]
  w = 20
  #eps = pnrom()
  v = prob*crraUtility(w+o1, risk,threshold) + (1-prob)*crraUtility(w+o2,risk,threshold)
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

#create gradient


library("nloptr")


# ------------- Exercise 8 -------------

# For 1st scenario, set r=0.5, sigma = 0.1
#Choice 1: u1 = 0.5*(u(48,r)+u(48,r))
#Choice 2: u1 = 0.5*(u(40,r)+u(64,r))
# Sim ulate eps1 and eps2 ~ N(0,sigma)
#Choice1 = u1 + eps1, same for choice2
#create ySimuated = 0 or 1, where y=0 if choice1>choice2, 1 otherwise
# Then, we use the simulated y values to generate an estimate for r and theta.
#These should be the same if everything is going correctly (we will find it isn't)


simulatedYTestFunc = function(utilFunc, r, sigma, inputs, threshold=0.01){
  #inputs here is the legth 4 vector of a comparison choice
  u1 = combUtil(utilFunc, inputs[1], inputs[2], r, threshold)
  u2 = combUtil(utilFunc, inputs[3], inputs[4], r, threshold)
  eps1 = pnorm(0, sigma)
  eps2 = pnorm(0, sigma)
  if((u1+eps1)>(u2+eps2)){
    return(0)} else {
      return(1)}
}

#paramGrid = rbind(c(0.5,0.1),c(0.5,0.9),c(0.8,))

riskparams = seq(0.1,2,by=0.01)

VUtilFunc2 = function(choice, risk, sigma, prob = 0.5, threshold = 0.01){
  #choice input should contain two potential outcomes
  o1 = choice[1]
  o2 = choice[2]
  w = 20
  eps = rnorm(1,0,sigma)
  v = prob*crraUtility(w+o1, risk+eps ,threshold) + (1-prob)*crraUtility(w+o2,risk,threshold)
  return(v)
}

choiceProbFunc = function(inputs,risk){
  choice1 = c(inputs[1],inputs[2])
  choice2 = c(inputs[3],inputs[4])
  
  V1 = VUtilFunc(choice1, risk,0.5,0.01)
  V2 = VUtilFunc(choice2, risk,0.5,0.01)
  return(pnorm(v1-v2,sigma))
}

yChoiceProbs = matrix(nrow = (choiceList),ncol = length(riskparams))
for(i in 1:length(choiceList)){
  for(j in 1:length(riskparams)){
      yChoiceProbs[i,j] = choiceProbFunc(choiceList[i,],riskparams[j])
  }
}

plot(riskparams,yChoiceProbs)