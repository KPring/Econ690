library("AER")
library(dplyr)
library(sjmisc)

data("GSOEP9402")
dat = GSOEP9402

# ------------- Exercise 1 -------------

#1.
band = function(vec){
  return(((4*(sd(vec))^5)/(3*length(vec)))^(1/5))
}

#2.
guassian = function(z){
  return((1/(sqrt(2*pi)))*exp(-1*z^2/2))
}

#3.
kdens = function(x0, vec){
  h = band(vec)
  g = guassian((x0 - vec)/h)
  return(sum(g)/(length(vec)*h))
}

# ------------- Exercise 2 -------------

ecdf = function(x0, vec){
  return((1/length(vec))*sum(as.numeric(vec<x0)))
}

# ------------- Exercise 3 -------------

trapzf = function(candd, func = kdens, n = 100, ...){
  c = candd[1]
  d = candd[2]
  h = (d-c)/n
  funcInputs = seq(c, d, by=h)
  weighting = rep(1, n+1)
  weighting[1] = weighting[n+1] = 1/2
  return(h*sum(apply(as.matrix(funcInputs), 1, func, ...)*weighting))
}

# ------------- Exercise 4 -------------

#I divide income by 1000 here to get income more into the range of the explanatory variables.
wagesVec = dat$income/1000
lambdaU = 2
lambdaE = 0.5
delta = 0.2
r = 0.01

#1.
fcdf = function(x0){
  G = ecdf(x0, wagesVec)
  return(1 - (1/lambdaE)*((delta/G)-delta))
}

fpdf = function(x0){
  G = kdens(x0, wagesVec)
  out = 1 - (1/lambdaE)*((delta/G)-delta)
  return(out)
}

#2.
maxW = max(wagesVec)

#Expression inside the integral in phi function, where F(x) is the fpdf of x.
integratingFuncFpdf = function(x){
  return((1-fpdf(x))/(r + delta + lambdaE*(1-fpdf(x))))
}

#The expression we are trying to get to zero in the bisection, since the root of this should be phi
phiFunc = function(phi, b0){
  print(trapzf(c(phi, maxW), func = integratingFuncFpdf))
  return(phi - (b0 + (lambdaU - lambdaE)*trapzf(c(phi, maxW), func = integratingFuncFpdf)))
}

#My bisection function for a previous assignment, allows me better control
bisecFunc = function(func, ..., lowbound=-10, upbound=500, threshold = 0.001){
  if(missing(func)){func = phiFunc}
  if(lowbound < -350){lowbound = -350}
  if(upbound > 610){upbound = 610} #phiFunc returns NA for phi lower than -350 and higher than 610, so prevent a and b taking these
  a = c = lowbound
  b = upbound
  #print(func(a, ...))
  #print(func(b, ...))
  fa = func(a, ...)
  fb = func(b, ...)
  if(min(abs(fa), abs(fb)) < threshold){
    return(min(abs(fa), abs(fb)))
  }
  #check initial inputs are valid
  count = 0
  while(fa*fb>=0 & count <40){
    #print(paste("problem:", a, b, func(a, ...), func(b, ...)))
    #return("Invalid initial inputs")
    #If the original bounds are too small, update them until they will work, up to a limit to prevent running forever
    #if (a < -350 & b > 1000){return("Invalid initial inputs")}
    a = a - 20
    if(a < -350){a = -350} #phiFunc returns NA for phi lower than -350 and higher than 614, so prevent a and b taking these
    b = b + 20
    if(b > 614){b = 614}
    fa = func(a, ...)
    fb = func(b, ...)
    while (is.na(fa)) {
      a = a + 1
      fa = func(a, ...)}
    while (is.na(fb)) {
      b = b - 1
      fb = func(b, ...)}
    count = count + 1
    if (count == 40){a = -350; b = 614}
  }  
  c = (a+b)/2
  fc = func(c, ...)
  while (abs(fc) > threshold){
    if (fa*fc < 0){
      b = c } else if (fb*fc < 0){
        a = c } else {return ("bisection function failed")}
    c = (a+b)/2
    fc = func(c, ...)
  }
  return(c)
}

lowboundStart = -10
upboundStart = 500

resWage = function(b, lowandup){
  # To speed up this bisection function, we will use updating bounds, instead of static ones.
  low = lowandup[1]
  up = lowandup[2]
  biSec = bisecFunc(phiFunc, b0 = b, lowbound = low, upbound = up)
  return(biSec)
}

# The results from this give a reservation wage for b=0 of 193.7516, which is extremely unrealistic seeming, especially given that realised wages are below this.
# However, phi does increase with b, as expected.

# ------------- Exercise 5 -------------

thetaData = NULL


maritalDummy = to_dummy(dat$marital, var.name = "Marital", suffix = "label")
#Select married as the default
XMatrix = cbind(rep(1,nrow(dat)), dat$kids, maritalDummy[-1], dat$meducation)

#1.
glm1 = glm(income/1000 ~ kids + marital + meducation, data=dat)
thetaData = c(glm1$coefficients, sigma(glm1))
names(thetaData)[8] = "sigma"

#2.

S = 5 #Simulation steps
#Ideally this would be higher, but I've set it low to get the optimising function to work
previouslyTriedB = matrix(nrow = 2, ncol = nrow(XMatrix) + 1)
previouslyTriedB[1, 1] = 0
previouslyTriedB[2, 1] = resWage(0, c(lowboundStart, upboundStart))
prevSigma = 20
epsMatrix = matrix(rnorm(S*nrow(dat), mean = 0, sd = prevSigma),nrow=nrow(dat))


simB = function(inputs, setIndividualsToTry = 1:nrow(dat)){
  start = Sys.time()
  numInd = length(setIndividualsToTry) # The number of individuals in the given sample
  beta = inputs[-(length(inputs))]
  #beta should be a vector of length = number of columns of XMatrix = 7
  bInd = as.vector(as.matrix(XMatrix[setIndividualsToTry,])%*%as.vector(beta))
  #Calculate non-random part of b for every individual
  sig = inputs[length(inputs)]
  # sig should be a positive scalar
  if(sig != prevSigma){
      epsMatrix = epsMatrix = matrix(rnorm(S*nrow(XMatrix), mean = 0, sd = sig), nrow=nrow(XMatrix))
      assign("prevSigma", sig, envir = .GlobalEnv)
  }
  # Set of epsilon values, where row is the individual, and column is the simulation step
  # The if clause here tries to prevent recalculation of the random variables when possible, so that the random component between iterations of the optimising function below skews results as little as possible
  b = matrix(bInd, nrow = length(bInd), ncol = S, byrow = FALSE) + epsMatrix[setIndividualsToTry,]
  # reservation wage phi isn't findable for b > 1680 or <-2500, so we ignore beta which results in these
  if(length(which(b>1680))>0){return(thetaData*1000)}
  if(length(which(b<(-2500)))>0){return(thetaData*1000)}
  res = NULL
  lowandup = c(lowboundStart, upboundStart)
  individualCount = 0
  for (individual in setIndividualsToTry){
    individualCount = individualCount + 1
    print(paste("Individual count: ", individualCount))
    resSim = NULL
    try = which.min(abs(bInd[individualCount] - previouslyTriedB[1,]))
    lowandup = c(previouslyTriedB[2, try]-10, previouslyTriedB[2, try]+10)
    for(simulation in 1:S){
      #b = bInd[individual] + epsMatrix[individual, simulation]
      resSim[simulation] = resWage(b[individualCount, simulation], lowandup)
      if(is.character(resSim[simulation])){return(thetaData*1000)}
      if(simulation == 1){
        previouslyTriedB[1, individualCount + 1] = bInd[individualCount]
        previouslyTriedB[2, individualCount + 1] = resSim[simulation]
      }
    }
    res[individualCount] = (1/S)*sum(resSim)
  }
  dat2 = XMatrix[setIndividualsToTry,]
  dat2 = cbind(res, dat2[-1])
  glm2 = glm(res ~ ., data=dat2)
  modelVals = c(glm2$coefficients, sigma(glm2))
  #Should be a list of 8 variables
  names(modelVals)[8] = "sigma"
  assign("previouslyTriedB", previouslyTriedB, envir = .GlobalEnv)
  end = Sys.time()
  timeToRun = end - start
  print(paste("Total time to run: ", timeToRun))
  return(modelVals)
}

#3.
library("nloptr")

betaAndSigma0 = c(rep(1,7), 20)
obj = function(betaAndSigma, numIndividualsToTry = nrow(XMatrix)){
  return(simB(betaAndSigma, numIndividualsToTry) - thetaData)
}
lower = c(rep(-1000,7), 0)
upper = c(rep(2000, 7), 2000)
#Each iteration takes ~3.5 minutes when restricting to the first 125 individuals in the data
#maxeval set to 20 to encourage this to complete
param = nloptr(betaAndSigma0, eval_f=obj, lb=lower, ub=upper, opts=list("algorithm"="NLOPT_LN_BOBYQA","print_level"=2,"xtol_rel"=1.0e-10,"maxeval"=20), setIndividualsToTry = 1:125)


#4.

numRuns = 100
samplePerRun = 20
modelResults = matrix(nrow = numRuns, ncol = 8)
for(run in 1:numRuns){
  BootstrapSet = runif(samplePerRun, 0, nrow(dat)) # Select random sample of individuals
  modelResults[run,] = sdOfParams(param, BootstrapSet)
}
for(run in 1:numRuns){
  for(j in 1:8){
    if(is.na(modelResults[run, j])){
      modelResults[run, j] = mean(modelResults[-which(is.na(modelResults[, j])), j])
      #replace any na values with the mean of the other values for the same parameter, so it doesn't ruin the sd calculation
    }
  }
}
sdevs = apply(modelResults, 2, sd)

