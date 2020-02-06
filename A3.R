#Assignment 3 for Econ 690, by Katherine Pringle - date: 30/01/2020

library(AER)
data("Affairs")
dat = Affairs

# ------------- Exercise 1 -------------


glm(dat)

library(ggplot2)
pp = ggplot(dat) + geom_histogram(aes(x=affairs,color=gender,fill=gender), position="dodge")
pp1 = ggplot(dat) + stat_count(aes(x=affairs,color=gender,fill=gender), position="dodge")
pp2 = ggplot(dat) + stat_count(aes(x=affairs,color=age,fill=age), position="dodge")
ggplot(dat) + geom_histogram(aes(x=affairs,fill=as.factor(age)), position="dodge")
pp3 = ggplot(dat) + stat_count(aes(x=affairs,color=religiousness,fill=religiousness), position="dodge")
ggplot(dat) + geom_histogram(aes(x=affairs,fill=as.factor(religiousness)), position="dodge")
pp4 = ggplot(dat) + stat_count(aes(x=affairs,color=education,fill=education), position="dodge")
ggplot(dat) + geom_histogram(aes(x=affairs,fill=as.factor(education)), position="dodge")


# ------------- Exercise 2 -------------

#remember most variables are not continuous
#See p32 of slides about likelihood

lm(affairs ~ ., data = dat)

# ------------- Exercise 3 -------------
install.packages(sjmisc)
library(dplyr)
library(sjmisc)

#dat2 = dat %>% transform(affairs= as.numeric((affairs != 0)))
yaffairs = as.numeric(dat$affairs != 0)
dat2 = dat %>% select(gender, age, yearsmarried, children, religiousness, education, occupation, rating)
#dat3 = to_dummy(dat2, var.name = name, suffix = "label")
datGenderDummy = to_dummy(dat2$gender, suffix = "label")
datAgeDummy = to_dummy(as.factor(dat2$age), var.name = "Age", suffix = "label")
datChildDummy = to_dummy(as.factor(dat2$children), var.name = "Has_Children", suffix = "label")
datReligDummy = to_dummy(as.factor(dat2$religiousness), var.name = "Degree_of_Religiosity", suffix = "label")
datEducDummy = to_dummy(as.factor(dat2$education), var.name = "Education", suffix = "label")
datOccDummy = to_dummy(as.factor(dat2$occupation), var.name = "Occupation", suffix = "label")
datRatDummy = to_dummy(as.factor(dat2$rating), var.name = "Marriage_Satisfaction", suffix = "label")
#dat3 = cbind(datGenderDummy[-length(datGenderDummy)])
dat3 = cbind(rep(1,nrow(dat)), datGenderDummy[-1], datAgeDummy[-1], datChildDummy[-1], datReligDummy[-1], datEducDummy[-1], datOccDummy[-1], dat$yearsmarried, datRatDummy[-1])

names(dat3)[c(1,28)] = c("Intercept","Years_Married")


affairsprobit = function(Xmat,y,Bcol){
  X = as.matrix(Xmat)
  B = as.matrix(Bcol)
  f = pnorm(X%*%B)
  likelihoodVec = (f^y)*(1-f)^(1-y)
  return(prod(likelihoodVec))
}

#Test function works:
number_tests = 3
Btry = matrix(rnorm(number_tests*ncol(dat3))/ncol(dat3),ncol(dat3))
Ltry = rep(0,ncol(Btry))
for (i in 1:ncol(Btry)){
  Ltry[i] = affairsprobit(dat3,yaffairs,Btry[,i])
}
#Ltry shows the likelihood of the test Betas in Btry
which.max(Ltry)
#To identify the likeliest

fulldat = cbind(yaffairs,dat3[-1])

my_glm = glm(yaffairs ~ ., data = fulldat, family = binomial(link="probit"))
Bglm = my_glm$coefficients
affairsprobit(dat3,yaffairs,Bglm)
log(affairsprobit(dat3,yaffairs,Bglm))
#-287.8774
logLik(my_glm)
#-287.8774
logLik(my_glm)[1] - log(affairsprobit(dat3,yaffairs,Bglm))
#No difference!


# ------------- Exercise 4 -------------

affairslogit = function(Xmat,y,Bcol){
  X = as.matrix(Xmat)
  B = as.matrix(Bcol)
  f = exp(X%*%B)/(1+exp(X%*%B))
  likelihoodVec = (f^y)*(1-f)^(1-y)
  return(prod(likelihoodVec))
}

#Test function works:
number_tests = 3
Btry = matrix(rnorm(number_tests*ncol(dat3))/ncol(dat3),ncol(dat3))
Ltry = rep(0,ncol(Btry))
for (i in 1:ncol(Btry)){
  Ltry[i] = affairslogit(dat3,yaffairs,Btry[,i])
}
#Ltry shows the likelihood of the test Betas in Btry
which.max(Ltry)
#To identify the likeliest

# fulldat = cbind(yaffairs,dat3[-1])

my_glm2 = glm(yaffairs ~ ., data = fulldat, family = binomial(link="logit"))
Bglm2 = my_glm2$coefficients
affairsprobit(dat3,yaffairs,Bglm2)
log(affairslogit(dat3,yaffairs,Bglm2))
# -287.5259
logLik(my_glm2)
#-287.5259
logLik(my_glm2)[1] - log(affairslogit(dat3,yaffairs,Bglm2))
#Same output again

# ------------- Exercise 5 -------------
#See slide 44

# Need to turn y into dummy matrix
datY = dat %>% select(affairs)
yDummyMat = to_dummy(datY, var.name = "Y", suffix = "label")
jNum = ncol(yDummyMat)
# jNum is the number of different values y can take

# Conditional logit

#Need to build new yVec with 
yaffairsVec = as.vector(t(yDummyMat))
#Need to build new xMat which is a rbind of a matrix made from each row of x variables * y dummies
xNewMat = matrix()
xNewMatrow = matrix(rep(0,193),1,193)
xNCol = matrix()
for (i in 1:nrow(dat3)){
  for (j in 1:ncol(dat3)){
    xNCol = t(t(as.matrix(dat3[i,]))*yDummyMat[i,j])
    xNewMatrow = cbind(xNewMatrow, xNCol)
  }
#xNewMatrow = t(t(as.matrix(dat3[i,]))%*%as.matrix(yDummyMat[i,]))
xNewMat = rbind(xNewMat,xNewMatrow)
xNewMatrow = matrix()
xNCol = matrix()
}

condlogit = function(xMat,yMat,BMat){
  # Expect to have yMat dimensions = (number of observations * jNum) * 1
  # xMat dimensions = (num of obs * jNum) * (number of dummified x variables * jNum)
  # BMat dimensions = (number of dummified x variables * jNum) * 1
  X = as.matrix(xMat)
  B = as.matrix(BMat)
  XB = X%*%B
  # dim = (num of obs * jNum) * 1
  XBmat = matrix(XB,ncol = jNum)
  XBsum = rowSums(XBmat)
  XBtotal = rep(XBsum,each=jNum)
  p = exp(XB)/XBtotal
  # dimensions = (num of obs * jNum) * 1
  likelihoodVec = (p^yMat)*((1-p)^(1-yMat))
  return(prod(as.matrix(likelihoodVec)))
}

# Multinomial logit

multlogit = function(xMat,yMat,BMat){
  # Expect to have yMat dimensions = number of observations * jNum
  # xMat dimensions = num of obs * number of dummified x variables
  # BMat dimensions = number of dummified x variables * jNum
  X = as.matrix(xMat)
  B = as.matrix(BMat)
  XB = X%*%B
  p = exp(XB)/rowSums(XB)
  # dimensions = num of obs * jNum
  likelihoodVec = (p^yMat)*((1-p)^(1-yMat))
  return(prod(as.matrix(rowSums(likelihoodVec))))
}

#Test
number_tests = 3
Btry = matrix(rnorm(jNum*ncol(dat3))/ncol(dat3), ncol = jNum)
Ltry = multlogit(dat3,yDummyMat,Btry)
# This doesn't work, returning Inf, but I can't see why, as every step up until the product seems to work fine.

#Ltry = rep(0,ncol(Btry))
#for (i in 1:ncol(Btry)){
#  Ltry[i] = affairslogit(dat3,yaffairs,Btry[,i])
#}
#Ltry shows the likelihood of the test Betas in Btry
#which.max(Ltry)

# Mixed logit
