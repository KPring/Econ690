# --------- Exercise 1 ---------
# set working director
setwd("C:/Users/mizzU_000/Desktop/Duke Working/Semester 4, take 2/Econ 690/Econ690Rwork")

# install packages
list_packages = c("Hmisc","gdata","boot","xtable","MASS","moments","snow","mvtnorm")

install.packages(list_packages)

# list contents of directory and environment
dir()
ls()

# 5.
678%%9
#Result is 3, so no, 678 is not divisible by 9.

#6.
save.image("misc.RDATA")

#7.
?mean
??cut2

#8. The below returns NaN
0/0


# --------- Exercise 2 ---------
#1.
Titanic
sum(Titanic)
sum(Titanic[,,"Adult",])
sum(Titanic["Crew",,,])
sum(Titanic["3rd",,"Child",])
sum(Titanic["2nd","Female","Adult",])
sum(Titanic["1st","Male","Child",])
sum(Titanic["Crew","Female",,"Yes"])
Titanic["1st","Male","Adult","Yes"]

#2.
prop.table(Titanic["1st","Male","Adult",])
prop.table(Titanic["1st","Female","Adult",])
prop.table(Titanic["1st","Male","Child",])
prop.table(Titanic["3rd","Female","Adult",])

# --------- Exercise 3 ---------
#1.
a1 = 1:50
a2 = seq(1,50)
a3 = rev(seq(50,1,-1))
a4 = seq(1,50, length.out = 50)

a5 = c(1)
i = 1
while (length(a5)< 50) {
  i = i+1
  a5 = append(a5,i)
}

b1 = rev(1:50)
b2 = 50:1
b3 = seq(50,1)
b4 = rev(seq(1,50,1))


#2.
a = rep(c(10,19,7),15)
b = rep(c(1,2,5,6),8)

#3.
x = seq(3.1,6,by=0.1)
logSinX = log(x)*sin(x)

#4.
meanFnVec1 = sample(0:100,90,replace=F)
mean(meanFnVec1)

meanFnVec2 = sample(0:100,90,replace=T)
mean(meanFnVec2)

#5.
#a)
rowVec = 1:20
colVec = 1:15
#flVec = exp(sqrt(rowVec))*log(rowVec^5)
#cosRowVec = cos(rowVec)
#sinColVec = sin(colVec)
#mat2 = matrix(exp(sqrt(rowVec))*log(rowVec^5)/(5+cos(rowVec)*sin(colVec)),ncol=20,nrow=15)

#option 1:
sumVec1 = rep(0,20)
for (a in 1:20){
  sumVec1[a] = sum(exp(sqrt(a))*log(a^5)/(5+cos(a)*sin(colVec)))
next
}
sum(sumVec1)

#easier option:
mat1 = matrix(0,nrow=20,ncol=15)
for (a in 1:20){
  for (b in 1:15){
    mat1[a,b] = exp(sqrt(a))*log(a^5)/(5+cos(a)*sin(b))
  next
  }
  next
}
sum(mat1)

#b)
sumVec2 = rep(0,20)
for (a in 1:20){
  for (b in 1:a){
    sumVec2[a] = sum(exp(sqrt(a))*log(a^5)/(5+exp(a*b)*cos(a)*sin(b)))
  next
  }
  next
}
sum(sumVec2)

#6.
expCosX = exp(x)*cos(x)

# --------- Exercise 4 ---------

#1.
xVec = sample(0:999,1000,replace=T)
yVec = sample(0:999,1000,replace=T)

#2.
#a)
zVec = yVec[-1] - xVec[-1000]
#b)
wVec = sin(yVec[-1000])/cos(xVec[-1])
#c)
subX = subset(xVec,xVec>=200)
#d)
indY = which(yVec>=600)

# --------- Exercise 5 ---------

#1.
A = t(matrix(c(1,1,3,5,2,6,-2,-1,-3),3,3))

#a)
A%*%A%*%A
#yes, this equals 0

#b)
A = cbind(A,A[,1]+A[,3])

#c)
A[3,] = A[1,] + A[2,]

#d)
ave(A,row(A))
ave(A,col(A))

#2.
matA = matrix(c(2,1,1,1,1,3,3,1,2),3,3)
vecB = c(10,6,13)

#3.
solve(matA, vecB)

# --------- Exercise 6 ---------

#1.
fun1 = function(a,n)
{
vecfl = rep(a,n)
vecsl = 1:n
return((vecfl^vecsl)/vecsl)
}

#2.
fun2 = function(x)
{
  if (x<0){
  return(x^2+2*x+abs(x))
  } else {
    if (x<2){
  return(x^2+3+log(1+x))
  } else {
    return(x^2+4*x-14)
    }
  }
}

fun2(-3)
fun2(0)
fun2(3)

# --------- Exercise 7 ---------

#1.
v1 = sample(1:20,36,replace=T)

#2.
subV1 = (1:20)[-v1]

#3.
v2 = v1>5
v3 = as.numeric(v2)

#4.
m1 = t(matrix(v1,6,6))

#5.
xEx7 = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))

#6.
#first, get indices of missing values
w1 = which(is.na(xEx7))
#add indices of infinite values
w1 = append(w1,which(is.infinite(as.numeric(xEx7))))
#add indices of Not a Number values (I'm assuming this counts as non-finite)
w1 = append(w1,which(is.nan(as.numeric(xEx7))))
subxEx7 = xEx7[-w1]

# --------- Exercise 8 ---------

library(AER)
data("GSOEP9402")
dat = copy(GSOEP9402)

#2.
str(dat)
#It's a dataframe of 675 observations of 12 variables
dim(dat)
#there are 675 rows and 12 columns
dimnames(dat)[2]
#returns the names of the variables

#3.
ag = aggregate(income~year, dat, FUN=mean)
plot(ag)

#4.

# --------- Exercise 9 ---------

#1.
data("CASchools")
data1 = copy(CASchools)

#2.
reg1 = lm(read ~ district +school+ county + grades + students+ teachers+ calworks + lunch+ computer+ expenditure+ income+ english, data1)

#3.
form = read ~ district +school+ county + grades + students+ teachers+ calworks + lunch+ computer+ expenditure+ income+ english
reg2 = lm(form, data1,subset = 1:200)

# --------- Exercise 10 ---------

#1.
library(VGAM)
lu = rpareto(200,1,1)
length(which(lu>10))
#gives count of values >10
lu[which(lu>10)] = rlogis(length(which(lu>10)),6.5,0.5)

#2.
de = rnorm(200,1,2)
de = log(de)
missNeg = append(which(is.nan(de)),which(de<0))
cMissNeg = length(missNeg)
#gives count of missing and negative values of de
install.packages("truncnorm")
library(truncnorm)
de[missNeg] = rtruncnorm(cMissNeg,a=0,b=Inf,0,1)

#3.
orig = runif(200,0,1)
dest = runif(200,0,1)

#4.
hist = matrix(runif(4000,0,1),200,200)
dist = matrix(runif(4000,0,1),200,200)

#6.
su = matrix(rep(0,40000),200,200)
se = matrix(rep(0,40000),200,200)
for (j in 1:200){
  for (l in 1:200){
    su[j,l] = log(orig[j]+dest[l]+dist[j,l])/(1+log(orig[j]+dest[l]+dist[j,l]))
    se[j,l] = exp(orig[j]+dest[l]+hist[j,l])/(1+exp(orig[j]+dest[l]+hist[j,l]))
  }
}

#7.
r = 0.05
q = matrix(rep(0,40000),200,200)
qr = function(w){
  for (j in 1:200){
    for (l in 1:200){
      q[j,l] = w*(r+de[j])/(r+de[l])+lu[j]*log(w)-lu[l]*(1+log(w))+((r+de[j])/(r+de[l]))*sum(su[j,-j])-sum(su[l,-l])+((r+de[j])/(r+de[l]))*sum(se[j,-j])-sum(se[l,-l])
      
    }
  }
  return(q)
}

qr(9245)

#8.
gridw = seq(9100,55240,length.out = 50)

#9.
qarray = sapply(gridw,qr,simplify= T)

#time to do this
ptm = proc.time()
qarray = sapply(gridw,qr,simplify= T)
qarrtime = proc.time() - ptm

#time per entry in gridw
qarrtime["elapsed"]/length(gridw)
#about 0.7s

# --------- Exercise 11 ---------

#1.
is.array(c(1,2,3))
#no
is.vector(c(1,2,3))
#yes
is.matrix(c(1,2,3))
#no

#2.
t = table(x0,x0>0,x0>1,x0>2,x0>0.5,x0<1,x0>-1)

# --------- Exercise 12 ---------

# --------- Exercise 13 ---------

# --------- Exercise 14 ---------

# --------- Exercise 15 ---------

# --------- Exercise 16 ---------

