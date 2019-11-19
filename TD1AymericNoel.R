x=c(1,7,3,4)
y<-c(100:1)
x[3]+y[4]
cos(x[3])+sin(x[2])*exp(-y[2])
Df1=1
Df2=5
qf(p=0.9,df1=Df1,df2=Df2)
vect=rpois(n=100,lambda = 5)
x <- seq(-4, 4, l = 1)
plot(x,vect,type="l")
setwd("C:\Users\Aymeric-PC\Desktop\Cours\machine learning")
load("EU.Rdata")
myModel=lm(formula = CamCom2011 ~ Population2010 ,data=EU)
myModel$residuals
myModel$coefficients
summaryMyModel=summary(myModel)
summaryMyModel$sigma
library(MASS)
dim(Boston)
train = 1:400
test = -train

# Speficy that we are going to use only two variables (lstat and medv)
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

# Check the dimensions of the new dataset
dim(training_data)
#ans> [1] 400   2
plot(training_data$lstat, training_data$medv,xlab="x",ylab="y",main="titre",col="red",lwd=25)
