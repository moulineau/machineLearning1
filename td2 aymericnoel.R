#TP2
#1)
library(MASS)
Boston=Boston

#2)
#we keep the first 253 obs of boston
train= 1:400
training_set= Boston[train,names(Boston)]
testing_set=Boston[-train,]
#Boston[train,names(Boston)] = we take the "train rows" and names(Boston) =all the variables

#3)
cor(Boston$medv,Boston$age)
#the correlation is equal to -0.3769546 so the variable are stringly negatively correlated

#4)
myModel=lm(formula = medv ~ age ,data=training_set)
plot(training_set$age,training_set$medv,xlab = "age of the house",ylab = "price of the house",col="red", pch=20)
abline(myModel,col="blue",lwd=3)

#5)
myModel= lm(formula = medv ~age +log(lstat), data=training_set)
myModel$coefficients
#So Beta0=51.6021158, B1=0.0777927, B2=-14.3389686

#6)
summary(myModel)
#we have a line for each parameters of our model such as R-squared, coefficients, F-statistic,etc

#7)
#The predictor is significant because B1 has a positif effect on medv and lstat has a negative effect on medv

#8)
#This model is whole significant because Multiple R-squared is >0.5, it is 0.6684, so this is a good model
#p-value is near from 0 so the model is more significant

#9)
myModel=lm(formula = medv~., data=training_set)
summary(myModel)

#10)
myModel=lm(formula = medv~log(lstat)+.,data=training_set)
summary(myModel)

#11)
#R² improve because when the number of variable is up, R² is up

#12)
correlation<-cor(training_set)
#there are some variables which are very correlated,that is coefficients near to 1 or -1

#13)
# install.packages("corrplot")
library("corrplot")
corrplot.mixed(correlation)
#we can see easily which variables is correlated because bigger and coloured the circle is and more the variables are correlated

#14)
#The correlation between tax and rad is 0.87, so the variables are very correlated, near to 1

#'# 15)
#'## 15)bis
myModel=lm(formula = medv~log(lstat)+.-tax,data=training_set)
summary(myModel)
#R² is lower because we deleted one of the variables. But F-statistic gets higher, which means the p-values gets lower and thus the model is more significant without tax

#16)
yhat=predict(myModel,data=testing_set)
y=testing_set$medv
error=y-yhat
error_squared=error^2
MSE=mean(error_squared)
MSE

#17)
#chas is a int variable which has 1 or 0 as value
sum(Boston$chas)

#18)
boxplot(medv~chas,data=Boston)
