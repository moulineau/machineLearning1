#'#TP 3 Aymeric Noel
#'
#'#question 1
setwd("C:/Users/Aymeric-PC/Desktop/Cours/machine learning")
library(readr)
Social_Network_Ads <- read_csv("Social_Network_Ads.csv")

#'##*question 2*
summary(Social_Network_Ads)
#'We can see that social_network_ads has 5 variables : user ID, Gender,Age, Estimated salary and purchased

correlation<-cor(Social_Network_Ads$Age,Social_Network_Ads$EstimatedSalary)
correlation
#'There is a very small correlation between the age and the estimated salary

correlation<-cor(Social_Network_Ads$Purchased ,Social_Network_Ads$EstimatedSalary)
correlation
#'There is a medium correlation between the purchased and the estimatedSalary

correlation<-cor(Social_Network_Ads$Purchased ,Social_Network_Ads$Age)
correlation
#'There is a string relation between the age and the purchased, this is the strongest correlation

hist(Social_Network_Ads$Age,xlab = "Age of people",ylab = "number of people",col="red")
#'We can see that there is more people who are between 25 and 40 years

boxplot(Social_Network_Ads$Age~Social_Network_Ads$Purchased,xlab="Purchased",ylab="Age")
#'This boxplot confirm that there are more purchase when the person is older

boxplot(Social_Network_Ads$EstimatedSalary~Social_Network_Ads$Purchased,xlab="Purchased",ylab="Estimated salary")
#'There is more purchase when the median salary is higher than those who dont buy

#'##*question 3*
Social_Network_Ads$Age=scale(Social_Network_Ads$Age)[,1]
Social_Network_Ads$EstimatedSalary=scale(Social_Network_Ads$EstimatedSalary)[,1]
library(caTools)
set.seed(123)

split = sample.split(Social_Network_Ads$Purchased, SplitRatio = 0.75)
training_set = subset(Social_Network_Ads, split == TRUE)
test_set = subset(Social_Network_Ads, split == FALSE)

#'##*question 4*
#'
#'estimatedSalary_scale_training<- scale(training_set$EstimatedSalary)[,1]
#'
#'estimatedSalary_scale_test<- scale(test_set$EstimatedSalary)[,1]
#'
#'age_scale_training<- scale(training_set$Age)[,1]
#'
#'age_scale_test<- scale(test_set$Age)[,1]
#'

#'##*question 5*
model<- glm(Purchased~Age,family = "binomial",data=training_set)

#'##*question 6*
#'We must pass the argument family=binomial in order to tell  to run a logistic regression rather than some other type of generalized linear model.
#'

#'##*question 7*
summary(model)
coef(model)
#'so beta0 = -0.8940039 and beta1=0.9958049
#'
#'##$\hat{p}(X) = \frac{e^{0.8940039 + 0.9958049 X}}{1+e^{-0.8940039 + 0.9958049 X}}$
#'
#'##*question 8*
#'Age is significant because there is p value is near 0, p value < 2e-16

#'##*question 9*
AIC(model)
#'The value of AIC is 256.109
#'

#'##*question 10*
plot(training_set$Age,training_set$Purchased,xlab = "Age",ylab = "Purchased")

x <- seq(-2, 3, l = 200)
y <- exp(-(model$coefficients[1] + model$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

library(ggplot2)
ggplot(training_set, aes(x=Age, y=Purchased)) +
  geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#'##*question 11*
model2<-glm(Purchased~Age+EstimatedSalary,family = binomial,data = training_set)

#'##*question 12*
summary(model2)
#'The variables are significants because p value are nears 0. Both p value (for age and estimatedSalary)
#'

#'##*question 13*
AIC(model2)
#'AIC(model2)=205.7819 < AIC(model)=256.109 so this is a better model
#'

#'##*question 14*
predictions=predict(model2,newdata =test_set,type="response")
pred=predictions

#'##*question 15*
for (i in 1:100)
{
  if(predictions[i]>0.5)
    {predictions[i]=1} 
  else 
    {predictions[i]=0}
}

#'##*question 16*
matrix<-table(predictions,test_set$Purchased)
matrix
#'So we have 83 good predictions on 100 values
#'

#'##*question 17*
AnalyseMatrix<-function(x){
  accuracy <-(x[1,1]+x[2,2])/sum(x)
  cat("accuracy : ")
  print(accuracy)
  precision<-x[1,1]/(x[1,1]+x[2,1])
  cat("precision : ")
  print(precision)
  sensitivity<- x[1,1]/(x[1,1]+x[1,2])
  cat("sensitivity :")
  print(sensitivity)
  specificity<-x[2,2]/(x[2,2]+x[1,2])
  cat("specificity : ")
  print(specificity)
}
AnalyseMatrix(matrix)

#'##*question 18*
library("ROCR")
pred_ROCR1<-prediction(pred, test_set$Purchased)
perf_ROCR1<-performance(pred_ROCR1, "tpr","fpr")
plot(perf_ROCR1,col="blue")
abline(0,1)
auc1<-performance(pred_ROCR,measure = "auc")
auc1<-auc1@y.values[[1]]
auc1
#'AUC=0.91 >0.8 so this is a quite good classifier
#'

#'##*question 19*
predict2<- predict(model,newdata = test_set,type="response")
pred_ROCR2<-prediction(predict2,test_set$Purchased)
perf_ROCR2<-performance(pred_ROCR2,"tpr","fpr")
plot(perf_ROCR1,col="blue")
plot(perf_ROCR2,add=T,col="red")
#'So in blue we have the ROC of the model 2 with the 2 input variables, and in red the model with only the age
#'
auc2<-performance(pred_ROCR2,measure = "auc")
auc2<-auc2@y.values[[1]]
auc2
#'AUC1=0.91 and AUC2=0.88 so the best model is The second model with auc=0.91