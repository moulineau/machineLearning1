#'#TD4 AYMERIC NOEL
#'
#'##question 1
#'
dataset <- read.csv('http://www.mghassany.com/MLcourse/datasets/Social_Network_Ads.csv')


str(dataset) 
summary(dataset) 

boxplot(Age ~ Purchased, data=dataset, col = "blue", main="Boxplot Age ~ Purchased",ylab="Age",xlab="Purchased")
#'we can see that we are more purchased when we are older
#'
boxplot(EstimatedSalary ~ Purchased, data=dataset,col = "red", main="Boxplot EstimatedSalary ~ Purchased",ylab="estimated salary",xlab="purchased")
#'more we have a big salary, more we have a chance to purchase
#'
summary(aov(EstimatedSalary ~Purchased, data=dataset))
#' p value is <<0.01 so the variables are dependents
#' 
summary(aov(Age ~Purchased, data=dataset))
#' p value is <<0.01 so the variables are dependents
#' 
table(dataset$Gender,dataset$Purchased)
#'so the female makes more purchase because female~1=77 and male~1=66
#'
mosaicplot(~ Purchased + Gender, data=dataset,
           main = "MosaicPlot of two categorical variables: Puchased & Gender",
           color = 2:3, las = 1)
#'So there isn't correlations between gender and purchased
#'

chisq.test(dataset$Purchased, dataset$Gender)
#'p value is >0.4 so there purchaed and gender are independent
#'

dataset = dataset[3:5]
str(dataset) 

library(caTools)
set.seed(700173) 
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


training_set[-3] <- scale(training_set[-3]) 
test_set[-3] <- scale(test_set[-3])

classifier.logreg <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)

pred.glm = predict(classifier.logreg, newdata = test_set[,-3], type="response")

pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)

head(pred.glm)
head(pred.glm_0_1)

#' confusion matrix
#' 
cm = table(pred.glm_0_1, test_set[,3])
cm

mosaicplot(cm,col=sample(1:8,2)) 

#' ROC
#' 
require(ROCR)
score <- prediction(pred.glm,test_set[,3]) # we use the predicted probabilities not the 0 or 1
performance(score,"auc") # y.values
plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)

#'##question 2
#'
intercept=-(classifier.logreg$coefficients[1]/classifier.logreg$coefficients[3])
slope=-(classifier.logreg$coefficients[2]/classifier.logreg$coefficients[3])
plot(x = test_set$Age,y=test_set$EstimatedSalary,xlab="Age",ylab="Estimated Salary",main="Decision Boundary of Logistic Regression")
abline(a=intercept,b=slope)

#'##question 3
#'
plot(x = test_set$Age,y=test_set$EstimatedSalary,xlab="Age",ylab="Estimated Salary",bg=ifelse(pred.glm_0_1==1,'red','blue'),pch=21,main="Decision Boundary of Logistic Regression")
abline(a=intercept,b=slope)

#'##question 4
#'
plot(x = test_set$Age,y=test_set$EstimatedSalary,xlab="Age",ylab="Estimated Salary",bg=ifelse(test_set$Purchased==1,'red','blue'),pch=21,main="Decision Boundary of Logistic Regression")
abline(a=intercept,b=slope)
#'we have 9 false positive predictions (blue points among the red ones)
#'
cm[2,1]
#'we see that the false positive predictions are really of 9, so all is right
#'

#'##question 5
#'
library(MASS)
classifier.lda <- lda(Purchased~Age+EstimatedSalary, data=training_set)

#'##question 6
#'
classifier.lda
#'we have our coefficients and our priorfor each variables
#'

#'##question 7
#'
pred_lda=predict(classifier.lda,newdata =test_set)
prediction_LDA=pred_lda$class
str(predict(classifier.lda,newdata =test_set))
#function predict returns vector of length 3 with posterior, x and class which we keep here

#'##question 8
#'
cmlda = table(prediction_LDA, test_set[,3])
cmlda
cm
#'they are almost the same matrix of confusion, so one method is more precise here
#'

#'##question 9
#'
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary')
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'blue', 'red3'))
pred_grid = predict(classifier.lda, newdata = grid_set)$class
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)

#'##question 10.1
#'
class0=training_set[training_set$Purchased==0,]
class1=training_set[training_set$Purchased==1,]

#'##question 10.2
#'
pi0=length(class0$Purchased)/length(training_set$Purchased)
pi1=length(class1$Purchased)/length(training_set$Purchased)

#'##question 10.3
#'
mu0=c(mean(class0$Age),mean(class0$EstimatedSalary))
mu1=c(mean(class1$Age),mean(class1$EstimatedSalary))

#'##question 10.4
#'
sigma=((length(class0$Purchased)-1)*var(class0[-3]))/(length(training_set$Purchased)-2)+((length(class1$Purchased)-1)*var(class1[-3]))/(length(training_set$Purchased)-2)

#'##question 10.5
#'
x=c(1,1.5)
delta0=(t(x)%*%(sigma^-1)%*%mu0)-((1/2)*t(mu0)%*%(sigma^-1)%*%mu0)+log(pi0)
delta1=(t(x)%*%(sigma^-1)%*%mu1)-((1/2)*t(mu1)%*%(sigma^-1)%*%mu1)+log(pi1)
delta0
delta1

#' it belongs to class 1
#'

#'##question 10.6
#'


#'##question 11
#'
classifier.qda <- qda(Purchased~., data = training_set)
summary(classifier.qda)

#'##question 12
#'
prediction.qda = predict(classifier.qda, newdata = test_set[,-3])
pred.qda = prediction.qda$class
cm.qda = table(pred.qda,test_set[,3])
cm.qda
accuracy=(cm.qda[1,1]+cm.qda[2,2])/sum(cm.qda) *100
accuracy
cmlda
accuracy2=(cmlda[1,1]+cmlda[2,2])/sum(cmlda) *100
accuracy2

#'So resultas are better with qda method
#'

#'##question 13
#'
X1_1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2_2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set2 = expand.grid(X1_1, X2_2)
colnames(grid_set2) = c('Age', 'EstimatedSalary')
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary')
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'blue'))
pred_grid2 = predict(classifier.qda, newdata = grid_set)$class
contour(X1_1, X2_2, matrix(as.numeric(pred_grid2), length(X1_1), length(X2_2)), add = TRUE)

#'##question 14
#'
score.glm <- prediction(pred.glm,test_set[,3])
score.lda <- prediction(pred_lda$posterior[,2],test_set[,3])
score.qda <- prediction(prediction.qda$posterior[,2],test_set[,3])

#'we compute scores
#'

plot(performance(score.glm,"tpr","fpr"),col="green")
plot(performance(score.lda,"tpr","fpr"),col="red",add=T)
plot(performance(score.qda,"tpr","fpr"),col="blue",add=T)
abline(0,1,lty=8)

#'we display all AUC values
#'

AUC.scoreglm=performance(score.glm,"auc")@y.values[[1]]
AUC.scoreglm
AUC.scorelda=performance(score.lda,"auc")@y.values[[1]]
AUC.scorelda
AUC.scoreqda=performance(score.qda,"auc")@y.values[[1]]
AUC.scoreqda
#'So the best model here is the qda model, then the lda model then the logistic model
#'