#'#TD 5 Aymeric NOEL IF 5
#'
#'First we import the dataset using the library readr wich allows us to read a csv file
setwd("C:/Users/Aymeric-PC/Desktop/Cours/machine learning")
library(MASS)
library(readr)
library(ROCR)
bank_marketing <- read.table(file="bank_marketing.csv",sep=";",header = TRUE)
str(bank_marketing)
#'As we can see we have 17 variables, some are quantitative, other qualitatives
#'
summary(bank_marketing)

#'
#'
mosaicplot(~ y + housing, data=bank_marketing,main = "MosaicPlot of two categorical variables: y & housing",color = 2:3, las = 1)
mosaicplot(~ y + contact, data=bank_marketing,main = "MosaicPlot of two categorical variables: y & housing",color = 2:3, las = 1)
chisq.test(bank_marketing$housing,bank_marketing$contact)
chisq.test(bank_marketing$campaign,bank_marketing$duration)
chisq.test(bank_marketing$housing,bank_marketing$contact)
chisq.test(bank_marketing$duration,bank_marketing$housing)


library(caTools)
set.seed(700173) 
split = sample.split(bank_marketing$y, SplitRatio = 0.75)
training_set = subset(bank_marketing, split == TRUE)
test_set = subset(bank_marketing, split == FALSE)
#'To split the dataset we use the function split with a ratio of 0.75%
#'

model_lda=lda(y~campaign*balance+duration, data=training_set)
#'Our first model is a lda model (linear discriminant analysis)
#'

summary(model_lda)
pred_lda = predict(model_lda, newdata = test_set[,-17])
prediction_lda=pred_lda$class
cmlda = table(prediction_lda, test_set[,17])
cmlda
mosaicplot(cmlda,col=sample(1:8,2)) 

#'Our model isn't perfect because as we can see in the confusion matrix, we have only true yes (predicted yes which is actually a real yes)
#'
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
AnalyseMatrix(cmlda)
#'We can see here that the accuracy is  0.89 %, that is we have 0.89% of good predictions
#'


model_qda <- qda(y~campaign*balance+duration, data=training_set)
#'Let's compute a qda model (Quadratic Discriminant Analysis)
#'
summary(model_qda)
pred_qda = predict(model_qda, newdata = test_set[,-17])
prediction_qda=pred_qda$class
cmqda = table(prediction_qda, test_set[,17])
cmqda
mosaicplot(cmqda,col=sample(1:8,2)) 
#'This predictor seems better. Let's compute the accuracy, the precision, the sensitivity and the specificity to have more informations.
#'
AnalyseMatrix(cmqda)
#'At final, the accuracy is lower than the previous model, 0.86 % of good predictions for the qda model and 0.89 for the lda model, so lda seems a better predictor.
#'

#'Let's compute the ROCR curves about our two models.
#'

score.lda <- prediction(pred_lda$posterior[,2],test_set[,17])
score.qda <- prediction(pred_qda$posterior[,2],test_set[,17])
performance(score.lda,"auc") # y.values
plot(performance(score.lda,"tpr","fpr"),col="green")
performance(score.qda,"auc") # y.values
plot(performance(score.qda,"tpr","fpr"),col="red",add=T)
abline(0,1,lty=8)
#'The red curve is the qda predictor and the green one is the lda predictor
#'The red curve is under the green one, that is comfirms that the lda model is better
#'

AUC.scorelda=performance(score.lda,"auc")@y.values[[1]]
AUC.scorelda
AUC.scoreqda=performance(score.qda,"auc")@y.values[[1]]
AUC.scoreqda
#'We know that an auc of 1 represents a  "perfect model", an AUC above 0.8 represents a quite good model and an AUC of 0.5 is similar to a random guessing.
#'So the qda predictor is a quite good model because its AUC is 0.82.
#'
#'To conclude, let's test a final model with multiples predictors : campaign*duration, poutcome and month. These variables are all dependents. Let's compute an lda model because it seems better in our case.
#'
model_lda2 <- lda(y~campaign*duration+poutcome+month, data=training_set)
summary(model_lda2)
pred_lda2 = predict(model_lda2, newdata = test_set[,-17])
prediction_lda2=pred_lda2$class
cmqla2 = table(prediction_lda, test_set[,17])
cmqla2
mosaicplot(cmqla2,col=sample(1:8,2)) 
AnalyseMatrix(cmqla2)
#'We have an accuracy of 0.89% whcich is not bad compare to our previous model.
#'

score.lda2 <- prediction(pred_lda2$posterior[,2],test_set[,17])
AUC.scorelda2=performance(score.lda2,"auc")@y.values[[1]]
AUC.scorelda2
#'We have an AUC of 0.89, so our new model is quite good, better than our previous models.
#'
plot(performance(score.lda,"tpr","fpr"),col="green")
plot(performance(score.qda,"tpr","fpr"),col="red",add=T)
plot(performance(score.lda2,"tpr","fpr"),col="blue",add=T)
abline(0,1,lty=8)
#'This confirms that the last model lda (the blue curve) is better.
#'

