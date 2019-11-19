#1
library(MASS)
dim(Boston)
help(Boston)

#2
train = 1:400
test = -train
training_data=Boston[train,]
testing_data=Boston[test,]

#3
cor(training_data$medv,training_data$age)

#4
model1 <- lm(medv ~ age , data= training_data)
plot(training_data$age,trainig_data$medv,xlab="Age of the house", ylab="Median House value", col="red",pch=20)

abline(model1,col="blue",lwd=3)

#5
plot(training_data$medv,trainig_data$lstat)
#Pour obtenir une forme ressemblant à une droite on doit passer au log
plot(training_data$medv,log(trainig_data$lstat))

model2 <- lm(medv ~ log(lstat) + age , data=trainig_data)

#6
#dans la console 
summary(model2) #plus lisible
#7
#1ère colonne : estimated=represente les B (B0,B1,...) pour chaque variable
#2ème colonne : std error = représente erreur sur B, doit être 3 fois plus petite que B0
#3ème colonne : t value = pourcentage de la loi normale (student) => doit être forcément plus grand que 3 ou plus petit que -3
#4ème colonne : probabilité de la la loi normale (de t) = doit être très petite. Ex : s c'est 0.5 alors c'est mauvais
anova(model2)

#8
plot(trainig_data$age,model2$residuals)
plot(log(trainig_data$lstat),model2$residuals)
#plot de l'erreur sur la colonne lstat => le résultat doit être un bruit/ si le résultat représente une forme alors il y a une erreur dans les données
#exemple : si on ne fait pas de log pour lstat erreur, si on le fait pas d'erreur

#9
model3 <- lm(medv ~ . ,  data= training_data)
summary(model3)

#10
model4 <- lm(medv ~ . -lstat + log(lstat) ,  data= training_data)
summary(model4)
#Pas de corrélation entre medv et black => on enlève black car p=0.8 qui est assez grand

model5 <- lm(medv ~ . -lstat + log(lstat) - black ,  data= training_data)
summary(model5)
#11
#R² ajusté augmente

#12
round(cor(training_data),2)

#13
corrplot(cor(training_data))
#Représente la corrélation entre les variables elles-mêmes
corrplot.mixed(cor(training_data))
#avec les valeurs
#Ex : Voir les corrélations entre les variables permet d'éliminer qui sont très corrélées entre elles et donc se polluent l'une l'autre#
#Ex : Prix du tabac = a*Prix du dollar + b*Prix de l'euro => Or l'euro et le dollar sont très fortement corrélés donc ils vont polluer le résultat de a et b

#14
#Correlation between tax and rad=0.87
#Les deux variables se polluent l'une l'autre : on peut en supprimer une
model6 <- lm(medv ~ . -lstat + log(lstat) - black - tax,  data= training_data)
summary(model6)

#15
#On peut enlever les deux variables car R² (adjusted R squared) ne baisse que de 1% => dans certains cas ça pourrait être grave mais dans notre cas ce n'est pas grave
model7 <- lm(medv ~ . -lstat + log(lstat) - black - tax -rad,  data= training_data)
summary(model7)
#F-statistique diminue de 5 => pas très grave car c'est la proportion que toutes les variables soient égales à 0 en même temps 
#C'est-à-dire que toutes nos variables soient mauvaises

#16
y=testing_data$medv
y_hat=predict(model6,data.frame(testing_data))
hist(y_hat-y)
#Dans le cas idéal, il faut avoir une allure de loi normale car tout bruit suit la loi normale

print(var(y)) #Variance des données totales (=28)
print(var(y-y_hat)) #Variance des erreurs issues de l'estimation (=17) 
#La partie non représentée dans nos données est de 17/28 donc l'erreur est assez grande (seulement 11/28 sont représentés)
#Pourquoi la prédiction est assez faible (11/28) alors qu'on a R² qui est de 77% ? 
#Quand on a séparé training data et testing data, on a pris 400 données à la suite l'une de l'autre dans training data 
#Peut-être que les données étaient triées ce qui fait que peut-être que les dernières données n'avaient aucune relation avec les 400 premières
#Il faudrait donc reprendre le training data et le testing data de manière aléaratoire

#17
#chas est une variable qualitative => on va la transformer en une variable quantitative en donnant comme valeurs 0 ou 1%
#on va faire un tableau disjonctif complet
#Au lieu d'une seule colonne, on aura deux colonnes : à côté de la rivière (1) ou non (0)




