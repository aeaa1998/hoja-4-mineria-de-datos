library("ggpubr")
library("ggplot2")
library(dplyr)
library(rpart)
library(caret)
library(tree)
library(rsample) 
library(rpart.plot)
library(randomForest)
library(ipred)
library(corrplot)

trainSetGiven = read.csv("./data/train.csv", header = TRUE)


#Join the two sets of data
dataSet <- bind_rows(trainSetGiven)
dataSet$Id=NULL

set.seed(123)

dataSetCompleteNumeric =dataSet[, !sapply(dataSet, is.character)]
dataSetCompleteNumeric = dataSetCompleteNumeric[complete.cases(dataSetCompleteNumeric), ]
dataSet = dataSet[complete.cases(dataSet$SalePrice),]

#caro > 150,000
#barato < 100,000
#medio 100,000 - 150,000
caros = dataSetCompleteNumeric[dataSetCompleteNumeric$SalePrice>150000,] 
barato = dataSetCompleteNumeric[dataSetCompleteNumeric$SalePrice<100000,] 
medio = dataSetCompleteNumeric[dataSetCompleteNumeric$SalePrice>100000 & dataSetCompleteNumeric$SalePrice<150000,] 

#Proporciones caros
split <- initial_split(caros, prop = .65)
trainCaro <- training(split)
testCaro <- training(split)


#Proporciones medios
split <- initial_split(medio, prop = .65)
trainMedio <- training(split)
testMedio <- training(split)

#Proporciones baratos
split <- initial_split(barato, prop = .65)
trainBarato <- training(split)
testBarato <- training(split)

# Proporcion 35, 65
train <- bind_rows(trainCaro, trainMedio, trainBarato)
test  <- bind_rows(testCaro, testMedio, testBarato)

#-------------------------------------------------
# Regresión Lineal Simple 
#-------------------------------------------------
#Se escogio Overall Qual ya que con nuestro arbol de regresión se tuvo que esa era
# la vairable mas importante

fitSpOv<-lm(SalePrice~OverallQual, data = train)

#Estimar el precio a partir de su calidad en general
prediction<-predict(fitSpOv, newdata = test)

#El analisis de los residuos

residuales <- test$SalePrice-prediction
summary(residuales)
print("Mean error de la prediccion y el test")
abs(mean(test$SalePrice - prediction))
#Podemos ver que en la mediana entre los valores es realmente pequeña lo cual es un buen predictor

#No obstante el modelo ya lo calcula por lo que se pueden usar
# Si se mira el resumen del modelo podemos analizar el comportamiento de los residuos.
summary(fitSpOvCaro)
library(ModelMetrics)
#Podemos ver que queda un valor bastante alto dentro de nuestro RMSE eso es debido a que 
#Hay mucha vairacion entre los precios
rmse(test$SalePrice,prediction)

#En la grafica Residuals vs Fitted, se puede ver que los residuos se distribuyen de forma
# mas o menos aleatoria alrededor de 0
# En el grafico qq se puede ver que puede ser un grafico normal
plot(test$SalePrice, test$OverallQual)
points(prediction, test$OverallQual, col="red",pch=15)
par(mfrow = c(2,2))
plot(fitSpOv)


#-------------------------------------------------
# Regresion Lineal Multiple 
#-------------------------------------------------

fitLMP<-lm(SalePrice~., data = train)

summary(fitLMP)

#Multiple R-squared:      0.81,	Adjusted R-squared:      0.80

#Variables importantes 
#MSSubClass
#OverallQual
#OverallCond
#YearBuilt
#MasVnrArea
#X1stFlrSF
#X1stFlrSF
#BsmtFullBath
#BedroomAbvGr
#KitchenAbvGr
#TotRmsAbvGrd
#Fireplaces
#GarageCars
#ScreenPorch


predicted<-predict(fitLMP,newdata = test)

test$prediccion <- predicted
predicted
confussionMatrix<-confusionMatrix(test$SalePrice,test$prediccion)





