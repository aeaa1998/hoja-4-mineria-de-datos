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

predicted<-predict(fitLMP,newdata = test)
test$prediccion <- predicted
rmse(test$SalePrice,test$prediccion)
#Nueva prediccion con todas las variables cuantitativas

#Analisis de residuales con todas las variables
residuales <- test$SalePrice-test$prediccion
summary(residuales)
print("Mean error de la prediccion y el test con todas las variables")
abs(mean(test$SalePrice - prediction))
print("Nos queda un error bastante bajo en los residuales dando tambien un buen modelo")
plot(test$SalePrice, test$OverallQual)
points(prediction, test$OverallQual, col="red",pch=15)
par(mfrow = c(2,2))
plot(fitLMP)

#Analisis con solo las variables importatnes

#Variables importantes 
#MSSubClass
#OverallQual
#OverallCond
#YearBuilt
#MasVnrArea
#X1stFlrSF
#X2ndFlrSF
#BsmtFullBath
#BedroomAbvGr
#KitchenAbvGr
#TotRmsAbvGrd
#Fireplaces
#GarageCars
#ScreenPorch




#Set new model with only important variables
train$LotFrontage = NULL
train$LotArea = NULL
train$YearRemodAdd = NULL
train$BsmtFinSF1 = NULL
train$BsmtFinSF2 = NULL
train$BsmtUnfSF = NULL
train$TotalBsmtSF = NULL
train$LowQualFinSF = NULL
train$GrLivArea = NULL
train$BsmtHalfBath = NULL
train$FullBath = NULL
train$HalfBath = NULL
train$GarageYrBlt = NULL
train$GarageArea = NULL
train$WoodDeckSF = NULL
train$OpenPorchSF = NULL
train$EnclosedPorch = NULL
train$X3SsnPorch = NULL
train$PoolArea = NULL
train$MiscVal = NULL
train$MoSold = NULL
train$YrSold = NULL

#Ver si hay overfitting

fitCheckOV<-lm(SalePrice~., data = train)

summary(fitCheckOV)



#Get the correlation plot
par(mfrow = c(1,1))
corrplot(cor(train))

#Remove overfititng values
train$YearBuilt = NULL
train$X1stFlrSF = NULL
train$GarageCars = NULL
train$TotRmsAbvGrd = NULL

#Modelo sin overfitting
train$OverallCond = NULL
train$KitchenAbvGr = NULL
train$BedroomAbvGr = NULL
fitCheckNoOverfitting<-lm(SalePrice~., data = train)

summary(fitCheckNoOverfitting)
#Removemos nuevas variables sin imortancia

par(mfrow = c(1,1))
corrplot(cor(train))

predicted<-predict(fitCheckNoOverfitting,newdata = test)
test$new_prediccion <- predicted
rmse(test$SalePrice,test$new_prediccion)
#Nueva prediccion con todas las variables cuantitativas

#Analisis de residuales con todas las variables
residuales <- test$SalePrice-test$new_prediccion
summary(residuales)
print("Mean error de la prediccion y el test con todas las variables")
abs(mean(test$SalePrice - prediction))
print("Nos queda un error bastante bajo en los residuales dando tambien un buen modelo")
plot(test$SalePrice, test$OverallQual)
points(prediction, test$OverallQual, col="red",pch=15)
par(mfrow = c(2,2))
plot(fitLMP)

#Graficos de dispersion con variables selectivas
fitMLM_SalePrice<-lm(SalePrice~.,data = datos[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice")])
cat("SalePrice = ",round(fitMLM_SalePrice$coefficients[7],2), "YearRemodAdd + ", round(fitMLM_SalePrice$coefficients[6],2), "GarageArea + ", round(fitMLM_SalePrice$coefficients[5],2), "TotalBsmtSF", round(fitMLM_SalePrice$coefficients[4],2), "BsmtUnfSF + " , round(fitMLM_SalePrice$coefficients[3],2), "YearBuilt + ", round(fitMLM_SalePrice$coefficients[2],2), "GrLivArea", round(fitMLM_SalePrice$coefficients[1],2))
plot(datos[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice")])
