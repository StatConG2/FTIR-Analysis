library(readxl)
library(tidyverse)
library(dplyr)


specData <- read_excel("abs.values.xlsx")

specData <- specData %>% filter(mel.concentration != 100.00)

testData <- specData %>% filter(rep == "rep4") 

trainData <- specData %>% filter(!...1 %in% testData$...1)

trainData1 <- trainData[, c(-1,-3)]
trainData2 <- trainData1[, c(1, 2950:3050)]
#trainData2 <- trainData1[, c(1, 4:1050)]

testData1 <- testData[, c(-1,-3)]
testData2 <- testData1[, c(1, 2950:3050)]
#testData2 <- testData1[, c(1, 4:1050)]

data1 <- specData %>% select(c(-1,-3))

# load libraries
library(MASS)
library(caret)

set.seed(1)

ctrl <- trainControl(
  method = "LOOCV",
  number = 40,
)

tuneGrid <- expand.grid(
  ncomp   = seq(1, 40, by = 1)
)

model <- train(
  mel.concentration ~ .,
  data = trainData2,
  method = 'pls',
  #preProcess = c("center", "scale"), 
  trControl = ctrl,
  tuneGrid = tuneGrid
)
model
dev.off()

plot(model)

test.features <- subset(testData2, select = -c(1))

predictions <- predict(model, newdata = test.features)

predictions

test.target <-  subset(testData1, select = c(mel.concentration))[,1]
actual <- subset(testData1, select = c(mel.concentration))

RMSE(predictions, test.target$mel.concentration)

# R2
cor(test.target, predictions) ^ 2

plot(test.target$mel.concentration, predictions)

X.train <- matrix(1:57)

for(i in 2:102){
  X.train <- cbind(X.train,trainData2[,i,drop=TRUE])
}

X.test <- matrix(1:19)

for(i in 2:102){
  X.test <- cbind(X.test,trainData2[,i,drop=TRUE])
}



concentration <- trainData2$mel.concentration
#concentration <- rep(c(as.numeric(substring(concentration,1,nchar(concentration)-1)),100,0),each=4)
concentration.test <- testData2$mel.concentration

# New Approach
# install modeldata library
install.packages('modeldata')
library(modeldata)
library(pls)

# fit the full model on train data
my_plsr <- plsr(concentration ~ X.train, ncomp=10, scale = TRUE,
                validation='LOO')
my_plsr

summary(my_plsr)

# create a plot to define the number of components
plot(RMSEP(my_plsr))
# 7 pls components will be enough

## Prediction using training test
pls_pred <- predict(my_plsr, X.train, ncomp=7)

# calculate RMSE
sqrt(mean((pls_pred - concentration)^2))

# R2
cor(concentration, pls_pred) ^ 2

# Plot actual vs predictions
plot(concentration, pls_pred)

#---------------------------------------------------------------------------------------------------------

## Prediction using testing test
pls_pred.test <- predict(my_plsr, X.test, ncomp=7)

# calculate RMSE
sqrt(mean((pls_pred.test - concentration.test)^2))

# R2
cor(concentration.test, pls_pred.test) ^ 2

# Plot actual vs predictions
plot(concentration.test, pls_pred.test)





