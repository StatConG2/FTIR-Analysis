library(pls)
library(caret)

df = read.csv("abs.values.csv")
## removing 100%
df = df[df$mel.concentration != 100,]

df = df[,c(2,4:5054)]
data = df



# train test spliot
set.seed(123)
trainIndex <- createDataPartition(data$mel.concentration, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Preprocess the data by standardizing the predictor variables
xTrain <- scale(trainData[, -1])
yTrain <- trainData[, 1]
xTest <- scale(testData[, -1])
yTest <- testData[, 1]

data_train = cbind(yTrain, xTrain)
data_train = as.data.frame(data_train)

# Fit a PLS regression model
plsrModel <- plsr(yTrain~., data = data_train, ncomp = 5)
summary(plsrModel)

# Use the model to predict the test data
yPred <- predict(plsrModel, newdata = xTest)

# Evaluate the model performance on the test data
rmse <- RMSE(yPred, yTest)


# Display the results
cat("RMSE =", round(rmse, 2), "\n")