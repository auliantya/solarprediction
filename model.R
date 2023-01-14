####################################
# Solar Forecasting                #
####################################

# Importing libraries
library(RCurl) # for downloading CSV file
library(e1071)
library(caret)
library(data.table)

# Importing the Iris data set
TrainSet <- read.csv(text = getURL("https://raw.githubusercontent.com/auliantya/solarprediction/master/training.csv"),header = TRUE)
TrainSet <- TrainSet[,-1]

# Building SVM model
model <- svm(solar_radiation ~ ., data = TrainSet, kernel = "linear", cost = 10, scale=TRUE)

# Save model to RDS file
saveRDS(model, "model.rds")
