rm(list = ls())
set.seed(0)
library(data.table)
library(caret)
library(rpart)

# Read the data
rawDF <- fread("Downloads/rawDF.csv")

# Part A
# Check if there is NAs is the dataset
ifelse(sum(apply(rawDF, 2, function(x){sum(is.na(x))}))>0,"yes","no")
numericDF <- data.frame(apply(rawDF[,-c(1,2)],2,function(x){as.numeric(x)}))

# Part B:
# Min max scaling:
numericDF2 <- data.frame(apply(numericDF[,-which(colnames(numericDF) == 'h.label')],2,function(x){ return((x - min(x))/(max(x)-min(x)))}))
numericDF2$'h.label' <- (numericDF[,"h.label"])

# Part C:
table(numericDF2$h.label) # to check if the data is imbalenced
idx = createDataPartition(numericDF2$h.label,p=0.75)
train <- numericDF2[idx$Resample1,] # Get the train data
test  <- numericDF2[ setdiff(1:nrow(numericDF2),idx$Resample1) ,] # get the test data

# Part D:
calcRMSE = function(predV,obV){
  myRMSE = sqrt(mean((predV - obV)^2))
  return(myRMSE)
}

# Part E:
fit = rpart(h.label ~ . , data = train ) #, method = 'class' # Fitting the decision tree
predV = predict(fit,test[,-13]) # Prediction on test data using model build on train data
obV = test[,13] # Extraction of the observed Y variable in test data
calcRMSE(predV,obV) # Calcuate the RMSE based on function written in part D

