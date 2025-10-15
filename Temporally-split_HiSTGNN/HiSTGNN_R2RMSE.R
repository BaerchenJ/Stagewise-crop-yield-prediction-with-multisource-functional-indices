
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rio)
library(dplyr)
library(openxlsx)

back <- function(a1, a2){
  a3 = (max(a2)-min(a2))*a1 + min(a2)
  return(a3)
}

trainpred <- read.delim("Train.txt")

validpred <- read.delim("Valid.txt")
  
testpred <- read.delim("Test.txt")

traindata <- import("ytrain.xlsx")
traindata <- cbind(traindata, trainpred = unlist(trainpred))

validdata <- import("yvalid.xlsx")
validdata <- cbind(validdata, validpred = unlist(validpred))

testdata <- import("ytest.xlsx")
testdata <- cbind(testdata, testpred = unlist(testpred))

expvar1_train <- sum((traindata$Ynorm - traindata$trainpred)^2)
expvar2_train <- sum((traindata$Ynorm - mean(traindata$Ynorm))^2)
R2_train = 1 - expvar1_train/expvar2_train
RMSE_train <- sqrt(mean((traindata$Yori- back(traindata$trainpred, traindata43$Yori))^2))

expvar1_valid <- sum((validdata$Ystar - validdata$validpred)^2)
expvar2_valid <- sum((validdata$Ystar - mean(validdata$Ystar))^2)
R2_valid = 1 - expvar1_valid/expvar2_valid
RMSE_valid <- sqrt(mean((validdata$Yori- back(validdata$validpred, validdata43$Yori))^2))

expvar1_test <- sum((testdata$Ystar - testdata$testpred)^2)
expvar2_test <- sum((testdata$Ystar - mean(testdata$Ystar))^2)
R2_test = 1 - expvar1_test/expvar2_test
RMSE_test <- sqrt(mean((testdata$Yori- back(testdata$testpred, testdata43$Yori))^2))

# save r2 rmse

r2 = list(r2_train = round(R2_train, 2), r2_valid = round(R2_valid, 2), r2_test = round(R2_test, 2))

rmse = list(rmse_train = round(RMSE_train, 2), rmse_valid = round(RMSE_valid, 2), rmse_test = round(RMSE_test, 2))

write.xlsx(data.frame(r2, rmse), "HiSTGNN_result.xlsx")
