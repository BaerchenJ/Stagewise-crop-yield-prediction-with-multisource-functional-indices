
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

back <- function(a1, a2){
  a3 = (max(a2)-min(a2))*a1 + min(a2)
  return(a3)
}

library(rio)
library(dplyr)
library(mgcv)
library(openxlsx)

########################## Stage 1

wholedata <- import('BSVFMOOS_temporal.xlsx')

# train

traindata <- wholedata %>% filter(Year %in% seq(1980, 2013))

y_train = traindata$Ystar

# R2 train

expvar2_train <- sum((y_train - mean(y_train))^2)

expvar1_train <- sum((y_train - traindata$Fit)^2)

r2_train = 1 - expvar1_train/expvar2_train

# RMSE train

rmse_train <- sqrt(mean((traindata$Yori - back(traindata$Fit, traindata$Yori))^2))

# R2 valid

validdata <- wholedata %>% filter(Year %in% seq(2014, 2018))

yvalid <- validdata$Ystar

expvar2_valid <- sum((yvalid - mean(yvalid))^2) 

expvar1_valid <- sum((yvalid - validdata$Fit)^2)

r2_valid = 1 - expvar1_valid/expvar2_valid

# RMSE valid

rmse_valid <- sqrt(mean((validdata$Yori - back(validdata$Fit, validdata$Yori))^2))

# R2 test

testdata <- wholedata %>% filter(Year %in% seq(2019, 2022))

ytest <- testdata$Ystar

expvar2_test <- sum((ytest - mean(ytest))^2) 

expvar1_test <- sum((ytest - testdata$Fit)^2)

r2_test = 1 - expvar1_test/expvar2_test

# RMSE test

rmse_test <- sqrt(mean((testdata$Yori - back(testdata$Fit, testdata$Yori))^2))

## save results

# r2 rmse

r2list = list(r2_train = round(r2_train, 2), r2_valid = round(r2_valid, 2), r2_test = round(r2_test, 2))

rmselist = list(rmse_train = round(rmse_train, 2), rmse_valid = round(rmse_valid, 2), rmse_test = round(rmse_test, 2))

write.xlsx(data.frame(r2list, rmselist), "oos_temporal_result.xlsx")

# Fit

traindf <- data.frame(traindata[,1:5], Ynorm = y_train, Fit = traindata$Fit, Resi = y_train - traindata$Fit)

write.xlsx(traindf, "oos_temporal_train.xlsx")

validdf <- data.frame(validdata[,1:5], Ynorm = yvalid, Fit = validdata$Fit, Resi = yvalid - validdata$Fit)

write.xlsx(validdf, "oos_temporal_valid.xlsx")

testdf <- data.frame(testdata[,1:5], Ynorm = ytest, Fit = testdata$Fit, Resi = ytest - testdata$Fit)

write.xlsx(testdf, "oos_temporal_test.xlsx")
