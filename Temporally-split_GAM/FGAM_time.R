
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

back <- function(a1, a2){
  a3 = (max(a2)-min(a2))*a1 + min(a2)
  return(a3)
}

library(refund)
library(rio)
library(dplyr)
library(openxlsx)

weatherweek <- import("...\\Data\\fundat_wi.xlsx")

## train

traindata <- weatherweek %>% filter(Year %in% seq(1980, 2013))

y <- range01(traindata$Ystar)

traintemp <- traindata[, 4:107]

for (i in 1:104){
  traintemp[,i] = range01(as.numeric(traintemp[,i]))
}

fCR <- as.matrix(traintemp[,1:26])
fGDD <- as.matrix(traintemp[,27:52])
fSR <- as.matrix(traintemp[,53:78])
fVP <- as.matrix(traintemp[,79:104])

s1fgam <- fgam(y ~ af(fCR, k = 5, bs = 'ps') + 
                   af(fGDD, k = 5, bs = 'ps') + 
                   af(fSR, k = 5, bs = 'ps') + 
                   af(fVP, k = 5, bs = 'ps'), method="REML")  # k = 10 will considerably increase the computing time 

# R2 train

expvar2_train <- sum((y - mean(y))^2)
expvar1_train <- sum((s1fgam$residuals)^2)
r2_train = 1 - expvar1_train/expvar2_train

# RMSE train

rmse_train <- sqrt(mean((traindata$Ystar - back(s1fgam$fitted.values, traindata$Ystar))^2))

## valid data

validdata <- weatherweek %>% filter(Year %in% seq(2014, 2018))

yvalid <- range01(validdata$Ystar)

validtemp <- validdata[, 4:107]

for (i in 1:104){
  validtemp[,i] = range01(as.numeric(validtemp[,i]))
}

source('predict_fgam.R')

validpreds <- predfgam(object = s1fgam, 
                       newdata = list(fCR = as.matrix(validtemp[,1:26]),
                                      fGDD = as.matrix(validtemp[,27:52]), 
                                      fSR = as.matrix(validtemp[,53:78]), 
                                      fVP = as.matrix(validtemp[,79:104])),
                                      type = "response") 

## R2 valid

expvar2_valid <- sum((yvalid - mean(yvalid))^2) 
expvar1_valid <- sum((yvalid - validpreds)^2)
r2_valid = 1 - expvar1_valid/expvar2_valid

# RMSE valid

rmse_valid <- sqrt(mean((validdata$Ystar - back(validpreds, validdata$Ystar))^2))

## test data

testdata <- weatherweek %>% filter(Year %in% seq(2019, 2022))

ytest <- range01(testdata$Ystar)

testtemp <- testdata[, 4:107]

for (i in 1:104){
  testtemp[,i] = range01(as.numeric(testtemp[,i]))
}

testpreds <- predfgam(object = s1fgam, 
                      newdata = list(fCR = as.matrix(testtemp[,1:26]),
                                     fGDD = as.matrix(testtemp[,27:52]), 
                                     fSR = as.matrix(testtemp[,53:78]), 
                                     fVP = as.matrix(testtemp[,79:104])), type = "response")

## R2 test

expvar2_test <- sum((ytest - mean(ytest))^2) 
expvar1_test <- sum((ytest - testpreds)^2)
r2_test = 1 - expvar1_test/expvar2_test

# RMSE test

rmse_test <- sqrt(mean((testdata$Ystar - back(testpreds, testdata$Ystar))^2))

## save results

# r2 rmse

r2list = list(r2_train = round(r2_train, 2), r2_valid = round(r2_valid, 2), r2_test = round(r2_test, 2))

rmselist = list(rmse_train = round(rmse_train, 2), rmse_valid = round(rmse_valid, 2), rmse_test = round(rmse_test, 2))

write.xlsx(data.frame(r2list, rmselist), "Tsplit_Res\\FGAM_time.xlsx")

