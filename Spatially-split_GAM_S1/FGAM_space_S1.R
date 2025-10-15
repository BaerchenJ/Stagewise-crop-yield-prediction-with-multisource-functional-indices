
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

wholedata <- import("...\\Data\\fundat_wi_adc.xlsx")

ADCnum = unique(wholedata$ADC)

ADCGROUP = list()

for (i in 1:length(ADCnum)){
  ADCGROUP[[i]] <- wholedata[which(wholedata$ADC == ADCnum[i]),]
}

set.seed(2)

vatecounty = list()

for (j in 1:length(ADCGROUP)){
  vatecounty[[j]] = sample(unique(ADCGROUP[[j]]$County), 2)
}

valicounty = testcounty = vector(length = 9)

for (jj in 1:9){
  valicounty[jj] = vatecounty[[jj]][1] 
  testcounty[jj] = vatecounty[[jj]][2] 
}

## train data

traindata <- wholedata %>% filter(!County %in% unlist(vatecounty))

y <- range01(traindata$Ystar)

traintemp <- traindata[,6:109]

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
                   af(fVP, k = 5, bs = 'ps'), method="REML")

# R2 train

expvar2_train <- sum((y - mean(y))^2)
expvar1_train <- sum((s1fgam$residuals)^2)
r2_train = 1 - expvar1_train/expvar2_train

## R2 train 8 years

train8y = cbind(traindata[,1:5], y8y = y, resi8y = s1fgam$residuals, fit8y = s1fgam$fitted.values)
train8y <- train8y %>% filter(Year %in% c(2015:2022))

expvar2_train8y <- sum((train8y$y8y - mean(train8y$y8y))^2)
expvar1_train8y <- sum((train8y$resi8y)^2)
r2_train8y = 1 - expvar1_train8y/expvar2_train8y


## R2 train 5 years

train5y = cbind(traindata[,1:5], y5y = y, resi5y = s1fgam$residuals, fit5y = s1fgam$fitted.values)
train5y <- train5y %>% filter(Year %in% c(2018:2022))

expvar2_train5y <- sum((train5y$y5y - mean(train5y$y5y))^2)
expvar1_train5y <- sum((train5y$resi5y)^2)
r2_train5y = 1 - expvar1_train5y/expvar2_train5y


# RMSE train

rmse_train <- sqrt(mean((traindata$Ystar - back(s1fgam$fitted.values, traindata$Ystar))^2))

## RMSE train 8 years

rmse_train8y <- sqrt(mean((train8y$Ystar - back(train8y$fit8y, traindata$Ystar))^2))

## RMSE train 5 years

rmse_train5y <- sqrt(mean((train5y$Ystar - back(train5y$fit5y, traindata$Ystar))^2))

## valid data

validdata <- wholedata %>% filter(County %in% valicounty)

yvalid <- range01(validdata$Ystar)

validtemp <- validdata[,6:109]

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

## R2 valid 8 years

valid8y = cbind(validdata[,1:5], y8y = yvalid, resi8y = yvalid - validpreds, fit8y = validpreds)
valid8y <- valid8y %>% filter(Year %in% c(2015:2022))

expvar2_valid8y <- sum((valid8y$y8y - mean(valid8y$y8y))^2)
expvar1_valid8y <- sum((valid8y$resi8y)^2)
r2_valid8y = 1 - expvar1_valid8y/expvar2_valid8y

## R2 valid 5 years

valid5y = cbind(validdata[,1:5], y5y = yvalid, resi5y = yvalid - validpreds, fit5y = validpreds)
valid5y <- valid5y %>% filter(Year %in% c(2018:2022))

expvar2_valid5y <- sum((valid5y$y5y - mean(valid5y$y5y))^2)
expvar1_valid5y <- sum((valid5y$resi5y)^2)
r2_valid5y = 1 - expvar1_valid5y/expvar2_valid5y

# RMSE valid

rmse_valid <- sqrt(mean((validdata$Ystar - back(validpreds, validdata$Ystar))^2))

## RMSE valid 8 years

rmse_valid8y <- sqrt(mean((valid8y$Ystar - back(valid8y$fit8y, validdata$Ystar))^2))

## RMSE valid 5 years

rmse_valid5y <- sqrt(mean((valid5y$Ystar - back(valid5y$fit5y, validdata$Ystar))^2))

## test data

testdata <- wholedata %>% filter(County %in% testcounty)

ytest <- range01(testdata$Ystar)

testtemp <- testdata[,6:109]

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

## R2 test 8 years

test8y = cbind(testdata[,1:5], y8y = ytest, resi8y = ytest - testpreds, fit8y = testpreds)
test8y <- test8y %>% filter(Year %in% c(2015:2022))

expvar2_test8y <- sum((test8y$y8y - mean(test8y$y8y))^2)
expvar1_test8y <- sum((test8y$resi8y)^2)
r2_test8y = 1 - expvar1_test8y/expvar2_test8y

## R2 test 5 years

test5y = cbind(testdata[,1:5], y5y = ytest, resi5y = ytest - testpreds, fit5y = testpreds)
test5y <- test5y %>% filter(Year %in% c(2018:2022))

expvar2_test5y <- sum((test5y$y5y - mean(test5y$y5y))^2)
expvar1_test5y <- sum((test5y$resi5y)^2)
r2_test5y = 1 - expvar1_test5y/expvar2_test5y


# RMSE test

rmse_test <- sqrt(mean((testdata$Ystar - back(testpreds, testdata$Ystar))^2))

## RMSE test 8 years

rmse_test8y <- sqrt(mean((test8y$Ystar - back(test8y$fit8y, testdata$Ystar))^2))

## RMSE test 5 years

rmse_test5y <- sqrt(mean((test5y$Ystar - back(test5y$fit5y, testdata$Ystar))^2))


## save results

# r2 rmse

r2list = list(r2_train = round(r2_train, 2), r2_valid = round(r2_valid, 2), r2_test = round(r2_test, 2))

rmselist = list(rmse_train = round(rmse_train, 2), rmse_valid = round(rmse_valid, 2), rmse_test = round(rmse_test, 2))

write.xlsx(data.frame(r2list, rmselist), "S1Res\\FGAM_space_S1_result.xlsx")

## 8 years

r2list8y = list(r2_train8y = round(r2_train8y, 2), r2_valid8y = round(r2_valid8y, 2), r2_test8y = round(r2_test8y, 2))

rmselist8y = list(rmse_train8y = round(rmse_train8y, 2), rmse_valid8y = round(rmse_valid8y, 2), rmse_test8y = round(rmse_test8y, 2))

write.xlsx(data.frame(r2list8y, rmselist8y), "S1Res\\FGAM_space_S1_result8y.xlsx")

## 5 years

r2list5y = list(r2_train5y = round(r2_train5y, 2), r2_valid5y = round(r2_valid5y, 2), r2_test5y = round(r2_test5y, 2))

rmselist5y = list(rmse_train5y = round(rmse_train5y, 2), rmse_valid5y = round(rmse_valid5y, 2), rmse_test5y = round(rmse_test5y, 2))

write.xlsx(data.frame(r2list5y, rmselist5y), "S1Res\\FGAM_space_S1_result5y.xlsx")


# Fit

traindf <- data.frame(traindata[,1:5], Ynorm = y, Fit = s1fgam$fitted.values, Resi = s1fgam$residuals)

write.xlsx(traindf, "S1Res\\FGAM_space_S1_train.xlsx")

validdf <- data.frame(validdata[,1:5], Ynorm = yvalid, Fit = validpreds, Resi = yvalid - validpreds)

write.xlsx(validdf, "S1Res\\FGAM_space_S1_valid.xlsx")

testdf <- data.frame(testdata[,1:5], Ynorm = ytest, Fit = testpreds, Resi = ytest - testpreds)

write.xlsx(testdf, "S1Res\\FGAM_space_S1_test.xlsx")
