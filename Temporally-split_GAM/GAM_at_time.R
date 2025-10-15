
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

weathermean <- import('...\\Data\\wimean.xlsx')

# train

traindata <- weathermean %>% filter(Year %in% seq(1980, 2013))

y = range01(traindata$Ystar)

trainwimean <- traindata[, 4:7]

for (ii in 1:4){
  trainwimean[,ii] = range01(as.numeric(trainwimean[,ii]))
}

# k = 5 since k = 10 will considerably increase compute with slight improvement on performance.

s1gam <- gam(y~te(CR, GDD, SR, VP, k = 5, bs = "ps"), data = trainwimean, method = "REML")

# R2 train

expvar2_train <- sum((y - mean(y))^2)
expvar1_train <- sum((s1gam$residuals)^2)
r2_train = 1 - expvar1_train/expvar2_train

# RMSE train

rmse_train <- sqrt(mean((traindata$Ystar - back(s1gam$fitted.values, traindata$Ystar))^2))

## R2 valid

validdata <- weathermean %>% filter(Year %in% seq(2014, 2018))

yvalid <- range01(validdata$Ystar)

validwiweek <- validdata[, 4:7]

for (ii in 1:4){
  validwiweek[,ii] = range01(as.numeric(validwiweek[,ii]))
}

validpreds <- predict(s1gam, newdata = validwiweek)

expvar2_valid <- sum((yvalid - mean(yvalid))^2) 
expvar1_valid <- sum((yvalid - validpreds)^2)
r2_valid = 1 - expvar1_valid/expvar2_valid

# RMSE valid

rmse_valid <- sqrt(mean((validdata$Ystar - back(validpreds, validdata$Ystar))^2))

## R2 test

testdata <- weathermean %>% filter(Year %in% seq(2019, 2022))

ytest <- range01(testdata$Ystar)

testwiweek <- testdata[, 4:7]

for (ii in 1:4){
  testwiweek[,ii] = range01(as.numeric(testwiweek[,ii]))
}

testpreds <- predict(s1gam, newdata = testwiweek)

expvar2_test <- sum((ytest - mean(ytest))^2) 
expvar1_test <- sum((ytest - testpreds)^2)
r2_test = 1 - expvar1_test/expvar2_test

# RMSE test

rmse_test <- sqrt(mean((testdata$Ystar - back(testpreds, testdata$Ystar))^2))

## save results

# r2 rmse

r2list = list(r2_train = round(r2_train, 2), r2_valid = round(r2_valid, 2), r2_test = round(r2_test, 2))

rmselist = list(rmse_train = round(rmse_train, 2), rmse_valid = round(rmse_valid, 2), rmse_test = round(rmse_test, 2))

write.xlsx(data.frame(r2list, rmselist), "Tsplit_Res\\GAM_at_time.xlsx")



