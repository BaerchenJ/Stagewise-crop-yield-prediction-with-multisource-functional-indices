
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

weathermean <- import('...\\Data\\wimean_adc.xlsx')

ADCnum = unique(weathermean$ADC)

ADCGROUP = list()

for (i in 1:length(ADCnum)){
  ADCGROUP[[i]] <- weathermean[which(weathermean$ADC == ADCnum[i]),]
}

set.seed(2)

vatecounty = list()

for (j in 1:length(ADCGROUP)){
  vatecounty[[j]] = sample(unique(ADCGROUP[[j]]$County), 2)
}

valicounty = testcounty = vector(length = 9)

for (j in 1:9){
  valicounty[j] = vatecounty[[j]][1] 
  testcounty[j] = vatecounty[[j]][2] 
}

# train

traindata <- weathermean %>% filter(!County %in% unlist(vatecounty))

y = range01(traindata$Ystar)

trainwimean <- traindata[,6:9]

for (ii in 1:4){
  trainwimean[,ii] = range01(as.numeric(trainwimean[,ii]))
}

s1gam <- gam(y~te(CR, GDD, SR, VP, k = 5, bs = "ps"), data = trainwimean, method = "REML")

# R2 train

expvar2_train <- sum((y - mean(y))^2)
expvar1_train <- sum((s1gam$residuals)^2)
r2_train = 1 - expvar1_train/expvar2_train

# RMSE train

rmse_train <- sqrt(mean((traindata$Ystar - back(s1gam$fitted.values, traindata$Ystar))^2))

## R2 valid

validdata <- weathermean %>% filter(County %in% valicounty)

yvalid <- range01(validdata$Ystar)

validwiweek <- validdata[,6:9]

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

testdata <- weathermean %>% filter(County %in% testcounty)

ytest <- range01(testdata$Ystar)

testwiweek <- testdata[,6:9]

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

write.xlsx(data.frame(r2list, rmselist), "S1Res\\GAM_at_space_S1_result.xlsx")

# Fit

traindf <- data.frame(traindata[,1:5], Ynorm = y, Fit = s1gam$fitted.values, Resi = s1gam$residuals)

write.xlsx(traindf, "S1Res\\GAM_at_space_S1_train.xlsx")

validdf <- data.frame(validdata[,1:5], Ynorm = yvalid, Fit = validpreds, Resi = yvalid - validpreds)

write.xlsx(validdf, "S1Res\\GAM_at_space_S1_valid.xlsx")

testdf <- data.frame(testdata[,1:5], Ynorm = ytest, Fit = testpreds, Resi = ytest - testpreds)

write.xlsx(testdf, "S1Res\\GAM_at_space_S1_test.xlsx")
