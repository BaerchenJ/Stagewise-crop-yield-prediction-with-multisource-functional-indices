
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

back <- function(a1, a2){
  a3 = (max(a2)-min(a2))*a1 + min(a2)
  return(a3)
}

library(rio)
library(dplyr)
library(refund)
library(openxlsx)

########################## Stage 2

spweek <- import('...\\Data\\fundat_sp_adc.xlsx')

ADCnum = unique(spweek$ADC)

ADCGROUP = list()

for (i in 1:length(ADCnum)){
  ADCGROUP[[i]] <- spweek[which(spweek$ADC == ADCnum[i]),]
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

resi_train_43 <- import("S1Res\\FGAM_space_S1_train.xlsx")

resi_train_8 <- resi_train_43 %>% filter(Year %in% c(2015:2022))

y2_train = resi_train_8$Resi

traindata <- spweek %>% filter(!County %in% unlist(vatecounty))

trainspmean <- traindata[,5:56]

for (ii in 1:52){
  trainspmean[,ii] = range01(as.numeric(trainspmean[,ii]))
}

fSM <- as.matrix(trainspmean[,1:26])
fPR <- as.matrix(trainspmean[,27:52])

#res = list()

# we cannot set k = k in the fgam(), thus cannot loop the k value

# k = 15  ## k = 20 costs too much time

# K = 15

s2fgam <- fgam(y2_train ~ af(fSM, k = 15, bs = 'ps') + af(fPR, k = 15, bs = 'ps'), method="REML")

# R2 train

# s1
ytrain = resi_train_8$Ynorm
expvar2_train <- sum((ytrain - mean(ytrain))^2)

expvar1_train_s1 <- sum((y2_train)^2) # y2_train is the residual of stage 1
r2_train_s1 = 1 - expvar1_train_s1/expvar2_train

# s2
expvar1_train_s2 <- sum((s2fgam$residuals)^2)
r2_train_s2 = 1 - expvar1_train_s2/expvar2_train

## s2 5 years
resi_train5 = cbind(resi_train_8[,1:6], resi2 = s2fgam$residuals)
resi_train5 <- resi_train5 %>% filter(Year %in% c(2018:2022))

ytrain5 = resi_train5$Ynorm
expvar2_train5 <- sum((ytrain5 - mean(ytrain5))^2)

expvar1_train_s2_5 <- sum((resi_train5$resi2)^2)
r2_train_s2_5 = 1 - expvar1_train_s2_5/expvar2_train5


# RMSE train

# s1

Ystar_train = resi_train_43$Ystar

rmse_train_s1 <- sqrt(mean((resi_train_8$Ystar - back(ytrain - y2_train, Ystar_train))^2))

# s2

rmse_train_s2 <- sqrt(mean((resi_train_8$Ystar - back(ytrain - s2fgam$residuals, Ystar_train))^2))

## s2 5 years

rmse_train_s2_5 <- sqrt(mean((resi_train5$Ystar - back(ytrain5 - resi_train5$resi2, Ystar_train))^2))


# R2 valid

resi_valid_43 <- import("S1Res\\FGAM_space_S1_valid.xlsx")

resi_valid_8 <- resi_valid_43 %>% filter(Year %in% c(2015:2022))

y2_valid = resi_valid_8$Resi

validdata <- spweek %>% filter(County %in% unlist(valicounty))

validspweek <- validdata[,5:56]

for (ii in 1:52){
  validspweek[,ii] = range01(as.numeric(validspweek[,ii]))
}

source('predict_fgam.R')

validpreds <- predfgam(object = s2fgam, 
                       newdata = list(fSM = as.matrix(validspweek[,1:26]),
                                      fPR = as.matrix(validspweek[,27:52])), 
                       type = "response") 

# R2 valid

# s1
yvalid = resi_valid_8$Ynorm
expvar2_valid <- sum((yvalid - mean(yvalid))^2)

expvar1_valid_s1 <- sum((y2_valid)^2) # y2 is the residual of stage 1
r2_valid_s1 = 1 - expvar1_valid_s1/expvar2_valid

# s2
expvar1_valid_s2 <- sum((y2_valid - validpreds)^2) # the residual of stage 2
r2_valid_s2 = 1 - expvar1_valid_s2/expvar2_valid

## s2 5 years
resi_valid5 = cbind(resi_valid_8[,1:6], resi2 = y2_valid - validpreds)
resi_valid5 <- resi_valid5 %>% filter(Year %in% c(2018:2022))

yvalid5 = resi_valid5$Ynorm
expvar2_valid5 <- sum((yvalid5 - mean(yvalid5))^2)

expvar1_valid_s2_5 <- sum((resi_valid5$resi2)^2)
r2_valid_s2_5 = 1 - expvar1_valid_s2_5/expvar2_valid5

# RMSE valid

# s1

Ystar_valid = resi_valid_43$Ystar

rmse_valid_s1 <- sqrt(mean((resi_valid_8$Ystar - back(yvalid - y2_valid, Ystar_valid))^2))

# s2

rmse_valid_s2 <- sqrt(mean((resi_valid_8$Ystar - back(yvalid - y2_valid + validpreds, Ystar_valid))^2))

## s2 5 years

rmse_valid_s2_5 <- sqrt(mean((resi_valid5$Ystar - back(yvalid5 - resi_valid5$resi2, Ystar_valid))^2))

# R2 test

resi_test_43 <- import("S1Res\\FGAM_space_S1_test.xlsx")

resi_test_8 <- resi_test_43 %>% filter(Year %in% c(2015:2022))

y2_test = resi_test_8$Resi

testdata <- spweek %>% filter(County %in% unlist(testcounty))

testspweek <- testdata[,5:56]

for (ii in 1:52){
  testspweek[,ii] = range01(as.numeric(testspweek[,ii]))
}

testpreds <- predfgam(object = s2fgam, 
                      newdata = list(fSM = as.matrix(testspweek[,1:26]),
                                     fPR = as.matrix(testspweek[,27:52])), 
                      type = "response")

# R2 test

# s1
ytest = resi_test_8$Ynorm
expvar2_test <- sum((ytest - mean(ytest))^2)

expvar1_test_s1 <- sum((y2_test)^2) # the residual of stage 1
r2_test_s1 = 1 - expvar1_test_s1/expvar2_test

# s2
expvar1_test_s2 <- sum((y2_test - testpreds)^2) # the residual of stage 2
r2_test_s2 = 1 - expvar1_test_s2/expvar2_test

## s2 5 years
resi_test5 = cbind(resi_test_8[,1:6], resi2 = y2_test - testpreds)
resi_test5 <- resi_test5 %>% filter(Year %in% c(2018:2022))

ytest5 = resi_test5$Ynorm
expvar2_test5 <- sum((ytest5 - mean(ytest5))^2)

expvar1_test_s2_5 <- sum((resi_test5$resi2)^2)
r2_test_s2_5 = 1 - expvar1_test_s2_5/expvar2_test5

# RMSE test

# s1

Ystar_test = resi_test_43$Ystar

rmse_test_s1 <- sqrt(mean((resi_test_8$Ystar - back(ytest - y2_test, Ystar_test))^2))

# s2

rmse_test_s2 <- sqrt(mean((resi_test_8$Ystar - back(ytest - y2_test + testpreds, Ystar_test))^2))

## s2 5 years

rmse_test_s2_5 <- sqrt(mean((resi_test5$Ystar - back(ytest5 - resi_test5$resi2, Ystar_test))^2))


#res[[k/5]] = list(k = k, r2_valid_s2 = r2_valid_s2)

#resdf = data.frame(do.call(rbind, res))

#write.xlsx(resdf, "TuneK\\FGAM_space_S2.xlsx")

## save results

# r2 rmse

r2vec_s1 = c(train = round(r2_train_s1, 2), valid = round(r2_valid_s1, 2), test = round(r2_test_s1, 2))

r2vec_s2 = c(train = round(r2_train_s2, 2), valid = round(r2_valid_s2, 2), test = round(r2_test_s2, 2))

rmsevec_s1 = c(train = round(rmse_train_s1, 2), valid = round(rmse_valid_s1, 2), test = round(rmse_test_s1, 2))

rmsevec_s2 = c(train = round(rmse_train_s2, 2), valid = round(rmse_valid_s2, 2), test = round(rmse_test_s2, 2))

write.xlsx(data.frame(r2vec_s1, r2vec_s2, rmsevec_s1, rmsevec_s2), "S2Res\\FGAM_space_S2_result.xlsx")

# r2 rmse s2 5 years

r2vec_s2_5 = c(train = round(r2_train_s2_5, 2), valid = round(r2_valid_s2_5, 2), test = round(r2_test_s2_5, 2))

rmsevec_s2_5  = c(train = round(rmse_train_s2_5, 2), valid = round(rmse_valid_s2_5, 2), test = round(rmse_test_s2_5, 2))

write.xlsx(data.frame(r2vec_s2_5, rmsevec_s2_5), "S2Res\\FGAM_space_S2_result5y.xlsx")


# Fit

traindf <- data.frame(resi_train_8, 
                      Fit2 = s2fgam$fitted.values, 
                      Resi2 = s2fgam$residuals)

write.xlsx(traindf, "S2Res\\FGAM_space_S2_train.xlsx")

validdf <- data.frame(resi_valid_8, 
                      Fit2 = validpreds, 
                      Resi2 = y2_valid - validpreds)

write.xlsx(validdf, "S2Res\\FGAM_space_S2_valid.xlsx")

testdf <- data.frame(resi_test_8, 
                     Fit2 = testpreds, 
                     Resi2 = y2_test - testpreds)

write.xlsx(testdf, "S2Res\\FGAM_space_S2_test.xlsx")




