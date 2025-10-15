
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

back <- function(a1, a2){
  a3 = (max(a2)-min(a2))*a1 + min(a2)
  return(a3)
}

library(mgcv)
library(rio)
library(dplyr)
library(openxlsx)

weatherweek <- import("...\\Data\\fundat_wi_adc.xlsx")

ADCnum = unique(weatherweek$ADC)

ADCGROUP = list()

for (i in 1:length(ADCnum)){
  ADCGROUP[[i]] <- weatherweek[which(weatherweek$ADC == ADCnum[i]),]
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

## train

traindata <- weatherweek %>% filter(!County %in% unlist(vatecounty))

y1 <- range01(traindata$Ystar)

trainwiweek <- traindata[,6:109]

for (ii in 1:104){
  trainwiweek[,ii] = range01(as.numeric(trainwiweek[,ii]))
}

#res = list()

#for (k in seq(5, 50, by = 5)){

k = 35

gamw1 <- gam(y1~s(CR1, k = k, bs = "ps") + s(GDD1, k = k, bs = "ps") + s(SR1, k = k, bs = "ps") + s(VP1, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y2 <- gamw1$residuals
gamw2 <- gam(y2~s(CR2, k = k, bs = "ps") + s(GDD2, k = k, bs = "ps") + s(SR2, k = k, bs = "ps") + s(VP2, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y3 <- gamw2$residuals
gamw3 <- gam(y3~s(CR3, k = k, bs = "ps") + s(GDD3, k = k, bs = "ps") + s(SR3, k = k, bs = "ps") + s(VP3, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y4 <- gamw3$residuals
gamw4 <- gam(y4~s(CR4, k = k, bs = "ps") + s(GDD4, k = k, bs = "ps") + s(SR4, k = k, bs = "ps") + s(VP4, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y5 <- gamw4$residuals
gamw5 <- gam(y5~s(CR5, k = k, bs = "ps") + s(GDD5, k = k, bs = "ps") + s(SR5, k = k, bs = "ps") + s(VP5, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y6 <- gamw5$residuals
gamw6 <- gam(y6~s(CR6, k = k, bs = "ps") + s(GDD6, k = k, bs = "ps") + s(SR6, k = k, bs = "ps") + s(VP6, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y7 <- gamw6$residuals
gamw7 <- gam(y7~s(CR7, k = k, bs = "ps") + s(GDD7, k = k, bs = "ps") + s(SR7, k = k, bs = "ps") + s(VP7, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y8 <- gamw7$residuals
gamw8 <- gam(y8~s(CR8, k = k, bs = "ps") + s(GDD8, k = k, bs = "ps") + s(SR8, k = k, bs = "ps") + s(VP8, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y9 <- gamw8$residuals
gamw9 <- gam(y9~s(CR9, k = k, bs = "ps") + s(GDD9, k = k, bs = "ps") + s(SR9, k = k, bs = "ps") + s(VP9, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y10 <- gamw9$residuals
gamw10 <- gam(y10~s(CR10, k = k, bs = "ps") + s(GDD10, k = k, bs = "ps") + s(SR10, k = k, bs = "ps") + s(VP10, k = k, bs = "ps"), data = trainwiweek, method = "REML")

y11 <- gamw10$residuals
gamw11 <- gam(y11~s(CR11, k = k, bs = "ps") + s(GDD11, k = k, bs = "ps") + s(SR11, k = k, bs = "ps") + s(VP11, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y12 <- gamw11$residuals
gamw12 <- gam(y12~s(CR12, k = k, bs = "ps") + s(GDD12, k = k, bs = "ps") + s(SR12, k = k, bs = "ps") + s(VP12, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y13 <- gamw12$residuals
gamw13 <- gam(y13~s(CR13, k = k, bs = "ps") + s(GDD13, k = k, bs = "ps") + s(SR13, k = k, bs = "ps") + s(VP13, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y14 <- gamw13$residuals
gamw14 <- gam(y14~s(CR14, k = k, bs = "ps") + s(GDD14, k = k, bs = "ps") + s(SR14, k = k, bs = "ps") + s(VP14, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y15 <- gamw14$residuals
gamw15 <- gam(y15~s(CR15, k = k, bs = "ps") + s(GDD15, k = k, bs = "ps") + s(SR15, k = k, bs = "ps") + s(VP15, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y16 <- gamw15$residuals
gamw16 <- gam(y16~s(CR16, k = k, bs = "ps") + s(GDD16, k = k, bs = "ps") + s(SR16, k = k, bs = "ps") + s(VP16, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y17 <- gamw16$residuals
gamw17 <- gam(y17~s(CR17, k = k, bs = "ps") + s(GDD17, k = k, bs = "ps") + s(SR17, k = k, bs = "ps") + s(VP17, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y18 <- gamw17$residuals
gamw18 <- gam(y18~s(CR18, k = k, bs = "ps") + s(GDD18, k = k, bs = "ps") + s(SR18, k = k, bs = "ps") + s(VP18, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y19 <- gamw18$residuals
gamw19 <- gam(y19~s(CR19, k = k, bs = "ps") + s(GDD19, k = k, bs = "ps") + s(SR19, k = k, bs = "ps") + s(VP19, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y20 <- gamw19$residuals
gamw20 <- gam(y20~s(CR20, k = k, bs = "ps") + s(GDD20, k = k, bs = "ps") + s(SR20, k = k, bs = "ps") + s(VP20, k = k, bs = "ps"), data = trainwiweek, method = "REML")

y21 <- gamw20$residuals
gamw21 <- gam(y21~s(CR21, k = k, bs = "ps") + s(GDD21, k = k, bs = "ps") + s(SR21, k = k, bs = "ps") + s(VP21, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y22 <- gamw21$residuals
gamw22 <- gam(y22~s(CR22, k = k, bs = "ps") + s(GDD22, k = k, bs = "ps") + s(SR22, k = k, bs = "ps") + s(VP22, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y23 <- gamw22$residuals
gamw23 <- gam(y23~s(CR23, k = k, bs = "ps") + s(GDD23, k = k, bs = "ps") + s(SR23, k = k, bs = "ps") + s(VP23, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y24 <- gamw23$residuals
gamw24 <- gam(y24~s(CR24, k = k, bs = "ps") + s(GDD24, k = k, bs = "ps") + s(SR24, k = k, bs = "ps") + s(VP24, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y25 <- gamw24$residuals
gamw25 <- gam(y25~s(CR25, k = k, bs = "ps") + s(GDD25, k = k, bs = "ps") + s(SR25, k = k, bs = "ps") + s(VP25, k = k, bs = "ps"), data = trainwiweek, method = "REML")
y26 <- gamw25$residuals
gamw26 <- gam(y26~s(CR26, k = k, bs = "ps") + s(GDD26, k = k, bs = "ps") + s(SR26, k = k, bs = "ps") + s(VP26, k = k, bs = "ps"), data = trainwiweek, method = "REML")


gamlist = list(gamw1, gamw2, gamw3, gamw4, gamw5, 
               gamw6, gamw7, gamw8, gamw9, gamw10,
               gamw11, gamw12, gamw13, gamw14, gamw15, 
               gamw16, gamw17, gamw18, gamw19, gamw20,
               gamw21, gamw22, gamw23, gamw24, gamw25, 
               gamw26)

## R2 train

expvar2_train <- sum((y1 - mean(y1))^2)
expvar1_train <- sum((gamw26$residuals)^2)
r2_train = 1 - expvar1_train/expvar2_train

# RMSE train

rmse_train <- sqrt(mean((traindata$Ystar - back(y1 - gamw26$residuals, traindata$Ystar))^2))

## R2 valid

validdata <- weatherweek %>% filter(County %in% valicounty)

yvalid <- range01(validdata$Ystar)

validwiweek <- validdata[,6:109]

for (ii in 1:104){
  validwiweek[,ii] = range01(as.numeric(validwiweek[,ii]))
}

validpredmat = matrix(NA, nrow(validwiweek), 26)

for (weekn in 1:26){
  validpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = validwiweek)
}

validpreds <- rowSums(validpredmat)

expvar2_valid <- sum((yvalid - mean(yvalid))^2) 
expvar1_valid <- sum((yvalid - validpreds)^2)
r2_valid = 1 - expvar1_valid/expvar2_valid

# RMSE valid

rmse_valid <- sqrt(mean((validdata$Ystar - back(validpreds, validdata$Ystar))^2))

## R2 test

testdata <- weatherweek %>% filter(County %in% testcounty)

ytest <- range01(testdata$Ystar)

testwiweek <- testdata[,6:109]

for (ii in 1:104){
  testwiweek[,ii] = range01(as.numeric(testwiweek[,ii]))
}

testpredmat = matrix(NA, nrow(testwiweek), 26)

for (weekn in 1:26){
  testpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = testwiweek)
}

testpreds <- rowSums(testpredmat)

expvar2_test <- sum((ytest - mean(ytest))^2) 
expvar1_test <- sum((ytest - testpreds)^2)
r2_test = 1 - expvar1_test/expvar2_test

# RMSE test

rmse_test <- sqrt(mean((testdata$Ystar - back(testpreds, testdata$Ystar))^2))

#res[[k/5]] = list(k = k, r2_valid = r2_valid)
#}

#write.xlsx(data.frame(do.call(rbind, res)), "TuneK\\GAM_ws_space_S1.xlsx")

## save results

# r2 rmse

r2list = list(r2_train = round(r2_train, 2), r2_valid = round(r2_valid, 2), r2_test = round(r2_test, 2))

rmselist = list(rmse_train = round(rmse_train, 2), rmse_valid = round(rmse_valid, 2), rmse_test = round(rmse_test, 2))

write.xlsx(data.frame(r2list, rmselist), "S1Res\\GAM_ws_space_S1_result.xlsx")

# Fit

traindf <- data.frame(traindata[,1:5], Ynorm = y1, Fit = y1 - gamw26$residuals, Resi = gamw26$residuals)

write.xlsx(traindf, "S1Res\\GAM_ws_space_S1_train.xlsx")

validdf <- data.frame(validdata[,1:5], Ynorm = yvalid, Fit = validpreds, Resi = yvalid - validpreds)

write.xlsx(validdf, "S1Res\\GAM_ws_space_S1_valid.xlsx")

testdf <- data.frame(testdata[,1:5], Ynorm = ytest, Fit = testpreds, Resi = ytest - testpreds)

write.xlsx(testdf, "S1Res\\GAM_ws_space_S1_test.xlsx")
