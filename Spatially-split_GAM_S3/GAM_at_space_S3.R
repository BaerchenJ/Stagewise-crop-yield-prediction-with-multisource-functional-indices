
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

########################## Stage 3

rimean <- import('...\\Data\\rimean_adc.xlsx')

ADCnum = unique(rimean$ADC)

ADCGROUP = list()

for (i in 1:length(ADCnum)){
  ADCGROUP[[i]] <- rimean[which(rimean$ADC == ADCnum[i]),]
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

resi_train_8 <- import("S2Res\\GAM_at_space_S2_train.xlsx")

resi_train_5 <- resi_train_8 %>% filter(Year %in% c(2018:2022))

y3_train = resi_train_5$Resi2

traindata <- rimean %>% filter(!County %in% unlist(vatecounty))

trainrimean <- traindata[,5:18]

for (ii in 1:14){
  trainrimean[,ii] = range01(as.numeric(trainrimean[,ii]))
}

res = list()

# first fix k = 5 to find the most predictive index
#k = 5

#for (cc in 1:ncol(trainrimean)){

# second fix cc to tune k
cc = 1

RI = trainrimean[,cc]


#for (k in seq(5, 50, by = 5)){
k = 5

s3gam <- gam(y3_train~s(RI, k = k, bs = "ps"), method = "REML")


# R2 train

# s1
ytrain = resi_train_5$Ynorm
expvar2_train <- sum((ytrain - mean(ytrain))^2)

expvar1_train_s1 <- sum((resi_train_5$Resi)^2) # the residuals of stage 1
r2_train_s1 = 1 - expvar1_train_s1/expvar2_train

# s2
expvar1_train_s2 <- sum((resi_train_5$Resi2)^2) # the residuals of stage 2
r2_train_s2 = 1 - expvar1_train_s2/expvar2_train

# s3
expvar1_train_s3 <- sum((s3gam$residuals)^2) # the residuals of stage 3
r2_train_s3 = 1 - expvar1_train_s3/expvar2_train

# RMSE train

# s1

resi_train_43 <- import("S1Res\\GAM_at_space_S1_train.xlsx") # to convert back to original scale, we need data of 43 years

Ystar_train = resi_train_43$Ystar

rmse_train_s1 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi, Ystar_train))^2))

# s2

rmse_train_s2 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi2, Ystar_train))^2))

# s3 

rmse_train_s3 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - s3gam$residuals, Ystar_train))^2))

# R2 valid

resi_valid_8 <- import("S2Res\\GAM_at_space_S2_valid.xlsx")

resi_valid_5 <- resi_valid_8 %>% filter(Year %in% c(2018:2022))

y3_valid = resi_valid_5$Resi2

validdata <- rimean %>% filter(County %in% unlist(valicounty))

validrimean <- validdata[,5:18]

for (ii in 1:14){
  validrimean[,ii] = range01(as.numeric(validrimean[,ii]))
}

validri = data.frame(RI = validrimean[,cc])

validpreds <- predict(s3gam, newdata = validri)

# R2 valid

# s1
yvalid = resi_valid_5$Ynorm
expvar2_valid <- sum((yvalid - mean(yvalid))^2)

expvar1_valid_s1 <- sum((resi_valid_5$Resi)^2) # the residuals of stage 1
r2_valid_s1 = 1 - expvar1_valid_s1/expvar2_valid

# s2
expvar1_valid_s2 <- sum((resi_valid_5$Resi2)^2) # the residuals of stage 2
r2_valid_s2 = 1 - expvar1_valid_s2/expvar2_valid

# s3
expvar1_valid_s3 <- sum((y3_valid - validpreds)^2) # the residuals of stage 3
r2_valid_s3 = 1 - expvar1_valid_s3/expvar2_valid

# RMSE valid

# s1

resi_valid_43 <- import("S1Res\\GAM_at_space_S1_valid.xlsx") 

Ystar_valid = resi_valid_43$Ystar

rmse_valid_s1 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi, Ystar_valid))^2))

# s2

rmse_valid_s2 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi2, Ystar_valid))^2))

# s3 

rmse_valid_s3 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - y3_valid + validpreds, Ystar_valid))^2))


# R2 test

resi_test_8 <- import("S2Res\\GAM_at_space_S2_test.xlsx")

resi_test_5 <- resi_test_8 %>% filter(Year %in% c(2018:2022))

y3_test = resi_test_5$Resi2

testdata <- rimean %>% filter(County %in% unlist(testcounty))

testrimean <- testdata[,5:18]

for (ii in 1:14){
  testrimean[,ii] = range01(as.numeric(testrimean[,ii]))
}

testri = data.frame(RI = testrimean[,cc])

testpreds <- predict(s3gam, newdata = testri)

# R2 test

# s1
ytest = resi_test_5$Ynorm
expvar2_test <- sum((ytest - mean(ytest))^2)

expvar1_test_s1 <- sum((resi_test_5$Resi)^2) # the residuals of stage 1
r2_test_s1 = 1 - expvar1_test_s1/expvar2_test

# s2
expvar1_test_s2 <- sum((resi_test_5$Resi2)^2) # the residuals of stage 2
r2_test_s2 = 1 - expvar1_test_s2/expvar2_test

# s3
expvar1_test_s3 <- sum((y3_test - testpreds)^2) # the residuals of stage 3
r2_test_s3 = 1 - expvar1_test_s3/expvar2_test

# RMSE test

# s1

resi_test_43 <- import("S1Res\\GAM_at_space_S1_test.xlsx") # to convert back to original scale, we need data of 43 years

Ystar_test = resi_test_43$Ystar

rmse_test_s1 <- sqrt(mean((resi_test_5$Ystar - back(ytest - resi_test_5$Resi, Ystar_test))^2))

# s2

rmse_test_s2 <- sqrt(mean((resi_test_5$Ystar - back(ytest - resi_test_5$Resi2, Ystar_test))^2))

# s3 

rmse_test_s3 <- sqrt(mean((resi_test_5$Ystar - back(ytest - y3_test + testpreds, Ystar_test))^2))

#res[[cc]] = list(cc = cc, k = k, r2_valid_s2 = r2_valid_s2, r2_valid_s3 = r2_valid_s3, r2_test_s2 = r2_test_s2, r2_test_s3 = r2_test_s3)

# res[[k/5]]
#}

#resdf = data.frame(do.call(rbind, res))

#write.xlsx(resdf, "TuneK\\GAM_at_space_S3_IndexSelection.xlsx")

#write.xlsx(resdf, "TuneK\\GAM_at_space_S3_NDVI_5.xlsx")

## save results

# r2 rmse

r2vec_s1 = c(train = round(r2_train_s1, 2), valid = round(r2_valid_s1, 2), test = round(r2_test_s1, 2))

r2vec_s2 = c(train = round(r2_train_s2, 2), valid = round(r2_valid_s2, 2), test = round(r2_test_s2, 2))

r2vec_s3 = c(train = round(r2_train_s3, 2), valid = round(r2_valid_s3, 2), test = round(r2_test_s3, 2))

rmsevec_s1 = c(train = round(rmse_train_s1, 2), valid = round(rmse_valid_s1, 2), test = round(rmse_test_s1, 2))

rmsevec_s2 = c(train = round(rmse_train_s2, 2), valid = round(rmse_valid_s2, 2), test = round(rmse_test_s2, 2))

rmsevec_s3 = c(train = round(rmse_train_s3, 2), valid = round(rmse_valid_s3, 2), test = round(rmse_test_s3, 2))

write.xlsx(data.frame(r2vec_s1, r2vec_s2, r2vec_s3, rmsevec_s1, rmsevec_s2, rmsevec_s3), "S3Res\\GAM_at_space_S3_NDVI_result.xlsx")

# Fit

traindf <- data.frame(resi_train_5, 
                      Fit3 = s3gam$fitted.values, 
                      Resi3 = s3gam$residuals)

write.xlsx(traindf, "S3Res\\GAM_at_space_S3_NDVI_train.xlsx")

validdf <- data.frame(resi_valid_5, 
                      Fit3 = validpreds, 
                      Resi3 = y3_valid - validpreds)

write.xlsx(validdf, "S3Res\\GAM_at_space_S3_NDVI_valid.xlsx")

testdf <- data.frame(resi_test_5, 
                     Fit3 = testpreds, 
                     Resi3 = y3_test - testpreds)

write.xlsx(testdf, "S3Res\\GAM_at_space_S3_NDVI_test.xlsx")

