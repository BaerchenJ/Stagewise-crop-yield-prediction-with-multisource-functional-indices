
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

riname = c("ndvi", "evi", "satvi", "savi", "msi", "gndvi", "grvi", "lswi", "tsavi", "msavi", "wdvi", "ci", "v")

## removed tvi

res = list()

for (rn in 1:length(riname)){

wholedata <- import(paste0('fundat_', riname[rn], '.xlsx'), sep = "")

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

for (j in 1:9){
  valicounty[j] = vatecounty[[j]][1] 
  testcounty[j] = vatecounty[[j]][2] 
}

# train

resi_train_8 <- import("S2Res\\FGAM1D1D_train.xlsx")

resi_train_5 <- resi_train_8 %>% filter(Year %in% c(2018:2022))

y3_train = resi_train_5$Resi2

traindata <- wholedata %>% filter(!County %in% unlist(vatecounty))

trainriweek <- traindata[,5:30]

for (ii in 1:26){
  trainriweek[,ii] = range01(as.numeric(trainriweek[,ii]))
}

#res = list()

k = 5

fRI = as.matrix(trainriweek)

s3fgam <- fgam(y3_train ~ af(fRI, k = 5, bs = 'ps'), method="REML")

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
expvar1_train_s3 <- sum((s3fgam$residuals)^2) # the residuals of stage 3
r2_train_s3 = 1 - expvar1_train_s3/expvar2_train

# RMSE train

# s1

resi_train_43 <- import("S1Res\\FGAM_train.xlsx") # to convert back to original scale, we need data of 43 years

Ystar_train = resi_train_43$Ystar

rmse_train_s1 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi, Ystar_train))^2))

# s2

rmse_train_s2 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi2, Ystar_train))^2))

# s3 

rmse_train_s3 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - s3fgam$residuals, Ystar_train))^2))

# R2 valid

resi_valid_8 <- import("S2Res\\FGAM1D1D_valid.xlsx")

resi_valid_5 <- resi_valid_8 %>% filter(Year %in% c(2018:2022))

y3_valid = resi_valid_5$Resi2

validdata <- wholedata %>% filter(County %in% unlist(valicounty))

validriweek <- validdata[,5:30]

for (ii in 1:26){
  validriweek[,ii] = range01(as.numeric(validriweek[,ii]))
}

source('predict_fgam.R')

validpreds <- predfgam(object = s3fgam, 
                       newdata = list(fRI = as.matrix(validriweek)), 
                       type = "response") 

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

resi_valid_43 <- import("S1Res\\FGAM_valid.xlsx") 

Ystar_valid = resi_valid_43$Ystar

rmse_valid_s1 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi, Ystar_valid))^2))

# s2

rmse_valid_s2 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi2, Ystar_valid))^2))

# s3 

rmse_valid_s3 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - y3_valid + validpreds, Ystar_valid))^2))

# R2 test

resi_test_8 <- import("S2Res\\FGAM1D1D_test.xlsx")

resi_test_5 <- resi_test_8 %>% filter(Year %in% c(2018:2022))

y3_test = resi_test_5$Resi2

testdata <- wholedata %>% filter(County %in% unlist(testcounty))

testriweek <- testdata[,5:30]

for (ii in 1:26){
  testriweek[,ii] = range01(as.numeric(testriweek[,ii]))
}

testpreds <- predfgam(object = s3fgam, 
                      newdata = list(fRI = as.matrix(testriweek)), 
                      type = "response")

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

resi_test_43 <- import("S1Res\\FGAM_test.xlsx") # to convert back to original scale, we need data of 43 years

Ystar_test = resi_test_43$Ystar

rmse_test_s1 <- sqrt(mean((resi_test_5$Ystar - back(ytest - resi_test_5$Resi, Ystar_test))^2))

# s2

rmse_test_s2 <- sqrt(mean((resi_test_5$Ystar - back(ytest - resi_test_5$Resi2, Ystar_test))^2))

# s3 

rmse_test_s3 <- sqrt(mean((resi_test_5$Ystar - back(ytest - y3_test + testpreds, Ystar_test))^2))


res[[rn]] = list(RI = riname[rn], k = k, 
                r2_train_s1 = r2_train_s1, rmse_train_s1 = rmse_train_s1,
                r2_train_s2 = r2_train_s2, rmse_train_s2 = rmse_train_s2,
                r2_train_s3 = r2_train_s3, rmse_train_s3 = rmse_train_s3,
                r2_valid_s1 = r2_valid_s1, rmse_valid_s1 = rmse_valid_s1,
                r2_valid_s2 = r2_valid_s2, rmse_valid_s2 = rmse_valid_s2,
                r2_valid_s3 = r2_valid_s3, rmse_valid_s3 = rmse_valid_s3,
                r2_test_s1 = r2_test_s1, rmse_test_s1 = rmse_test_s1,
                r2_test_s2 = r2_test_s2, rmse_test_s2 = rmse_test_s2,
                r2_test_s3 = r2_test_s3, rmse_test_s3 = rmse_test_s3)
}

resdf = data.frame(do.call(rbind, res))

write.xlsx(resdf, "TuneK\\FGAM1D1D1D_IndexSelection.xlsx")

