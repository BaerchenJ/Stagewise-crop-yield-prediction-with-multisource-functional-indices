
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

########################## Stage 2

spmean <- import('...\\Data\\spmean_adc.xlsx')

ADCnum = unique(spmean$ADC)

ADCGROUP = list()

for (i in 1:length(ADCnum)){
  ADCGROUP[[i]] <- spmean[which(spmean$ADC == ADCnum[i]),]
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

resi_train_43 <- import("S1Res\\GAM_at_space_S1_train.xlsx")

resi_train_8 <- resi_train_43 %>% filter(Year %in% c(2015:2022))

y2_train = resi_train_8$Resi

traindata <- spmean %>% filter(!County %in% unlist(vatecounty))

trainspmean <- traindata[,5:6]

for (ii in 1:2){
  trainspmean[,ii] = range01(as.numeric(trainspmean[,ii]))
}

#res = list()

#for (k in seq(5, 20, by = 5)){

k = 5

s2gam <- gam(y2_train~te(SM, PR, k = k, bs = "ps"), data = trainspmean, method = "REML")

# R2 train

# s1
ytrain = resi_train_8$Ynorm
expvar2_train <- sum((ytrain - mean(ytrain))^2)

expvar1_train_s1 <- sum((y2_train)^2) # the residual of stage 1
r2_train_s1 = 1 - expvar1_train_s1/expvar2_train

# s2
expvar1_train_s2 <- sum((s2gam$residuals)^2)
r2_train_s2 = 1 - expvar1_train_s2/expvar2_train

# RMSE train

# s1

Ystar_train = resi_train_43$Ystar

rmse_train_s1 <- sqrt(mean((resi_train_8$Ystar - back(ytrain - y2_train, Ystar_train))^2))

# s2

rmse_train_s2 <- sqrt(mean((resi_train_8$Ystar - back(ytrain - s2gam$residuals, Ystar_train))^2))

# R2 valid

resi_valid_43 <- import("S1Res\\GAM_at_space_S1_valid.xlsx")

resi_valid_8 <- resi_valid_43 %>% filter(Year %in% c(2015:2022))

y2_valid = resi_valid_8$Resi

validdata <- spmean %>% filter(County %in% unlist(valicounty))

validspmean <- validdata[,5:6]

for (ii in 1:2){
  validspmean[,ii] = range01(as.numeric(validspmean[,ii]))
}

validpreds <- predict(s2gam, newdata = validspmean)

# R2 valid

# s1
yvalid = resi_valid_8$Ynorm
expvar2_valid <- sum((yvalid - mean(yvalid))^2)

expvar1_valid_s1 <- sum((y2_valid)^2) # the residual of stage 1
r2_valid_s1 = 1 - expvar1_valid_s1/expvar2_valid

# s2
expvar1_valid_s2 <- sum((y2_valid - validpreds)^2) # the residual of stage 2
r2_valid_s2 = 1 - expvar1_valid_s2/expvar2_valid

# RMSE valid

# s1

Ystar_valid = resi_valid_43$Ystar

rmse_valid_s1 <- sqrt(mean((resi_valid_8$Ystar - back(yvalid - y2_valid, Ystar_valid))^2))

# s2

rmse_valid_s2 <- sqrt(mean((resi_valid_8$Ystar - back(yvalid - y2_valid + validpreds, Ystar_valid))^2))

# R2 test

resi_test_43 <- import("S1Res\\GAM_at_space_S1_test.xlsx")

resi_test_8 <- resi_test_43 %>% filter(Year %in% c(2015:2022))

y2_test = resi_test_8$Resi

testdata <- spmean %>% filter(County %in% unlist(testcounty))

testspmean <- testdata[,5:6]

for (ii in 1:2){
  testspmean[,ii] = range01(as.numeric(testspmean[,ii]))
}

testpreds <- predict(s2gam, newdata = testspmean)

# R2 test

# s1
ytest = resi_test_8$Ynorm
expvar2_test <- sum((ytest - mean(ytest))^2)

expvar1_test_s1 <- sum((y2_test)^2) # the residual of stage 1
r2_test_s1 = 1 - expvar1_test_s1/expvar2_test

# s2
expvar1_test_s2 <- sum((y2_test - testpreds)^2) # the residual of stage 2
r2_test_s2 = 1 - expvar1_test_s2/expvar2_test

# RMSE test

# s1

Ystar_test = resi_test_43$Ystar

rmse_test_s1 <- sqrt(mean((resi_test_8$Ystar - back(ytest - y2_test, Ystar_test))^2))

# s2

rmse_test_s2 <- sqrt(mean((resi_test_8$Ystar - back(ytest - y2_test + testpreds, Ystar_test))^2))

#res[[k/5]] = list(k = k, r2_valid_s2 = r2_valid_s2)

#}

#resdf = data.frame(do.call(rbind, res))

#write.xlsx(resdf, "TuneK\\GAM_at_space_S2.xlsx")

## save results

# r2 rmse

r2vec_s1 = c(train = round(r2_train_s1, 2), valid = round(r2_valid_s1, 2), test = round(r2_test_s1, 2))

r2vec_s2 = c(train = round(r2_train_s2, 2), valid = round(r2_valid_s2, 2), test = round(r2_test_s2, 2))

rmsevec_s1 = c(train = round(rmse_train_s1, 2), valid = round(rmse_valid_s1, 2), test = round(rmse_test_s1, 2))

rmsevec_s2 = c(train = round(rmse_train_s2, 2), valid = round(rmse_valid_s2, 2), test = round(rmse_test_s2, 2))

write.xlsx(data.frame(r2vec_s1, r2vec_s2, rmsevec_s1, rmsevec_s2), "S2Res\\GAM_at_space_S2_result.xlsx")

# Fit

traindf <- data.frame(resi_train_8, 
                      Fit2 = s2gam$fitted.values, 
                      Resi2 = s2gam$residuals)

write.xlsx(traindf, "S2Res\\GAM_at_space_S2_train.xlsx")

validdf <- data.frame(resi_valid_8, 
                      Fit2 = validpreds, 
                      Resi2 = y2_valid - validpreds)

write.xlsx(validdf, "S2Res\\GAM_at_space_S2_valid.xlsx")

testdf <- data.frame(resi_test_8, 
                     Fit2 = testpreds, 
                     Resi2 = y2_test - testpreds)

write.xlsx(testdf, "S2Res\\GAM_at_space_S2_test.xlsx")
