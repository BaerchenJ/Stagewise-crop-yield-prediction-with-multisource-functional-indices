
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

resi_train_43 <- import("S1Res\\GAM_ws_space_S1_train.xlsx")

resi_train_8 <- resi_train_43 %>% filter(Year %in% c(2015:2022))

s2y1 = resi_train_8$Resi

traindata <- spweek %>% filter(!County %in% unlist(vatecounty))

trainspweek <- traindata[,5:56]

for (ii in 1:52){
  trainspweek[,ii] = range01(as.numeric(trainspweek[,ii]))
}

#res = list()

#for (k in seq(5, 50, by = 5)){

k = 10

gamw1 <- gam(s2y1~s(SM1, k = k, bs = "ps") + s(PR1, k = k, bs = "ps"), data = trainspweek, method = "REML")
y2 <- gamw1$residuals
gamw2 <- gam(y2~s(SM2, k = k, bs = "ps") + s(PR2, k = k, bs = "ps"), data = trainspweek, method = "REML")
y3 <- gamw2$residuals
gamw3 <- gam(y3~s(SM3, k = k, bs = "ps") + s(PR3, k = k, bs = "ps"), data = trainspweek, method = "REML")
y4 <- gamw3$residuals
gamw4 <- gam(y4~s(SM4, k = k, bs = "ps") + s(PR4, k = k, bs = "ps"), data = trainspweek, method = "REML")
y5 <- gamw4$residuals
gamw5 <- gam(y5~s(SM5, k = k, bs = "ps") + s(PR5, k = k, bs = "ps"), data = trainspweek, method = "REML")
y6 <- gamw5$residuals
gamw6 <- gam(y6~s(SM6, k = k, bs = "ps") + s(PR6, k = k, bs = "ps"), data = trainspweek, method = "REML")
y7 <- gamw6$residuals
gamw7 <- gam(y7~s(SM7, k = k, bs = "ps") + s(PR7, k = k, bs = "ps"), data = trainspweek, method = "REML")
y8 <- gamw7$residuals
gamw8 <- gam(y8~s(SM8, k = k, bs = "ps") + s(PR8, k = k, bs = "ps"), data = trainspweek, method = "REML")
y9 <- gamw8$residuals
gamw9 <- gam(y9~s(SM9, k = k, bs = "ps") + s(PR9, k = k, bs = "ps"), data = trainspweek, method = "REML")
y10 <- gamw9$residuals
gamw10 <- gam(y10~s(SM10, k = k, bs = "ps") + s(PR10, k = k, bs = "ps"), data = trainspweek, method = "REML")

y11 <- gamw10$residuals
gamw11 <- gam(y11~s(SM11, k = k, bs = "ps") + s(PR11, k = k, bs = "ps"), data = trainspweek, method = "REML")
y12 <- gamw11$residuals
gamw12 <- gam(y12~s(SM12, k = k, bs = "ps") + s(PR12, k = k, bs = "ps"), data = trainspweek, method = "REML")
y13 <- gamw12$residuals
gamw13 <- gam(y13~s(SM13, k = k, bs = "ps") + s(PR13, k = k, bs = "ps"), data = trainspweek, method = "REML")
y14 <- gamw13$residuals
gamw14 <- gam(y14~s(SM14, k = k, bs = "ps") + s(PR14, k = k, bs = "ps"), data = trainspweek, method = "REML")
y15 <- gamw14$residuals
gamw15 <- gam(y15~s(SM15, k = k, bs = "ps") + s(PR15, k = k, bs = "ps"), data = trainspweek, method = "REML")
y16 <- gamw15$residuals
gamw16 <- gam(y16~s(SM16, k = k, bs = "ps") + s(PR16, k = k, bs = "ps"), data = trainspweek, method = "REML")
y17 <- gamw16$residuals
gamw17 <- gam(y17~s(SM17, k = k, bs = "ps") + s(PR17, k = k, bs = "ps"), data = trainspweek, method = "REML")
y18 <- gamw17$residuals
gamw18 <- gam(y18~s(SM18, k = k, bs = "ps") + s(PR18, k = k, bs = "ps"), data = trainspweek, method = "REML")
y19 <- gamw18$residuals
gamw19 <- gam(y19~s(SM19, k = k, bs = "ps") + s(PR19, k = k, bs = "ps"), data = trainspweek, method = "REML")
y20 <- gamw19$residuals
gamw20 <- gam(y20~s(SM20, k = k, bs = "ps") + s(PR20, k = k, bs = "ps"), data = trainspweek, method = "REML")

y21 <- gamw20$residuals
gamw21 <- gam(y21~s(SM21, k = k, bs = "ps") + s(PR21, k = k, bs = "ps"), data = trainspweek, method = "REML")
y22 <- gamw21$residuals
gamw22 <- gam(y22~s(SM22, k = k, bs = "ps") + s(PR22, k = k, bs = "ps"), data = trainspweek, method = "REML")
y23 <- gamw22$residuals
gamw23 <- gam(y23~s(SM23, k = k, bs = "ps") + s(PR23, k = k, bs = "ps"), data = trainspweek, method = "REML")
y24 <- gamw23$residuals
gamw24 <- gam(y24~s(SM24, k = k, bs = "ps") + s(PR24, k = k, bs = "ps"), data = trainspweek, method = "REML")
y25 <- gamw24$residuals
gamw25 <- gam(y25~s(SM25, k = k, bs = "ps") + s(PR25, k = k, bs = "ps"), data = trainspweek, method = "REML")
y26 <- gamw25$residuals
gamw26 <- gam(y26~s(SM26, k = k, bs = "ps") + s(PR26, k = k, bs = "ps"), data = trainspweek, method = "REML")


gamlist = list(gamw1, gamw2, gamw3, gamw4, gamw5, 
               gamw6, gamw7, gamw8, gamw9, gamw10,
               gamw11, gamw12, gamw13, gamw14, gamw15, 
               gamw16, gamw17, gamw18, gamw19, gamw20,
               gamw21, gamw22, gamw23, gamw24, gamw25, 
               gamw26)


# R2 train

# s1
ytrain = resi_train_8$Ynorm
expvar2_train <- sum((ytrain - mean(ytrain))^2)

expvar1_train_s1 <- sum((s2y1)^2) # s2y1 is the residual of stage 1
r2_train_s1 = 1 - expvar1_train_s1/expvar2_train

# s2
expvar1_train_s2 <- sum((gamw26$residuals)^2)
r2_train_s2 = 1 - expvar1_train_s2/expvar2_train

# RMSE train

# s1

Ystar_train = resi_train_43$Ystar

rmse_train_s1 <- sqrt(mean((resi_train_8$Ystar - back(ytrain - s2y1, Ystar_train))^2))

# s2

rmse_train_s2 <- sqrt(mean((resi_train_8$Ystar - back(ytrain - gamw26$residuals, Ystar_train))^2))

# R2 valid

resi_valid_43 <- import("S1Res\\GAM_ws_space_S1_valid.xlsx")

resi_valid_8 <- resi_valid_43 %>% filter(Year %in% c(2015:2022))

y2_valid = resi_valid_8$Resi

validdata <- spweek %>% filter(County %in% unlist(valicounty))

validspweek <- validdata[,5:56]

for (ii in 1:52){
  validspweek[,ii] = range01(as.numeric(validspweek[,ii]))
}

validpredmat = matrix(NA, nrow(validspweek), 26)

for (weekn in 1:26){
  validpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = validspweek)
}

validpreds <- rowSums(validpredmat)

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

resi_test_43 <- import("S1Res\\GAM_ws_space_S1_test.xlsx")

resi_test_8 <- resi_test_43 %>% filter(Year %in% c(2015:2022))

y2_test = resi_test_8$Resi

testdata <- spweek %>% filter(County %in% unlist(testcounty))

testspweek <- testdata[,5:56]

for (ii in 1:52){
  testspweek[,ii] = range01(as.numeric(testspweek[,ii]))
}

testpredmat = matrix(NA, nrow(testspweek), 26)

for (weekn in 1:26){
  testpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = testspweek)
}

testpreds <- rowSums(testpredmat)

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

#write.xlsx(resdf, "TuneK\\GAM_ws_space_S2.xlsx")

## save results

# r2 rmse

r2vec_s1 = c(train = round(r2_train_s1, 2), valid = round(r2_valid_s1, 2), test = round(r2_test_s1, 2))

r2vec_s2 = c(train = round(r2_train_s2, 2), valid = round(r2_valid_s2, 2), test = round(r2_test_s2, 2))

rmsevec_s1 = c(train = round(rmse_train_s1, 2), valid = round(rmse_valid_s1, 2), test = round(rmse_test_s1, 2))

rmsevec_s2 = c(train = round(rmse_train_s2, 2), valid = round(rmse_valid_s2, 2), test = round(rmse_test_s2, 2))

write.xlsx(data.frame(r2vec_s1, r2vec_s2, rmsevec_s1, rmsevec_s2), "S2Res\\GAM_ws_space_S2_result.xlsx")

# Fit

traindf <- data.frame(resi_train_8, 
                      Fit2 = s2y1 - gamw26$residuals, 
                      Resi2 = gamw26$residuals)

write.xlsx(traindf, "S2Res\\GAM_ws_space_S2_train.xlsx")

validdf <- data.frame(resi_valid_8, 
                      Fit2 = validpreds, 
                      Resi2 = y2_valid - validpreds)

write.xlsx(validdf, "S2Res\\GAM_ws_space_S2_valid.xlsx")

testdf <- data.frame(resi_test_8, 
                     Fit2 = testpreds, 
                     Resi2 = y2_test - testpreds)

write.xlsx(testdf, "S2Res\\GAM_ws_space_S2_test.xlsx")


