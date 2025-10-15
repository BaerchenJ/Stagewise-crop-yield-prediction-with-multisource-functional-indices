
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

riname = c("ndvi", "evi", "satvi", "savi", "msi", "gndvi", "grvi", "lswi", "tsavi", "msavi", "wdvi", "ci", "v")

# remove "tvi"
#Fehler in gam(y7 ~ s(G, k = k, bs = "ps"), data = trainriweek, method = "REML") : 
#Nicht genug (nicht-NA-) Daten, um etwas Sinnvolles zu tun

res = list()

for (rn in 1:length(riname)){

#rn = 9

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

resi_train_8 <- import("S2Res\\GAMW1D1D_train.xlsx")

resi_train_5 <- resi_train_8 %>% filter(Year %in% c(2018:2022))

y3_train = resi_train_5$Resi2

traindata <- wholedata %>% filter(!County %in% unlist(vatecounty))

trainriweek <- traindata[,5:30]

for (ii in 1:26){
  trainriweek[,ii] = range01(as.numeric(trainriweek[,ii]))
}

colnames(trainriweek) = LETTERS[seq( from = 1, to = 26 )]

#res = list()

#for (k in seq(5, 50, by = 5)){
  
k = 5

gamw1 <- gam(y3_train~s(A, k = k, bs = "ps"), data = trainriweek, method = "REML")
y2 <- gamw1$residuals
gamw2 <- gam(y2~s(B, k = k, bs = "ps"), data = trainriweek, method = "REML")
y3 <- gamw2$residuals
gamw3 <- gam(y3~s(C, k = k, bs = "ps"), data = trainriweek, method = "REML")
y4 <- gamw3$residuals
gamw4 <- gam(y4~s(D, k = k, bs = "ps"), data = trainriweek, method = "REML")
y5 <- gamw4$residuals
gamw5 <- gam(y5~s(E, k = k, bs = "ps"), data = trainriweek, method = "REML")
y6 <- gamw5$residuals
gamw6 <- gam(y6~s(F, k = k, bs = "ps"), data = trainriweek, method = "REML")
y7 <- gamw6$residuals
gamw7 <- gam(y7~s(G, k = k, bs = "ps"), data = trainriweek, method = "REML")
y8 <- gamw7$residuals
gamw8 <- gam(y8~s(H, k = k, bs = "ps"), data = trainriweek, method = "REML")
y9 <- gamw8$residuals
gamw9 <- gam(y9~s(I, k = k, bs = "ps"), data = trainriweek, method = "REML")
y10 <- gamw9$residuals
gamw10 <- gam(y10~s(J, k = k, bs = "ps"), data = trainriweek, method = "REML")

y11 <- gamw10$residuals
gamw11 <- gam(y11~s(K, k = k, bs = "ps"), data = trainriweek, method = "REML")
y12 <- gamw11$residuals
gamw12 <- gam(y12~s(L, k = k, bs = "ps"), data = trainriweek, method = "REML")
y13 <- gamw12$residuals
gamw13 <- gam(y13~s(M, k = k, bs = "ps"), data = trainriweek, method = "REML")
y14 <- gamw13$residuals
gamw14 <- gam(y14~s(N, k = k, bs = "ps"), data = trainriweek, method = "REML")
y15 <- gamw14$residuals
gamw15 <- gam(y15~s(O, k = k, bs = "ps"), data = trainriweek, method = "REML")
y16 <- gamw15$residuals
gamw16 <- gam(y16~s(P, k = k, bs = "ps"), data = trainriweek, method = "REML")
y17 <- gamw16$residuals
gamw17 <- gam(y17~s(Q, k = k, bs = "ps"), data = trainriweek, method = "REML")
y18 <- gamw17$residuals
gamw18 <- gam(y18~s(R, k = k, bs = "ps"), data = trainriweek, method = "REML")
y19 <- gamw18$residuals
gamw19 <- gam(y19~s(S, k = k, bs = "ps"), data = trainriweek, method = "REML")
y20 <- gamw19$residuals
gamw20 <- gam(y20~s(T, k = k, bs = "ps"), data = trainriweek, method = "REML")

y21 <- gamw20$residuals
gamw21 <- gam(y21~s(U, k = k, bs = "ps"), data = trainriweek, method = "REML")
y22 <- gamw21$residuals
gamw22 <- gam(y22~s(V, k = k, bs = "ps"), data = trainriweek, method = "REML")
y23 <- gamw22$residuals
gamw23 <- gam(y23~s(W, k = k, bs = "ps"), data = trainriweek, method = "REML")
y24 <- gamw23$residuals
gamw24 <- gam(y24~s(X, k = k, bs = "ps"), data = trainriweek, method = "REML")
y25 <- gamw24$residuals
gamw25 <- gam(y25~s(Y, k = k, bs = "ps"), data = trainriweek, method = "REML")
y26 <- gamw25$residuals
gamw26 <- gam(y26~s(Z, k = k, bs = "ps"), data = trainriweek, method = "REML")


gamlist = list(gamw1, gamw2, gamw3, gamw4, gamw5, 
               gamw6, gamw7, gamw8, gamw9, gamw10,
               gamw11, gamw12, gamw13, gamw14, gamw15, 
               gamw16, gamw17, gamw18, gamw19, gamw20,
               gamw21, gamw22, gamw23, gamw24, gamw25, 
               gamw26)

  
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
  expvar1_train_s3 <- sum((gamw26$residuals)^2) # the residuals of stage 3
  r2_train_s3 = 1 - expvar1_train_s3/expvar2_train

  # RMSE train
  
  # s1
  
  resi_train_43 <- import("S1Res\\GAMW1D_train.xlsx") # to convert back to original scale, we need data of 43 years
  
  Ystar_train = resi_train_43$Ystar
  
  rmse_train_s1 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi, Ystar_train))^2))
  
  # s2
  
  rmse_train_s2 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi2, Ystar_train))^2))
  
  # s3 
  
  rmse_train_s3 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - gamw26$residuals, Ystar_train))^2))
  
  # R2 valid
  
  resi_valid_8 <- import("S2Res\\GAMW1D1D_valid.xlsx")
  
  resi_valid_5 <- resi_valid_8 %>% filter(Year %in% c(2018:2022))
  
  y3_valid = resi_valid_5$Resi2
  
  validdata <- wholedata %>% filter(County %in% unlist(valicounty))
  
  validriweek <- validdata[,5:30]
  
  for (ii in 1:26){
    validriweek[,ii] = range01(as.numeric(validriweek[,ii]))
  }
  
  colnames(validriweek) = LETTERS[seq( from = 1, to = 26 )]
  
  validpredmat = matrix(NA, nrow(validriweek), 26)
  
  for (weekn in 1:26){
    validpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = validriweek)
  }
  
  validpreds <- rowSums(validpredmat)
  
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
  
  resi_valid_43 <- import("S1Res\\GAMW1D_valid.xlsx") 
  
  Ystar_valid = resi_valid_43$Ystar
  
  rmse_valid_s1 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi, Ystar_valid))^2))
  
  # s2
  
  rmse_valid_s2 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi2, Ystar_valid))^2))
  
  # s3 
  
  rmse_valid_s3 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - y3_valid + validpreds, Ystar_valid))^2))
  
  
  # R2 test
  
  resi_test_8 <- import("S2Res\\GAMW1D1D_test.xlsx")
  
  resi_test_5 <- resi_test_8 %>% filter(Year %in% c(2018:2022))
  
  y3_test = resi_test_5$Resi2
  
  testdata <- wholedata %>% filter(County %in% unlist(testcounty))
  
  testriweek <- testdata[,5:30]
  
  for (ii in 1:26){
    testriweek[,ii] = range01(as.numeric(testriweek[,ii]))
  }
  
  colnames(testriweek) = LETTERS[seq( from = 1, to = 26 )]
  
  testpredmat = matrix(NA, nrow(testriweek), 26)
  
  for (weekn in 1:26){
    testpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = testriweek)
  }
  
  testpreds <- rowSums(testpredmat)
  
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
  
  resi_test_43 <- import("S1Res\\GAMW1D_test.xlsx") # to convert back to original scale, we need data of 43 years
  
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

write.xlsx(resdf, "TuneK\\GAMW1D1D_IndexSelection.xlsx")


####################### 2D case #################################

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

riname = c("ndvi", "evi", "satvi", "savi", "msi", "gndvi", "grvi", "lswi", "tsavi", "msavi", "wdvi", "ci", "v")

# remove "tvi"
#Fehler in gam(y7 ~ s(G, k = k, bs = "ps"), data = trainriweek, method = "REML") : 
#Nicht genug (nicht-NA-) Daten, um etwas Sinnvolles zu tun

res = list()

rn = 1

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

resi_train_8 <- import("S2Res\\GAMW1D1D_train.xlsx")

resi_train_5 <- resi_train_8 %>% filter(Year %in% c(2018:2022))

y3_train = resi_train_5$Resi2

traindata <- wholedata %>% filter(!County %in% unlist(vatecounty))

trainriweek <- traindata[,5:30]

for (ii in 1:26){
  trainriweek[,ii] = range01(as.numeric(trainriweek[,ii]))
}

colnames(trainriweek) = LETTERS[seq( from = 1, to = 26 )]


## EVI data

wholedata_EVI <- import(paste0('fundat_', riname[2], '.xlsx'), sep = "")

traindata_EVI <- wholedata_EVI %>% filter(!County %in% unlist(vatecounty))

trainriweek_EVI <- traindata_EVI[,5:30]

for (ii in 1:26){
  trainriweek_EVI[,ii] = range01(as.numeric(trainriweek_EVI[,ii]))
}

colnames(trainriweek_EVI) = paste0(LETTERS[seq(1, 26)], 1)

trainriweek_NDVIEVI = cbind(trainriweek, trainriweek_EVI)

k = 5

gamw1 <- gam(y3_train~s(A, k = k, bs = "ps") + s(A1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y2 <- gamw1$residuals
gamw2 <- gam(y2~s(B, k = k, bs = "ps") + s(B1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y3 <- gamw2$residuals
gamw3 <- gam(y3~s(C, k = k, bs = "ps") + s(C1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y4 <- gamw3$residuals
gamw4 <- gam(y4~s(D, k = k, bs = "ps") + s(D1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y5 <- gamw4$residuals
gamw5 <- gam(y5~s(E, k = k, bs = "ps") + s(E1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y6 <- gamw5$residuals
gamw6 <- gam(y6~s(F, k = k, bs = "ps") + s(F1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y7 <- gamw6$residuals
gamw7 <- gam(y7~s(G, k = k, bs = "ps") + s(G1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y8 <- gamw7$residuals
gamw8 <- gam(y8~s(H, k = k, bs = "ps") + s(H1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y9 <- gamw8$residuals
gamw9 <- gam(y9~s(I, k = k, bs = "ps") + s(I1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y10 <- gamw9$residuals
gamw10 <- gam(y10~s(J, k = k, bs = "ps") + s(J1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")

y11 <- gamw10$residuals
gamw11 <- gam(y11~s(K, k = k, bs = "ps") + s(K1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y12 <- gamw11$residuals
gamw12 <- gam(y12~s(L, k = k, bs = "ps") + s(L1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y13 <- gamw12$residuals
gamw13 <- gam(y13~s(M, k = k, bs = "ps") + s(M1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y14 <- gamw13$residuals
gamw14 <- gam(y14~s(N, k = k, bs = "ps") + s(N1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y15 <- gamw14$residuals
gamw15 <- gam(y15~s(O, k = k, bs = "ps") + s(O1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y16 <- gamw15$residuals
gamw16 <- gam(y16~s(P, k = k, bs = "ps") + s(P1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y17 <- gamw16$residuals
gamw17 <- gam(y17~s(Q, k = k, bs = "ps") + s(Q1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y18 <- gamw17$residuals
gamw18 <- gam(y18~s(R, k = k, bs = "ps") + s(R1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y19 <- gamw18$residuals
gamw19 <- gam(y19~s(S, k = k, bs = "ps") + s(S1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y20 <- gamw19$residuals
gamw20 <- gam(y20~s(T, k = k, bs = "ps") + s(T1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")

y21 <- gamw20$residuals
gamw21 <- gam(y21~s(U, k = k, bs = "ps") + s(U1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y22 <- gamw21$residuals
gamw22 <- gam(y22~s(V, k = k, bs = "ps") + s(V1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y23 <- gamw22$residuals
gamw23 <- gam(y23~s(W, k = k, bs = "ps") + s(W1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y24 <- gamw23$residuals
gamw24 <- gam(y24~s(X, k = k, bs = "ps") + s(X1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y25 <- gamw24$residuals
gamw25 <- gam(y25~s(Y, k = k, bs = "ps") + s(Y1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")
y26 <- gamw25$residuals
gamw26 <- gam(y26~s(Z, k = k, bs = "ps") + s(Z1, k = k, bs = "ps"), data = trainriweek_NDVIEVI, method = "REML")


gamlist = list(gamw1, gamw2, gamw3, gamw4, gamw5, 
               gamw6, gamw7, gamw8, gamw9, gamw10,
               gamw11, gamw12, gamw13, gamw14, gamw15, 
               gamw16, gamw17, gamw18, gamw19, gamw20,
               gamw21, gamw22, gamw23, gamw24, gamw25, 
               gamw26)


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
expvar1_train_s3 <- sum((gamw26$residuals)^2) # the residuals of stage 3
r2_train_s3 = 1 - expvar1_train_s3/expvar2_train

# RMSE train

# s1

resi_train_43 <- import("S1Res\\GAMW1D_train.xlsx") # to convert back to original scale, we need data of 43 years

Ystar_train = resi_train_43$Ystar

rmse_train_s1 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi, Ystar_train))^2))

# s2

rmse_train_s2 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - resi_train_5$Resi2, Ystar_train))^2))

# s3 

rmse_train_s3 <- sqrt(mean((resi_train_5$Ystar - back(ytrain - gamw26$residuals, Ystar_train))^2))


# R2 valid

resi_valid_8 <- import("S2Res\\GAMW1D1D_valid.xlsx")

resi_valid_5 <- resi_valid_8 %>% filter(Year %in% c(2018:2022))

y3_valid = resi_valid_5$Resi2

validdata <- wholedata %>% filter(County %in% unlist(valicounty))

validriweek <- validdata[,5:30]

for (ii in 1:26){
  validriweek[,ii] = range01(as.numeric(validriweek[,ii]))
}

colnames(validriweek) = LETTERS[seq( from = 1, to = 26 )]

## EVI data

validdata_EVI <- wholedata_EVI %>% filter(County %in% unlist(valicounty))

validriweek_EVI <- validdata_EVI[,5:30]

for (ii in 1:26){
  validriweek_EVI[,ii] = range01(as.numeric(validriweek_EVI[,ii]))
}

colnames(validriweek_EVI) = paste0(LETTERS[seq(1, 26)], 1)

validriweek_NDVIEVI = cbind(validriweek, validriweek_EVI)


validpredmat = matrix(NA, nrow(validriweek), 26)

for (weekn in 1:26){
  validpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = validriweek_NDVIEVI)
}

validpreds <- rowSums(validpredmat)

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

resi_valid_43 <- import("S1Res\\GAMW1D_valid.xlsx") 

Ystar_valid = resi_valid_43$Ystar

rmse_valid_s1 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi, Ystar_valid))^2))

# s2

rmse_valid_s2 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - resi_valid_5$Resi2, Ystar_valid))^2))

# s3 

rmse_valid_s3 <- sqrt(mean((resi_valid_5$Ystar - back(yvalid - y3_valid + validpreds, Ystar_valid))^2))



# R2 test

resi_test_8 <- import("S2Res\\GAMW1D1D_test.xlsx")

resi_test_5 <- resi_test_8 %>% filter(Year %in% c(2018:2022))

y3_test = resi_test_5$Resi2

testdata <- wholedata %>% filter(County %in% unlist(testcounty))

testriweek <- testdata[,5:30]

for (ii in 1:26){
  testriweek[,ii] = range01(as.numeric(testriweek[,ii]))
}

colnames(testriweek) = LETTERS[seq( from = 1, to = 26 )]

## EVI data

testdata_EVI <- wholedata_EVI %>% filter(County %in% unlist(testcounty))

testriweek_EVI <- testdata_EVI[,5:30]

for (ii in 1:26){
  testriweek_EVI[,ii] = range01(as.numeric(testriweek_EVI[,ii]))
}

colnames(testriweek_EVI) = paste0(LETTERS[seq(1, 26)], 1)

testriweek_NDVIEVI = cbind(testriweek, testriweek_EVI)


testpredmat = matrix(NA, nrow(testriweek), 26)

for (weekn in 1:26){
  testpredmat[,weekn] <- predict(object = gamlist[[weekn]], newdata = testriweek_NDVIEVI)
}

testpreds <- rowSums(testpredmat)

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

resi_test_43 <- import("S1Res\\GAMW1D_test.xlsx") # to convert back to original scale, we need data of 43 years

Ystar_test = resi_test_43$Ystar

rmse_test_s1 <- sqrt(mean((resi_test_5$Ystar - back(ytest - resi_test_5$Resi, Ystar_test))^2))

# s2

rmse_test_s2 <- sqrt(mean((resi_test_5$Ystar - back(ytest - resi_test_5$Resi2, Ystar_test))^2))

# s3 

rmse_test_s3 <- sqrt(mean((resi_test_5$Ystar - back(ytest - y3_test + testpreds, Ystar_test))^2))

res[[1]] = list(RI1 = riname[1], RI2 = riname[2], k = k, 
                r2_train_s1 = r2_train_s1, rmse_train_s1 = rmse_train_s1,
                r2_train_s2 = r2_train_s2, rmse_train_s2 = rmse_train_s2,
                r2_train_s3 = r2_train_s3, rmse_train_s3 = rmse_train_s3,
                r2_valid_s1 = r2_valid_s1, rmse_valid_s1 = rmse_valid_s1,
                r2_valid_s2 = r2_valid_s2, rmse_valid_s2 = rmse_valid_s2,
                r2_valid_s3 = r2_valid_s3, rmse_valid_s3 = rmse_valid_s3,
                r2_test_s1 = r2_test_s1, rmse_test_s1 = rmse_test_s1,
                r2_test_s2 = r2_test_s2, rmse_test_s2 = rmse_test_s2,
                r2_test_s3 = r2_test_s3, rmse_test_s3 = rmse_test_s3)

resdf = data.frame(do.call(rbind, res))

write.xlsx(resdf, "TuneK\\GAMW1D1D_NDVIEVI.xlsx")




