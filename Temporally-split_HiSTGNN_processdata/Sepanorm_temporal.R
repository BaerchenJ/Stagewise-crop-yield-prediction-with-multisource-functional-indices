
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

library(rio)
library(dplyr)
library(openxlsx)

wholedata <- import("fundat_wi.xlsx")

## train

traindata <- wholedata %>% filter(Year %in% seq(1980, 2013))

trainwiweek <- traindata[,5:109]

for (ii in 1:105){
  trainwiweek[,ii] = range01(as.numeric(trainwiweek[,ii]))
}

write.xlsx(cbind(traindata[,1:4], trainwiweek), "trainnorm.xlsx")

## valid

validdata <- wholedata %>% filter(Year %in% seq(2014, 2018))

validwiweek <- validdata[,5:109]

for (ii in 1:105){
  validwiweek[,ii] = range01(as.numeric(validwiweek[,ii]))
}

write.xlsx(cbind(validdata[,1:4], validwiweek), "validnorm.xlsx")

## test

testdata <- wholedata %>% filter(Year %in% seq(2019, 2022))

testwiweek <- testdata[,5:109]

for (ii in 1:105){
  testwiweek[,ii] = range01(as.numeric(testwiweek[,ii]))
}

write.xlsx(cbind(testdata[,1:4], testwiweek), "testnorm.xlsx")

wholesepanorm <- rbind(cbind(traindata[,1:4], trainwiweek), 
                       
                       cbind(validdata[,1:4], validwiweek), 
                       
                       cbind(testdata[,1:4], testwiweek))

write.xlsx(wholesepanorm, "wholesepanorm.xlsx")