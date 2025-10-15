
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rio)
 
RIData <- import("...\\Data\\fundat_satvi.xlsx") # fundat_ndvi, fundat_evi

RIData$ADC <- NULL
RIData$Color <- NULL

RIData <- RIData[RIData$Year == 2018, ]

RIData$Year <- NULL

any(is.na(RIData))

ridf <- RIData
 
s_ridf <- split(ridf, ridf$County)

# extract matrix 

ri_matrix <- do.call(rbind, s_ridf)

ri_matrix <- ri_matrix[,2:27]

# matrix-level normalization

range01_matrix <- function(mat){
  mat <- apply(mat, 2, as.numeric)  # ensure numeric
  (mat - min(mat)) / (max(mat) - min(mat))
}

ri_matrix_norm <- range01_matrix(ri_matrix)

### Define Similarity Measures

# DTW
library(dtw)
library(proxy)

dtw_dist <- proxy::dist(ri_matrix_norm, method=function(x, y) dtw(x, y)$distance)

# EU dis
euclid_dist <- dist(ri_matrix_norm)

# MAN dis
man_dist <- dist(ri_matrix_norm, method = "manhattan")

# MAH dis
library(MASS)

mahal_dist <- function(x) {
  S_inv <- ginv(cov(x))
  n <- nrow(x)
  d_vec <- numeric(n*(n-1)/2)
  k <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      diff <- x[i, ] - x[j, ]
      d_vec[k] <- sqrt(t(diff) %*% S_inv %*% diff)
      k <- k + 1
    }
  }
  
  attr(d_vec, "Size") <- n
  attr(d_vec, "Labels") <- rownames(x)
  attr(d_vec, "Diag") <- FALSE
  attr(d_vec, "Upper") <- FALSE
  class(d_vec) <- "dist"
  
  return(d_vec)
}

mahal_dist_matrix <- mahal_dist(ri_matrix_norm)

### Hierarchical Clustering

hc_euc <- hclust(euclid_dist)
hc_man <- hclust(man_dist)
hc_mahal <- hclust(mahal_dist_matrix)
hc_dtw <- hclust(as.dist(dtw_dist))


hc_list <- list(Euclid=hc_euc, Manhattan=hc_man, Mahalanobis=hc_mahal, DTW=hc_dtw)

HCratio_RI <- sapply(hc_list, function(hc){
  1 - mean(hc$height) / max(hc$height)
})

HCratio_RI <- round(HCratio_RI, 2) # keep two digits

HCratio_RI


