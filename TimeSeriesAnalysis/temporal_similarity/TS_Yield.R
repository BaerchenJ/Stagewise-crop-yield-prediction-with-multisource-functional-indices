
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


################# Yield data #####################

library(rio)

YieldData <- import("...\\Data\\Yield.xlsx")

##################################################
# Split the data frame by the 'County' column

YieldData$Yield <- NULL  # remove the raw yield: We use Ystar, yield deviations.
Ystar_df <- YieldData

# normalize Ystar
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
Ystar_df[,3] = range01(as.numeric(Ystar_df[,3]))

s_Ystar_df <- split(Ystar_df, Ystar_df$County)

# extract the matrix
Ystar_num <- lapply(s_Ystar_df, function(x) as.numeric(as.character(x$Ystar)))

Ystar_matrix <- do.call(rbind, Ystar_num)


### Define Similarity Measures

# DTW
library(dtw)
library(proxy)

dtw_dist <- proxy::dist(Ystar_matrix, method=function(x, y) dtw(x, y)$distance)

# EU dis
euclid_dist <- dist(Ystar_matrix)

# MAN dis
man_dist <- dist(Ystar_matrix, method = "manhattan")

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

mahal_dist_matrix <- mahal_dist(Ystar_matrix)


### Hierarchical Clustering

hc_euc <- hclust(euclid_dist)
hc_man <- hclust(man_dist)
hc_mahal <- hclust(mahal_dist_matrix)
hc_dtw <- hclust(as.dist(dtw_dist))

hc_list <- list(Euclid=hc_euc, Manhattan=hc_man, Mahalanobis=hc_mahal, DTW=hc_dtw)

HCratio_Ystar <- sapply(hc_list, function(hc){
  1 - mean(hc$height) / max(hc$height)
})

HCratio_Ystar <- round(HCratio_Ystar, 2) # keep two digits

HCratio_Ystar
