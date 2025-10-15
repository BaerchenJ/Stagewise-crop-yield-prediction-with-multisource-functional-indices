
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Libraries 
library(fda)
library(splines)
library(mvtnorm)
library(truncnorm)
library(MCMCpack)
library(KFAS) 
library(rio)
library(raster)
library(Matrix)
library(BayesianTools)

###################### DATA IMPORT

## countyI

CountyI.info <- import("IllicountyI-info.xlsx")

## regdat 

regdat <- import("IlliregdatOOS.xlsx")

## dist.mat

countylatlon <- import("96points.csv")  # lat, lon of each county

counties <- data.frame(latitude=countylatlon[,3], longitude=countylatlon[,2])

library(geodist)

dist.mat <- as.matrix(geodist(counties)/1000)

## fundat

fundat_wi <- import("wholesepanorm_temporal.xlsx")  

# Temporally-split Train, Validation, Test sets were SEPARATELY scaled in advance, while combined in one data table.

fundat <- fundat_wi[,6:109]

### Source in Helper Functions
source("MFDLMBasisEstimation6.R") # basis function estimation (Kowal et al., 2017 "A Bayesian Multivariate Functional Dynamic Linear Model")
source("GenSamples.R")           # functions to generate posterior samples

### Years in Data
year = 1980:2022

##################################################################################
## Data Preparation                                                             ##
## 1. Identify counties with information from at least one year                 ##
## 2. Remove year effects and calculate Y_k*                                    ##
## 3. Reformulate the data with list() format by year                           ##
## 4. Replace missing values with 0                                             ##
##################################################################################

countyI.dat = sort(unique(regdat$CountyI)) 

Y.list=list();countyI=list(); 
pi.info=list(); pi.mat=c(); 
index.missing=c(); 

for (i in 1:length(year)){
  
  ## observed counties at each year
  tmp.id=sort(unique(regdat[regdat$Year==year[i], "CountyI"]))
  countyI[[i]] = tmp.id
  
  ## Year effect removed and saved in Y.list
  tmp.indx=which(regdat$Year==year[i])
  tmp.Y=rep(0,length=length(countyI.dat))
  tmp.Y[countyI.dat %in% tmp.id]=regdat$Ystar[tmp.indx]
  Y.list[[i]]=tmp.Y
  
  ## Identify which counties are missing from each year
  zero.vec=rep(FALSE,length(countyI.dat));
  zero.vec[countyI.dat %in% tmp.id]=TRUE
  index.missing=c(index.missing, zero.vec)
  
  ## Harvest land size of county information
  tmp.pi=rep(0,length=length(countyI.dat))
  tmp.pi[countyI.dat %in% tmp.id]=regdat$Area[tmp.indx]
  pi.info[[i]]=regdat$Area[tmp.indx]
  pi.mat=cbind(pi.mat,tmp.pi)
}

pi.B.diag=diag(as.vector(pi.mat))

##############################################
## Useful Numbers and Vectors for Reference ##
###############################################

n = length(countyI.dat) # number of counties in data
K = length(year) # number of repetitions
t = 1:(26*4)  # a sequence of grid points for functional predictors (if multivariate, augmented grid points)
one.vec = matrix(1,nrow=n,ncol=1) # (n times 1) vector of 1 
In = diag(1,nrow=n,ncol=n) # (n times n) identity matrix

########################################
## The Number of Basis Functions for  ## 
## Expansion of Functional Predictors ##
## p in Equation (2) in manuscript    ##
########################################

p = 5  # Based on DIC in our study

###############################################################
## Starting Values:                                          ##
##                                                           ##     
## Convergence is not sensitive to start values              ##  
## except for latent model selection process v_r, r=1,...,p  ##
###############################################################

## (i) initial values for parameters related to multivariate FPCA: 
##      xi, d, lambda, and sigma_u based on initParams()

set.seed(1917)
sessionInfo()

inits = initParams(Y=as.matrix(fundat),tau=t, K=p)
xi = inits$Beta0
d = inits$d0
sigma.u.0 = inits$Et0
lambda = inits$lambda0
splineInfo = inits$splineInfo
sigma.xi.0 = c(p:1) 
F.mat.0=splineInfo$Phi%*%d


## (ii) initial values for intercept, alpha, and beta_r (r=1,...,p) and their corresponding parameters
alpha0.0 = 0; beta.0 = matrix(0,nrow=n, ncol=p)
sigma.alpha0.0 = 1; sigma.beta.0=rep(1, p)
mu.alpha0.0 = 0; mu.beta.0 = rep(0, p); 

# (iii) model error
sigma.e2.0 = 1; 

# (iv) range parameters
phi.alpha0.0 = 200; phi.beta.0 = 200; 
phi.xi.0 = 200; phi.vr.0 = 200

# (v) variable selection parameters
mu.v.0 = rep(0,p); sigma.v.0 = rep(10, p)
initial.vec = rmvnorm(1,sigma=2*exp(-dist.mat/200))  
v.0 = matrix(initial.vec, n, p) # initial values for v_r, r=1,...,p
gamma.0 = (v.0>=0)

####################################################
## Parameters for Proposal Distributions to      ##
## implement M-H algorithm for ranges parameters ## 
## and v_r, r=1,...,p                            ##
##                                               ##
## Set appropriate variances that achieve       ##
## acceptance rate 25-50%                        ##
###################################################
# For range parameters phi
#var.beta = 50; var.xi = 300; var.v =20;
var.beta = 150; var.xi = 150; var.v =20;
# For v_r
prop.Var.v = In; 
prop.sigma.v = rep(1/n, p)


##########################################
## Hyper Priors                         ##
##                                      ##
## Specified in supplementary material  ##
##########################################
phi.mean.beta = 350; phi.var.beta = 100; # phi.mean.beta = 300;
phi.mean.xi = 350; phi.var.xi = 100; # phi.mean.xi = 300; 
phi.mean.vr = 350; phi.var.vr = 100; # phi.mean.vr = 300;
qt = 2.8; rt = 1/0.28; q.sigma = 2.8; r.sigma = 1/0.28; 
var.mulapha = var.mubeta = 50
alpha.zero = 0; phi = 1   


###################
## MCMC Settings ##
###################
n.itr = 15000  # number of iterations
n.burnin = 5000  # number of burn-in samples

#######################################
## Matrices and Arrays to Hold Draws ##
#######################################
alpha0.n = c()
beta.n = array(NA, dim = c(n, (p), n.itr)) 
alpha.n = c();
xi.mat.n = array(NA, dim = c(n, p*K, n.itr )) 
gamma.n = array(NA, dim = c(n, p, n.itr));
v.n = array(NA, dim = c(n, p, n.itr))
sigma.e2 = c()

mu.alpha0.n=c();mu.beta.n=c(); mu.alpha.n=c(); sigma.alpha0.n=c();sigma.beta.n=c();sigma.alpha.n=c();sigma.xi.n=c()
phi.alpha0.n=c();phi.beta.n=c();phi.alpha.n=c();phi.xi.n=c();mu.v.n=c();sigma.v.n=c();phi.vr.n=c()
sigma.u.n=c()

# For checking acceptance rates of M-H algorithms
a.rate.alpha0=c();
a.rate.beta=c(); a.rate.xi=c();   
a.rate.alpha=c();
a.rate.vr<-c()
a.rate.v=matrix(NA, n.itr, p ); 

#####################
## Begin MCMC Loop ##
#####################

for(r in 1:n.itr){
  
  ### define exponential correlation matrices for intercept, alpha, beta, xi, v based on 
  ### range parameters phi at the previous iteration
  cov.mat.alpha0.0<-exp(-dist.mat/phi.alpha0.0); tmp.inverse.alpha0=svd.inv(cov.mat.alpha0.0)  
  cov.mat.beta.0<-exp(-dist.mat/phi.beta.0); tmp.inverse.beta=svd.inv(cov.mat.beta.0) 
  cov.mat.xi.0<-exp(-dist.mat/phi.xi.0);tmp.inverse.xi=svd.inv(cov.mat.xi.0);
  cov.mat.vr.0<-exp(-dist.mat/phi.vr.0)

  #####################
  ### multivariate FPCA
  ######################
  
  ### update smoothing parameter lambda and spline coefficients d for FPC estimation
  sampleF=mfdlmF(Beta=xi, Y=as.matrix(fundat), Et=sigma.u.0, tau=t, d=d, splineInfo=splineInfo, lambda=lambda)
  d=sampleF$d; lambda = sampleF$lambda; xi = sampleF$Beta
  F.mat=splineInfo$Phi%*%d
  
  xi.mat=c();
  for (i in 1:length(year)){
    tmp.indx=which(regdat$Year==year[i])
    tmp.id=countyI[[i]]
    xi.tmp=matrix(0,nrow=length(countyI.dat),ncol=p)
    xi.tmp[countyI.dat %in% tmp.id,]=as.matrix(xi[tmp.indx,c(1:p)])
    xi.mat=cbind(xi.mat,xi.tmp)
  }
  
  gamma.mat.xi=matrix(rep(gamma.0,K),nrow=n);
  xi.mat.star=xi.mat*gamma.mat.xi
  
  
  ###################################################################
  ### spatially varying intercept and coefficients, beta_r
  ###################################################################
  
  ### Update intercept
  samplealpha0=gen.alpha0(xi.mat.star, beta.0, sigma.alpha0.0, tmp.inverse.alpha0)
  alpha0.n=cbind(alpha0.n, samplealpha0$sam)
  alpha0.0 = samplealpha0$sam
  
  ### update beta_r, r=1,...,p
  samplebeta=gen.beta(xi.mat.star, alpha0.0, sigma.beta.0, tmp.inverse.beta)
  beta.n[,,r]=samplebeta$sam
  beta.0=samplebeta$sam
  
  #################################
  ### mean and variance parameters
  ##################################
  
  ### update mean parameter for intercept
  c0=t(one.vec)%*%tmp.inverse.alpha0%*%alpha0.0/sigma.alpha0.0
  C0=(sum(tmp.inverse.alpha0)/sigma.alpha0.0 +1/var.mubeta)^(-1)    
  mu.alpha0.0 = rnorm(1,C0*c0,sqrt(C0))
  mu.alpha0.n = c(mu.alpha0.n, mu.alpha0.0)
  
  ### update mean parameters for beta_r
  mu.beta=c()
  for (rr in 1:p){
    c1=t(one.vec)%*%tmp.inverse.beta%*%beta.0[,rr]/sigma.beta.0[rr]
    C1=(sum(tmp.inverse.beta)/sigma.beta.0[rr] +1/var.mubeta)^(-1)    
    mu.beta= c(mu.beta, rnorm(1,C1*c1,sqrt(C1)))
  }
  mu.beta.0=mu.beta
  mu.beta.n = cbind(mu.beta.n, mu.beta.0)
  
  
  ### update variance parameter sigma_{alpha_0} for intercept 
  sigma.alpha0.0=rinvgamma(n=1,(n/2+q.sigma),as.numeric(1/2*t(alpha0.0-one.vec*mu.alpha0.0)%*% tmp.inverse.alpha0 %*% (alpha0.0-one.vec*mu.alpha0.0)+r.sigma))
  sigma.alpha0.n = c(sigma.alpha0.n, sigma.alpha0.0)
  
  ### update variance parameter sigma_{beta_r}, r=1,...,p, for beta_r
  sigma.beta=c()
  for (rr in 1:p){
    sigma.beta=c(sigma.beta, rinvgamma(n=1,(n/2+q.sigma),as.numeric(1/2*t(beta.0[,rr]-one.vec*mu.beta.0[rr])%*% tmp.inverse.beta %*% (beta.0[,rr]-one.vec*mu.beta.0[rr])+r.sigma)))
  }
  sigma.beta.0=sigma.beta
  sigma.beta.n=cbind(sigma.beta.n, sigma.beta.0)
  

  ################
  ### FPC scores 
  ################
  
  ### update xi
  beta.n.star=beta.0*gamma.0
  samplexi = gen.xi(phi.xi.0, beta.n.star, xi.mat, F.mat, alpha0.0, sigma.u.0, sigma.xi.0)
  xi.mat.n[, ,r]=samplexi$sam    
  xi.mat = samplexi$sam
  xi=samplexi$xi
  
  ### update sigma_{xi_r}, r=1,...,p
  tmp.prod=t(xi.mat)%*%tmp.inverse.xi%*%(xi.mat)
  sigma.xi=c();
  for (rr in 1:p){
    sum.prod1=0
    for(gg in 1:length(year)){
      sum.prod1=sum.prod1+tmp.prod[p*(gg-1)+rr,p*(gg-1)+rr]
    }
    sigma.xi=c(sigma.xi, rinvgamma(n=1,(nrow(regdat)/2+q.sigma),1/2*sum.prod1+r.sigma))
  }
  sigma.xi.0 = sigma.xi
  sigma.xi.n = cbind(sigma.xi.n, sigma.xi.0)
  
  ############################################################################################
  ###  range parameters in exponential correlation functions for intercept, beta_r, xi_r
  ###  a.rate.alpha_0, a.rate.beta, a.rate.xi save acceptance rates
  ############################################################################################
  
  ### update range parameter for intercept
  samplephialpha0=gen.phialpha0(phi.alpha0.0, alpha0.0, mu.alpha0.0, sigma.alpha0.0, phi.mean.beta, phi.var.beta)
  phi.alpha0.0=samplephialpha0$sam
  a.rate.alpha0 = c(a.rate.alpha0, samplephialpha0$a.rate) # when checking acceptance rate
  phi.alpha0.n = c(phi.alpha0.n, phi.alpha0.0)
  
  ### update range parameter for beta_r
  samplephibeta = gen.phibeta(phi.beta.0, beta.0, mu.beta.0, sigma.beta.0, phi.mean.beta, phi.var.beta)
  phi.beta.0 = samplephibeta$sam
  a.rate.beta = c(a.rate.beta, samplephibeta$a.rate)  # when checking acceptance rate
  phi.beta.n = c(phi.beta.n, phi.beta.0)
  
  ### update range parameter for xi
  samplephixi = gen.phixi(phi.xi.0, xi.mat, sigma.xi.0, phi.mean.xi, phi.var.xi)
  phi.xi.0 = samplephixi$sam
  a.rate.xi = c(a.rate.xi, samplephixi$a.rate) # when checking acceptance rate
  phi.xi.n = c(phi.xi.n, phi.xi.0)
  
  ##############################################################################################
  ### variable selection related parameters; mu_{v_r}, sigma_{v_r}, phi_v, v_r, r=1,...,p
  ##############################################################################################
  
  ### update mu_{v_r}
  mu.v=c()
  for (rr in 1:p){
    V.tmp=round(sigma.v.0[rr]*exp(-dist.mat/phi.vr.0),3)
    C=((1/phi)+sum(svd.inv(V.tmp)))^(-1); c=(t(one.vec)%*%svd.inv(V.tmp)%*%v.0[,rr])+alpha.zero/phi; 
    mu.v=c(mu.v,rnorm(1,mean=C*c,sd=sqrt(C)))
  }
  mu.v.0=mu.v
  mu.v.n = c(mu.v.n, mu.v.0)
  
  ### update sigma_{v_r}
  inv.Vl=svd.inv(round(exp(-dist.mat/phi.vr.0),3))
  sigma.v=c()
  for(rr in 1:p){
    sigma.v= c(sigma.v, rinvgamma(n=1,(length(countyI.dat)/2+q.sigma),1/2*(t(v.0[,rr]-mu.v.0[rr]*one.vec)%*%inv.Vl%*%(v.0[,rr]-mu.v.0[rr]*one.vec) )+r.sigma))
  }
  sigma.v.0=sigma.v
  sigma.v.n=cbind(sigma.v.n, sigma.v.0)
  
  ### update v_r
  samplel=gen.v(sigma.v.0, phi.vr.0, v.0, prop.sigma.v, prop.Var.v)
  v.0 = samplel$sam
  gamma.n[ , ,r]=(v.0 >=0)
  gamma.0=(v.0>0)
  a.rate.v[r,] = samplel$a.rate # when checking acceptance rate
  
  ### update phi_v
  samplephilr=gen.vr(phi.vr.0, var.v, phi.mean.vr, phi.var.vr)
  phi.vr.0=samplephilr$sam
  a.rate.vr=c(a.rate.vr,samplephilr$a.rate) # when checking acceptance rate
  phi.vr.n = c(phi.vr.n, phi.vr.0)
  
  ####################################################
  ### functional measurement errors and model error
  ####################################################
  
  ### update the measurement error sigma_{ul}
  tmp.prod=t(as.matrix(t(fundat))-F.mat%*%t(xi))%*%(as.matrix(t(fundat))-F.mat%*%t(xi))
  sigma.u.0 = rinvgamma(n=1,((nrow(regdat))*length(t)/2+q.sigma),1/2*sum(diag(tmp.prod))+r.sigma)
  sigma.u.n = c(sigma.u.n, sigma.u.0)
  
  ### update model error sigma_e
  long.Y=c(unlist(Y.list))
  long.xi=as.numeric(xi.mat,byrow=FALSE);
  long.beta=rep(as.numeric(beta.n.star, byrow=FALSE), K)
  long.alpha0=rep(alpha0.0,length(year))
  long.alpha0[index.missing==0]=0
  
  tmp.mat=matrix(iprod.ftn(long.xi,long.beta),nrow=n, byrow=FALSE)
  tmp.predict=c(rowSums(tmp.mat[,1:p]), rowSums(tmp.mat[,(p+1):(2*p)]), rowSums(tmp.mat[,(2*p+1):(3*p)]), rowSums(tmp.mat[,(3*p+1):(4*p)]),rowSums(tmp.mat[,(4*p+1):(5*p)]),
                rowSums(tmp.mat[,(5*p+1):(6*p)]), rowSums(tmp.mat[,(6*p+1):(7*p)]), rowSums(tmp.mat[,(7*p+1):(8*p)]),rowSums(tmp.mat[,(8*p+1):(9*p)]),
                rowSums(tmp.mat[,(9*p+1):(10*p)]), rowSums(tmp.mat[,(10*p+1):(11*p)]), rowSums(tmp.mat[,(11*p+1):(12*p)]),rowSums(tmp.mat[,(12*p+1):(13*p)]),
                rowSums(tmp.mat[,(13*p+1):(14*p)]),rowSums(tmp.mat[,(14*p+1):(15*p)]),rowSums(tmp.mat[,(15*p+1):(16*p)]),rowSums(tmp.mat[,(16*p+1):(17*p)]),
                rowSums(tmp.mat[,(17*p+1):(18*p)]),rowSums(tmp.mat[,(18*p+1):(19*p)]),rowSums(tmp.mat[,(19*p+1):(20*p)]),rowSums(tmp.mat[,(20*p+1):(21*p)]),
                rowSums(tmp.mat[,(21*p+1):(22*p)]),rowSums(tmp.mat[,(22*p+1):(23*p)]),rowSums(tmp.mat[,(23*p+1):(24*p)]),rowSums(tmp.mat[,(24*p+1):(25*p)]),
                rowSums(tmp.mat[,(25*p+1):(26*p)]),rowSums(tmp.mat[,(26*p+1):(27*p)]),rowSums(tmp.mat[,(27*p+1):(28*p)]),rowSums(tmp.mat[,(28*p+1):(29*p)]),
                rowSums(tmp.mat[,(29*p+1):(30*p)]),rowSums(tmp.mat[,(30*p+1):(31*p)]),rowSums(tmp.mat[,(31*p+1):(32*p)]),rowSums(tmp.mat[,(32*p+1):(33*p)]),
                rowSums(tmp.mat[,(33*p+1):(34*p)]),rowSums(tmp.mat[,(34*p+1):(35*p)]),rowSums(tmp.mat[,(35*p+1):(36*p)]),rowSums(tmp.mat[,(36*p+1):(37*p)]),
                rowSums(tmp.mat[,(37*p+1):(38*p)]),rowSums(tmp.mat[,(38*p+1):(39*p)]),rowSums(tmp.mat[,(39*p+1):(40*p)]),rowSums(tmp.mat[,(40*p+1):(41*p)]),
                rowSums(tmp.mat[,(41*p+1):(42*p)]),rowSums(tmp.mat[,(42*p+1):(43*p)]))
  tmp.res=t(long.Y-long.alpha0-tmp.predict)%*%pi.B.diag%*%(long.Y-long.alpha0-tmp.predict)
  sigma.e2.0 = rinvgamma(n=1,(qt+nrow(regdat)/2),as.numeric(1/2*tmp.res)+rt)
  sigma.e2=c(sigma.e2, sigma.e2.0)
  
  if (r%%10==0) print(round(mean(sigma.e2[c((r-9):r)])/1e+05,5))  # check iteration status and the estimated model error
  
}


## save fitted values for R^2 and RMSE 

Fit = long.alpha0 + tmp.predict   

y1hatdf <- cbind(fundat_wi[,1:5], Fit)

library(openxlsx)

write.xlsx(data.frame(y1hatdf), "BSVFMOOS_temporal.xlsx")


