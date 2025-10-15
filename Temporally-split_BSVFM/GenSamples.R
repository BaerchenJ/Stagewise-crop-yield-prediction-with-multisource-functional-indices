
######################
## Useful functions ##
#######################

## Inverse matrix via SVD 
svd.inv<-function(Mat){
  svd.res=svd(Mat)
  V=svd.res$v
  inv.D=diag(1/(svd.res$d),nrow=nrow(Mat),ncol=ncol(Mat))
  inv.res=V%*%inv.D%*%t(V)
  return(inv.res)
}

## Generate diagonal matrix
diag.ftn=function(vec){
  vec=as.numeric(vec)
  diag.res= diag(vec^2,nrow=length(vec),ncol=length(vec))
  return(diag.res)
}

## Element-wise product 
iprod.ftn=function(vec1,vec2){
  iprod.res=vec1*vec2
  return(iprod.res)
}

##########################################################################################################
## Generate samples form posterior distributions derived in supplementary materials
##########################################################################################################
## 1. Spatially varying coefficients
##   - gen.alpha0() samples spatially varying intercept
##   - gen.beta() samples spatially varing coefficients for multivariate functional predictors (r=1,...,p)
##   - gen.alpha() samples spatially varying coefficients for scalar predictor
## 
## 2. Spatially correlated projection scores
##   - gen.xi() samples spatially correlated projection scores
##
## 3. Range parameters for exponential correlation function via M-H algorithms
##   - gen.phialpha0() samples range parameter assumed on beta_0 
##   - gen.phibeta() samples common range parameter assumed on beta_r (r=1,...,p) 
##   - gen.phialpha() samples range parameter assumed on alpha
##   - gen.phixi() samples common range parameter assumed on xi_r (r=1,...,p)
## 
## 4. Spatial model selection
##   - gen.v() samples latent variables v_r (r=1,...,p) via M-H algorihm
##   - gen.vr() samples common range parameter assumed on v_r (r=1,...,p) 
##
###################################################################################################################

###################################################################################################################
gen.alpha0 = function( xi.mat.star, beta.0, sigma.alpha0.0, tmp.inverse.alpha0){

  tmp.xiTxi0=matrix(0,nrow=n,ncol=n)
  for (jj in 1:length(year)){
    tmp.xiTxi0=tmp.xiTxi0 + diag.ftn(sqrt(pi.mat[,jj]))
  }
  
  tmp.xi0TY=matrix(0,nrow=n,ncol=1)
  for (jj in 1:length(year)){
    tmp.prod=as.matrix(rowSums(xi.mat.star[,c((p*(jj-1)+1): (p*jj))]*beta.0))
    tmp.xi0TY=tmp.xi0TY + (Y.list[[jj]]-tmp.prod)*pi.mat[,jj]
  }
  
  A0=svd.inv(tmp.xiTxi0/sigma.e2.0 + tmp.inverse.alpha0/sigma.alpha0.0 )
  a0=(tmp.xi0TY/sigma.e2.0 + tmp.inverse.alpha0%*%one.vec*mu.alpha0.0/sigma.alpha0.0)
  sam= matrix(mvrnorm(1,A0%*%a0,A0),ncol=1)
  
  list(sam=sam)
}
###################################################################################################################

###################################################################################################################
gen.beta = function( xi.mat.star, alpha0.0, sigma.beta.0, tmp.inverse.beta){
  sam=c()
  for (rr in 1:p){
    
    tmp.xiTxi=matrix(0,nrow=n,ncol=n)
    for (jj in 1:length(year)){
      tmp.xiTxi=tmp.xiTxi + diag.ftn(xi.mat.star[,p*(jj-1)+rr]*sqrt(pi.mat[,jj]))
    }
    
    tmp.var=c(1:p)[-rr]
    tmp.xiTY=matrix(0,nrow=n,ncol=1)
    for (jj in 1:length(year)){
      tmp.prod=alpha0.0 + rowSums(xi.mat.star[,p*(jj-1)+tmp.var]*beta.0[,tmp.var])
      tmp.xiTY=tmp.xiTY + iprod.ftn(xi.mat.star[,p*(jj-1)+rr],(Y.list[[jj]]-tmp.prod)*pi.mat[,jj])
    }
    
    A=svd.inv(tmp.xiTxi/sigma.e2.0+tmp.inverse.beta/sigma.beta.0[rr])
    a=(tmp.xiTY/sigma.e2.0 + tmp.inverse.beta%*%one.vec*mu.beta.0[rr]/sigma.beta.0[rr])
    sam=cbind(sam, matrix(mvrnorm(1,A%*%a,A),ncol=1))
    beta.0[,rr]=sam[,rr]
  }
  list(sam=sam)
}
################################################################################################################### 


###################################################################################################################
 gen.xi = function(phi.xi.0, beta.n.star, xi.mat, F.mat, alpha0.0, sigma.u.0, sigma.xi.0){
   
   tmp.xi.stack=matrix(NA,nrow(xi),p)
 
   for(rr in 1:p){
     
     tmp.var=c(1:p)[-rr]
     tmp.stack=c()
     for(gg in 1:length(year)){
       tmp.id=countyI[[gg]]
       tmp.indx=countyI.dat%in%tmp.id
       B1=svd.inv(diag.ftn(beta.n.star[,rr]*sqrt(pi.mat[,gg]))[tmp.indx,tmp.indx]/sigma.e2.0+In[tmp.indx,tmp.indx]/sigma.u.0
                  + tmp.inverse.xi[tmp.indx,tmp.indx]/sigma.xi.0[rr])
       
       tmp.b1= alpha0.0[tmp.indx]+rowSums(xi.mat[tmp.indx, c(p*(gg-1)+tmp.var)]*beta.n.star[tmp.indx,tmp.var])
                  
       tmp.id2=(regdat$Year==year[gg])
       tmp.info=t(fundat)[,tmp.id2]-F.mat[,-rr]%*%t(xi[tmp.id2,-rr])
       
       b1=(iprod.ftn(beta.n.star[tmp.indx,rr],(Y.list[[gg]][tmp.indx]- tmp.b1)*pi.mat[tmp.indx,gg])/sigma.e2.0 
           + (colSums(tmp.info*F.mat[,rr]))/sigma.u.0) 
       tmp=rep(0,n)
       tmp.sample=matrix(mvrnorm(1,B1%*%b1,B1),ncol=1)
       tmp.stack=c(tmp.stack, tmp.sample)
       tmp[tmp.indx]=tmp.sample
       xi.mat[,p*(gg-1)+rr]=tmp
     }
     tmp.xi.stack[,rr]=tmp.stack
   }
   list(sam=xi.mat, xi=tmp.xi.stack)
 }
###################################################################################################################

###################################################################################################################
 gen.phialpha0 = function(phi.alpha0.0, alpha0.0, mu.alpha0.0, sigma.alpha0.0, phi.mean.beta, phi.var.beta){
   phi.alpha0.p=rtruncnorm(1, a=0, b=Inf, mean = phi.alpha0.0, sd = var.beta) # var.beta controls acceptance rate
   cov.mat.alpha0.p<- exp(-dist.mat/phi.alpha0.p)
   
   log.f0.p=dmvnorm(as.numeric(alpha0.0),mean=one.vec*mu.alpha0.0,sigma=sigma.alpha0.0*cov.mat.alpha0.p,log=TRUE)
   log.f0.0=dmvnorm(as.numeric(alpha0.0),mean=one.vec*mu.alpha0.0,sigma=sigma.alpha0.0*cov.mat.alpha0.0,log=TRUE)
   
   log.ratio=(log.f0.p)-(log.f0.0)
   pr.p=dtruncnorm(phi.alpha0.p, a=0, b=Inf, mean = phi.mean.beta, sd = phi.var.beta)
   pr.0=dtruncnorm(phi.alpha0.0, a=0, b=Inf, mean = phi.mean.beta, sd = phi.var.beta)
   q.0.p=dtruncnorm(phi.alpha0.0, a=0, b=Inf, mean = phi.alpha0.p, sd = var.beta)
   q.p.0=dtruncnorm(phi.alpha0.p, a=0, b=Inf, mean = phi.alpha0.0, sd = var.beta)
   num=pr.p*q.0.p; denom=pr.0*q.p.0
   
   alpha.beta=min(1,exp(log.ratio)*num/denom)
   b0.ind = (runif(1,0,1)<alpha.beta)
   if (b0.ind==TRUE){
     sam = phi.alpha0.p
   } else{
     sam = phi.alpha0.0
   }
   a.rate = b0.ind
   list(sam=sam, a.rate=a.rate)
 }
################################################################################################################### 

###################################################################################################################
gen.phibeta = function(phi.beta.0, beta.0, mu.beta.0, sigma.beta.0, phi.mean.beta, phi.var.beta){
  phi.beta.p=rtruncnorm(1, a=0, b=Inf, mean = phi.beta.0, sd = var.beta) # var.beta controls acceptance rate
  cov.mat.beta.p<- exp(-dist.mat/phi.beta.p)
  log.f.p=0; log.f.0=0
  for(rr in 1:p){
    log.f.p=log.f.p + dmvnorm(as.numeric(beta.0[,rr]),
                              mean=one.vec*mu.beta.0[rr],sigma=sigma.beta.0[rr]*cov.mat.beta.p,log=TRUE)
    log.f.0=log.f.0 + dmvnorm(as.numeric(beta.0[,rr]),
                              mean=one.vec*mu.beta.0[rr],sigma=sigma.beta.0[rr]*cov.mat.beta.0,log=TRUE)
  }
  
  log.ratio=log.f.p - log.f.0
  pr.p=dtruncnorm(phi.beta.p, a=0, b=Inf, mean = phi.mean.beta, sd = phi.var.beta)
  pr.0=dtruncnorm(phi.beta.0, a=0, b=Inf, mean = phi.mean.beta, sd = phi.var.beta)
  q.0.p=dtruncnorm(phi.beta.0, a=0, b=Inf, mean = phi.beta.p, sd = var.beta)
  q.p.0=dtruncnorm(phi.beta.p, a=0, b=Inf, mean = phi.beta.0, sd = var.beta)
  num=pr.p*q.0.p; denom=pr.0*q.p.0
  
  alpha.beta=min(1,exp(log.ratio)*num/denom)
  b.ind=(runif(1,0,1)<alpha.beta)
  if (b.ind==TRUE){
    sam = phi.beta.p
  } else{
    sam = phi.beta.0
  }
  a.rate=b.ind
  
  list(sam=sam, a.rate=a.rate)
}
###################################################################################################################

#####################################################################################################################
gen.phixi = function(phi.xi.0, xi.mat, sigma.xi.0, phi.mean.xi, phi.var.xi){   
  phi.xi.p=rtruncnorm(1, a=0, b=Inf, mean = phi.xi.0, sd = var.xi)  # var.xi controls acceptance rate
  cov.mat.xi.p<- exp(-dist.mat/phi.xi.p)
  
  log.all.p=matrix(NA,K,p)  ; log.all.0=matrix(NA,K,p)
  for (rr in 1:p){
    log.f.p=c(); log.f.0=c()
    for (jj in 1:length(year)){
      tmp.id=countyI[[jj]]
      tmp.indx=countyI.dat%in%tmp.id
      log.f.p=c(log.f.p,dmvnorm(xi.mat[tmp.indx,p*(jj-1)+rr],
                                mean=one.vec[tmp.indx]*0,sigma=sigma.xi.0[rr]*cov.mat.xi.p[tmp.indx,tmp.indx],log=TRUE))
      log.f.0=c(log.f.0,dmvnorm(xi.mat[tmp.indx,p*(jj-1)+rr],
                                mean=one.vec[tmp.indx]*0,sigma=sigma.xi.0[rr]*cov.mat.xi.0[tmp.indx,tmp.indx],log=TRUE))
    }
    log.all.p[,rr]=log.f.p; log.all.0[,rr]=log.f.0
  }
  
  log.ratio=(mean(rowSums(log.all.p)) - mean(rowSums(log.all.0)))
  pr.p=dtruncnorm(phi.xi.p, a=0, b=Inf, mean = phi.mean.xi, sd = phi.var.xi)
  pr.0=dtruncnorm(phi.xi.0, a=0, b=Inf, mean = phi.mean.xi, sd = phi.var.xi)
  q.0.p=dtruncnorm(phi.xi.0, a=0, b=Inf, mean = phi.xi.p, sd = var.xi)
  q.p.0=dtruncnorm(phi.xi.p, a=0, b=Inf, mean = phi.xi.0, sd = var.xi)
  num=pr.p*q.0.p; denom=pr.0*q.p.0
  alpha.xi=min(1,exp(log.ratio)*num/denom)
  xi.ind=(runif(1,0,1)<alpha.xi)
  if (xi.ind==TRUE){
    sam = phi.xi.p
  } else{
    sam = phi.xi.0
  }
  a.rate = xi.ind
  
  list(sam=sam, a.rate=a.rate)
}
###################################################################################################################

###################################################################################################################
gen.v = function(sigma.v.0, phi.vr.0, v.0, prop.sigma.v, prop.Var.v){
  
  sam=c(); a.rate=c()
  for (rr in 1:p){
    
    Var.v.tmp=round(sigma.v.0[rr]*exp(-dist.mat/phi.vr.0),3)
    l.p=matrix(rmvnorm(1,mean=v.0[,rr],sigma=prop.sigma.v[rr]*prop.Var.v),ncol=1)
    beta.mh.star=matrix(beta.0[,rr]*(l.p>=0), ncol=1)
    
    log.p.p=c(); log.p.0=c(); ind.vec=c(1:p)[-rr]
    for (gg in 1:length(year)){
      
      tmp.id=countyI[[gg]]
      tmp.indx=countyI.dat%in%tmp.id
      
      xi.tmp=rowSums(xi.mat[tmp.indx, (gg-1)*p+ind.vec]*beta.n.star[tmp.indx, ind.vec])
      
      log.p.p=c(log.p.p,dmvnorm(t(Y.list[[gg]][tmp.indx]),
                                mean=(xi.mat[tmp.indx,rr+(gg-1)*p]*beta.mh.star[tmp.indx]+xi.tmp+alpha0.0[tmp.indx]),
                                sigma=sigma.e2.0*diag(1/pi.info[[gg]]),log=TRUE))
      log.p.0=c(log.p.0,dmvnorm(t(Y.list[[gg]][tmp.indx]),
                                mean=(xi.mat[tmp.indx,rr+(gg-1)*p]*beta.n.star[tmp.indx,rr]+xi.tmp+alpha0.0[tmp.indx]),
                                sigma=sigma.e2.0*diag(1/pi.info[[gg]]),log=TRUE))
    }
    
    log.v.p=dmvnorm(t(l.p),mean=mu.v.0[rr]*one.vec, sigma=Var.v.tmp, log=TRUE)
    log.v.0=dmvnorm(t(v.0[,rr]),mean=mu.v.0[rr]*one.vec, sigma=Var.v.tmp, log=TRUE)
    log.ratio=(sum(log.p.p)+log.v.p)-(sum(log.p.0)+log.v.0)
    
    alpha.v=min(1,exp(log.ratio))
    l.ind=(runif(1,0,1)<alpha.v)
    
    if (l.ind==TRUE){
      sam = cbind(sam, l.p)
    } else{
      sam = cbind(sam, v.0[,rr])
    }
    a.rate = c(a.rate,l.ind)
  }
  
  list(sam=sam, a.rate=a.rate)
}
###################################################################################################################

###################################################################################################################
gen.vr = function(phi.vr.0, var.v, phi.mean.vr, phi.var.vr){
  
  (phi.vr.p=rtruncnorm(1, a=0, b=Inf, mean = phi.vr.0, sd = var.v)) # var.v controls acceptance rate
  cov.mat.vr.p<- exp(-dist.mat/phi.vr.p)
  
  log.f.p=0; log.f.0=0
  for(rr in 1:p){
    log.f.p=log.f.p + dmvnorm(as.numeric(v.0[,rr]),mean=one.vec*mu.v.0[rr],sigma=sigma.v.0[rr]*cov.mat.vr.p,log=TRUE)
    log.f.0=log.f.0 + dmvnorm(as.numeric(v.0[,rr]),mean=one.vec*mu.v.0[rr],sigma=sigma.v.0[rr]*cov.mat.vr.0,log=TRUE)
  }
  
  log.ratio=(log.f.p)-(log.f.0)
  pr.p=dtruncnorm(phi.vr.p, a=0, b=Inf, mean = phi.mean.vr, sd = phi.var.vr)
  pr.0=dtruncnorm(phi.vr.0, a=0, b=Inf, mean = phi.mean.vr, sd = phi.var.vr)
  q.0.p=dtruncnorm(phi.vr.0, a=0, b=Inf, mean = phi.vr.p, sd = var.v)
  q.p.0=dtruncnorm(phi.vr.p, a=0, b=Inf, mean = phi.vr.0, sd = var.v)
  num=pr.p*q.0.p; denom=pr.0*q.p.0
  
  alpha.v=min(1,exp(log.ratio)*num/denom)
  
  vr.ind=(runif(1,0,1)<alpha.v)
  if (vr.ind==TRUE){
    sam = phi.vr.p
  } else{
    sam = phi.vr.0
  }
  a.rate = vr.ind
  
  list(sam=sam, a.rate=a.rate)
}
###################################################################################################################