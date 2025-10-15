
########################################################################################################
# Funtions to estimate multivariate FPC
#
# Adjusted code from a part of the published code in 
# Kowal et al. (2017) "A Bayesian Multivariate Functional Dynamic LinearModel", JASA, 112:518, 733-744
#######################################################################################################


#####################################################################################################
# mfdlmF() samples FPC and the smoothing parameters lambda
#####################################################################################################

mfdlmF = function(Beta, Y, Et, tau, d, splineInfo, lambda, orderLambdas = TRUE){
  outcomeInds = c(1, which(tau[-1] < tau[-length(tau)])+1, length(tau)+1)
  allTaus = sort(unique(tau))
  
  Phi = splineInfo$Phi; Jkl = splineInfo$Jkl
  
  # Define these locally	
  C = length(outcomeInds) - 1 
  K = ncol(Beta)/C 
  
  # For outcome-specific Beta indices, subset with bc.inds = betaInds[c]:(betaInds[c+1]-1)
  betaInds = seq(1, C*(K+1), by=K)
  
  for(k in sample(1:K)){
    # Unconstrained posterior is N(solve(Bksum)%*%bksum, solve(Bksum))
    # Adjust bksum for orthogonality constraints
    # Then normalize the posterior samples
    bksum = 0 # for summing over c = 1,...,C
    Bksum = 0 # same
    
    for(c in 1:C){
      # Indices of the c-specific tau's:
      tauCinds = match(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], allTaus) 
      
      # Subsets: Y_t^{(c)}(\tau_1), ..., Y_t^{(c)}(\tau_m)
      yc.inds = outcomeInds[c]:(outcomeInds[c+1]-1) 	
      
      # Subsets: \beta_{1,t}^{(c)}, ..., \beta_{K,t}^{(c)}
      bc.inds = betaInds[c]:(betaInds[c+1]-1)		
      
      # The NAs in Y, combined w/ na.rm=TRUE, will only sum over non-missing data for bksum
      bksum = bksum + crossprod(Phi[tauCinds,], colSums((1/Et[c]*(Y[,yc.inds] - tcrossprod(Beta[,bc.inds[-k]], Phi[tauCinds,]%*%d[,-k])))*Beta[,bc.inds[k]], na.rm=TRUE))
      
      # Obtain outcome-specific complete cases:
      cc = complete.cases(Y[,yc.inds]) 
      
      # Sum over complete cases, but only look at outcome-specific tau's 
      Bksum  = Bksum + crossprod(Phi[tauCinds,])*sum(Beta[cc, bc.inds[k]]^2)/Et[c]
      
      # Only loop over incomplete cases:
      for(i in which(!cc)) Bksum  = Bksum + Beta[i,bc.inds[k]]^2/Et[c]*crossprod(Phi[tauCinds,][!is.na(Y[i,yc.inds]),])
    } 
    
    # Sample \lambda_k (precisions) using uniform priors on standard deviations, sd_k = lambda_k^-1/2, for identifiability:
    # 0 < sd_1 < sd_2 < ... < sd_K < 10^4 
    # or equivalently, Inf > \lambda_1 > \lambda_2 > ... > \lambda_K > 10^-8
    
    # Shape and rate parameters needed to enforce ordering:
 
    shape0 = (ncol(Phi) + 1)/2; #rate0 = crossprod(d[-(1:2),k])/2 			# for uniform prior on sd_k
    tmp.rate = crossprod(d[-(1:2),k])/2
    if(tmp.rate>0){
      rate0=tmp.rate
    }else{
      tmp.cp=diag(crossprod(d[-(1:2),]))
      rate0 = mean(tmp.cp[tmp.cp!=0]/2)
    }
    # Lower and upper bounds, w/ ordering constraints (if specified):
    lam.l = 10^-8; lam.u = Inf; if(orderLambdas){if(k != K) lam.l = lambda[k+1]; if(k != 1) lam.u = lambda[k-1]}
    u.min = pgamma(lam.l, shape=shape0, rate=rate0)
    u.max = pgamma(lam.u, shape=shape0, rate=rate0)
    cond = (u.min <= u.max)
    if (is.na(cond)==TRUE){
      cond = FALSE
    }
    
    if (cond == TRUE){
      u = runif(1, min = u.min, max = u.max)
    } else if (cond == FALSE){
      u = NA
    }
    
    if(u==1 | is.na(u)==TRUE){
      if(k==1){
        lambda[k] = 6*10^7
      }else{
        lambda[k] = lambda[k-1] - 10000
      }
      
    }  else{
      lambda[k] = qgamma(u, shape=shape0, rate=rate0)
    }     
    
    # Prior precision matrix implied by these \lambda_k's:
    priorPrec = diag(c(rep(10^-8, 2), rep(lambda[k], (ncol(Phi) - 2))))
  
    tmp=svd(Bksum + priorPrec)$d
    if(sum((tmp<1e-10))>0){
      tmp.pd=nearPD(Bksum + priorPrec)$mat
      cholFac = chol(tmp.pd)
    } else{
      cholFac = chol(Bksum + priorPrec)
    }

    # Joint orthogonality constraints:
    Lcon = Jkl%*%d[,-k]  	
    
    # Sample the unconstrained vector:
    dk = backsolve(cholFac,forwardsolve(t(cholFac),bksum) + rnorm(length(bksum)))		
    
    # Compute the shift:
    BLcon = backsolve(cholFac,forwardsolve(t(cholFac),Lcon))
    
    # Incorporate the constraint:
    #chol(crossprod(Lcon, BLcon))
    tmp=svd(crossprod(Lcon, BLcon))$d
    if(sum((tmp<1e-10))>0){
      tmp.pd=nearPD(crossprod(Lcon, BLcon))$mat
      dk = dk - BLcon%*%chol2inv(chol(tmp.pd))%*%crossprod(Lcon,dk)
    } else{
      dk = dk - BLcon%*%chol2inv(chol(crossprod(Lcon, BLcon)))%*%crossprod(Lcon,dk)
    }
    
    # Compute the norm for normalization
    Dnorm = sqrt(as.numeric(crossprod(dk,Jkl)%*%dk))
     d[,k] = as.vector(dk/Dnorm)

    # Also rescale Beta 
    Beta[, seq(from = k, to = K*C, by=K)] = Beta[, seq(from = k, to = K*C, by=K)]*Dnorm
  }
  list(d=d, lambda=lambda, Beta=Beta) 
}
#####################################################################################################
#####################################################################################################

#####################################################################################################
# mfdlm() is a helper function for mfdlmF()
#####################################################################################################
mfdlm = function(Y, tau, Beta, Et, Gt, Wt, Model, d, splineInfo, lambda){
  # Sample d_k and lambda_k, k = 1,...,K, common to all outcomes:
  Fall = mfdlmF(Beta, Y, Et, tau, d, splineInfo, lambda)
  d = Fall$d # for updating Beta
  
  # Sample \beta_t, t = 1,...,T, which contains all C*K factors
  Beta = mfdlmBeta(Y, Et, Gt, Wt, Model, tau, d, splineInfo)
  
  # return Beta, d, lambda
  list(Beta=Beta, d=d, lambda = Fall$lambda)
}
#####################################################################################################

#####################################################################################################
# mfdlmBeta() is a helper function for mfdlm() 
#####################################################################################################
mfdlmBeta = function(Y, Et, Gt, Wt, Model, tau, d, splineInfo){
  # All observation points across time and outcome:
  allTaus = sort(unique(tau))
  
  # For outcome-specific subset, use yc.inds = outcomeInds[c]:(outcomeInds[c+1]-1)
  outcomeInds = c(1, which(tau[-1] < tau[-length(tau)])+1, length(tau)+1)
  
  # Define these locally	
  C = length(outcomeInds) - 1
  K = ncol(d)
  
  # F evaluates the FLCs at the outcome-specific observation points
  F = array(0, c(length(tau), nrow(Gt))); inds = 1:K
  
  # Cycle through the outcomes c = 1,...,C:
  for(c in 1:C){
    # Indices of allTaus that correspond to c-specific tau's
    tauCinds = match(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], allTaus) 
    
    # Subsets: Y_t^{(c)}(\tau_1), ..., Y_t^{(c)}(\tau_m)
    yc.inds = outcomeInds[c]:(outcomeInds[c+1]-1) 	
    
    F[yc.inds, inds] = splineInfo$Phi[tauCinds,]%*%d
    
    # Update the column indexing for appropriate block structure
    if(K==1){inds = inds + 1} else {inds[1] = inds[length(inds)] + 1; inds[length(inds)] = inds[length(inds)] + K; inds = inds[1]:inds[length(inds)]}
  }
  
  # Repeat Et[c] for each outcome-specific number of tau's
  Model$H[,,1] = diag(rep(Et, diff(outcomeInds)))
  
  Model$T = Gt 	# Gt is an array of correct dimensions
  Model$Q = Wt	# same
  
  # Check for errors
  if(!is.SSModel(Model)) stop("Error: Model has incorrect dimensions")
  
  # Run the sampler
  simulateSSM(Model, "states", nsim = 1, antithetics=FALSE, filtered=FALSE)[,,1]
  
  # Could instead use a "streamlined" version of the KFAS function, with redundant computations removed (Note: no checks for errors!)
  # Must first define modelInfo = getModelInfo(Model), which only needs to be computed (and stored) once, before running MCMC
  #simulateSSM2(Model, modelInfo)[,,1]
}
#####################################################################################################
#####################################################################################################



#####################################################################################################
# initParams() initializes the main parameters in the model
#####################################################################################################
initParams = function(Y, tau, K=NULL, tolCPV = 0.99,  useAllTaus = FALSE){
  allTaus = sort(unique(tau))
  
  # For outcome-specific subset, use outcomeInds[c]:(outcomeInds[c+1]-1)
  outcomeInds = c(1, which(tau[-1] < tau[-length(tau)])+1, length(tau)+1)
  
  # also can infer the number of outcomes
  C = length(outcomeInds) - 1	
  
  # and the number of times:
  T = nrow(Y)
  
  # relevant spline info:
  splineInfo = getSplineInfo(tau)
  
  # Specify the tau values to interpolate for the Beta and F initialization (SVD)
  if(useAllTaus){
    useTaus = allTaus
  } else useTaus = c(splineInfo$a, splineInfo$intKnots, splineInfo$b)
  
  # For initialization:
  Y0 = array(0, c(C*T, length(useTaus)))
  
  for(c in 1:C){
    # just look at outcome c:
    Y0c = Y[, outcomeInds[c]:(outcomeInds[c+1]-1)]
    
    # if Y_t is partially missing, smooth across tau (for each such t):
    notCC = which(rowSums(!is.na(Y0c)) != 0 )
    Y0c[notCC,] = t(apply(Y0c[notCC,], 1, function(x) splinefun(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], x, method='natural')(tau[outcomeInds[c]:(outcomeInds[c+1]-1)])))
    
    # if Y_t is completely missing, smooth across t for each observation point
    Y0c = apply(Y0c, 2, function(x){splinefun(1:T, x, method='natural')(1:T)})
    
    # Y0 is a stacked data matrix, with C*T rows and length(allTaus) columns
    Y0[(1:T) + (c-1)*T, ] = t(apply(Y0c, 1, function(x) splinefun(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], x, method='natural')(useTaus)))
  }
  
  # Compute SVD of the (completed) data matrix:
  singVal = svd(Y0)
  
  # Cumulative sums of the s^2 proportions
  # More reasonable when Y has been centered
  cpv = cumsum(singVal$d^2/sum(singVal$d^2))
  print(t(matrix(cpv,dimnames=list(paste('k =',1:length(singVal$d))))))	
  
  # If K is unspecified, select based on cpv 
  if(is.null(K)) K = max(2, which(cpv >= tolCPV)[1])
  
  # For outcome-specific Beta indices, subset with betaInds[c]:(betaInds[c+1]-1)
  betaInds = seq(1, C*(K+1), by=K)
  F0 = vector("list", C); Beta0 = array(0,c(T, C*K)); Et0 = numeric(C); 
  
  for(c in 1:C){
    # If we use all tau's, then just subset for each outcome; otherwise, smooth the SVD estimates and interpolate the outcome-specific tau's
    if(useAllTaus){
      F0[[c]] = as.matrix(singVal$v[match(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], allTaus),1:K])
    } else F0[[c]] = apply(as.matrix(singVal$v[,1:K]), 2, function(x){splinefun(useTaus, x, method='natural')(tau[outcomeInds[c]:(outcomeInds[c+1]-1)])})
    
    Beta0[,betaInds[c]:(betaInds[c+1]-1)] = (singVal$u%*%diag(singVal$d))[(1:T) + (c-1)*T , 1:K]
    
    # Estimate the observation-level error variance:
    Et0[c] = 1/sum(!is.na(Y[, outcomeInds[c]:(outcomeInds[c+1]-1)])) * sum((Y[, outcomeInds[c]:(outcomeInds[c+1]-1)] - tcrossprod(Beta0[,betaInds[c]:(betaInds[c+1]-1)], F0[[c]]))^2, na.rm=TRUE)
  }
  # Initialize the common basis coefficients
  Fall = mfdlmFinit(Beta0, Y, Et0, tau, F0, splineInfo)
  d0 = Fall$d; lambda0 = Fall$lambda; Beta0 = Fall$Beta 
  
  # To obtain ordering, run a few simple simulations:
  dArray = array(diag(C*K), c(C*K, C*K, 1)) # diagonal 
  Model0 = SSModel(Y~-1+SSMcustom(Z = array(0, c(ncol(Y), C*K)), T = dArray, Q = dArray, P1 = diag(10^4, C*K)))
  for(nsi in 1:10){
    samples = mfdlmF(Beta0, Y, Et0, tau, d0, splineInfo, lambda0, orderLambdas = FALSE)
    d0 = samples$d; lambda0 = samples$lambda; Beta0 = mfdlmBeta(Y, Et0, dArray, dArray, Model0, tau, d0, splineInfo)
    print(paste('Running preliminary simulations: ', nsi,'/10', sep=''))
  }
  
  # Order the k components based on the smoothing parameters, lambda:
  adjOrder = order(lambda0, decreasing = TRUE)
  lambda0 = lambda0[adjOrder]; d0 = d0[,adjOrder]; 
  for(c in 1:C){ bc.inds = betaInds[c]:(betaInds[c+1]-1); Beta0[,bc.inds] = Beta0[,bc.inds][, adjOrder]}
  
  # Make sure the k = 1 curve is positive (usually an overall level or intercept)
  # Might as well initialize all curves to have positive sums
  for(k in 1:K){ if(sum(splineInfo$Phi%*%d0[,k]) < 0){ d0[,k] = - d0[,k]; Beta0[,seq(from = k, to = K*C, by=K)] = - Beta0[,seq(from = k, to = K*C, by=K)]}}
  
  list(Beta0=Beta0, d0=d0, Et0=Et0, lambda0=lambda0, splineInfo=splineInfo, K=K)
}
#####################################################################################################
#####################################################################################################


#####################################################################################################
# mfdlmFinit() initializes the multivariate FPC and the smoothing parameters 
#####################################################################################################
mfdlmFinit = function(Beta, Y, Et, tau, F0, splineInfo){
  outcomeInds = c(1, which(tau[-1] < tau[-length(tau)])+1, length(tau)+1)
  allTaus = sort(unique(tau))
  
  Phi = splineInfo$Phi; Jkl = splineInfo$Jkl
  
  C = length(outcomeInds) - 1 
  K = ncol(Beta)/C 
  
  # For outcome-specific Beta indices, subset with betaInds[c]:(betaInds[c+1]-1)
  betaInds = seq(1, C*(K+1), by=K)
  
  # Start w/ a small value for smoothness
  lambda = rep(10^-8, K) 
  
  d = array(0, c(ncol(Phi), K))
  
  for(k in 1:K){
    bksum = 0; Bksum = 0 
    for(c in 1:C){
      tauCinds = match(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], allTaus); yc.inds = outcomeInds[c]:(outcomeInds[c+1]-1); bc.inds = betaInds[c]:(betaInds[c+1]-1)		
      bksum = bksum + crossprod(Phi[tauCinds,], colSums((1/Et[c]*(Y[,yc.inds] - tcrossprod(Beta[,bc.inds[-k]], F0[[c]][,-k])))*Beta[,bc.inds[k]], na.rm=TRUE))
      cc = complete.cases(Y[,yc.inds]) 
      Bksum  = Bksum + crossprod(Phi[tauCinds,])*sum(Beta[cc,(betaInds[c]:(betaInds[c+1]-1))[k]]^2)/Et[c]
      for(i in which(!cc)) Bksum  = Bksum + Beta[i,bc.inds[k]]^2/Et[c]*crossprod(Phi[tauCinds,][!is.na(Y[i,yc.inds]),])
    } 
    priorPrec = diag(c(rep(10^-8, 2), rep(lambda[k], (ncol(Phi) - 2))))
    cholFac = chol(Bksum + priorPrec)
    dk = backsolve(cholFac,forwardsolve(t(cholFac),bksum))	
    # Use sequential orthogonality for initialization:	
    if(k > 1){
      Lcon = Jkl%*%d[,1:(k-1)]  	
      BLcon = backsolve(cholFac,forwardsolve(t(cholFac),Lcon))
      dk = dk - BLcon%*%chol2inv(chol(crossprod(Lcon, BLcon)))%*%crossprod(Lcon,dk)
    }
    Dnorm = sqrt(as.numeric(crossprod(dk,Jkl)%*%dk))
    d[,k] = dk/Dnorm
    Beta[, seq(from = k, to = K*C, by=K)] = Beta[, seq(from = k, to = K*C, by=K)]*Dnorm
    
    # Conditional MLE of lambda:
    lambda[k] = (ncol(Phi) - 2)/crossprod(d[-(1:2),k])
    
    for(c in 1:C){ tauCinds = match(tau[outcomeInds[c]:(outcomeInds[c+1]-1)], allTaus); F0[[c]][,k] = Phi[tauCinds,]%*%d[,k]}
  }
  list(d=d, lambda=lambda, Beta=Beta) 
}
#####################################################################################################
#####################################################################################################

#####################################################################################################
# getSplineInfo() initializes (and transforms) the spline basis
# Uses quantile-based placement of knots for a cubic spline basis
# Enfoces a penalty on the integrated squared second derivative 
# Computes the matrix of integrals for the orthonormality constraints
#####################################################################################################
getSplineInfo = function(tau){
  allTaus = sort(unique(tau)) 	# all observation points
  a = min(allTaus)        	# lower endpoint
  b = max(allTaus)        	# upper endpoint
  
  numIntKnots = 6												# number of interior knots (= M in paper) # 40
  intKnots = quantile(allTaus,seq(0,1,length= (numIntKnots+2))[-c(1,(numIntKnots+2))]) 	# interior knots
  
  basis = create.bspline.basis(c(a,b),breaks=c(a,intKnots,b))						# spline basis
  blin = create.monomial.basis(c(a,b), nbasis=2) 								# linear basis
  
  Phi = bs(allTaus,knots=intKnots,degree=3,Boundary.knots=c(a,b),intercept=TRUE) 		# basis matrix
  Omega = eval.penalty(basis, Lfdobj=2, rng=c(a,b)) 					 		# derivative matrix
  Jkl = eval.penalty(basis, Lfdobj=0, rng=c(a,b))						 		# integral matrix
  
  # Now, transform these matrices to align with the linear/nonlinear decomposition of d_k:
  # The first two entries of d_k are the linear components, the remaining are nonlinear (see Wand and Ormerod, 2008)
  eigOmega = eigen(Omega)
  indsZ = 1:(numIntKnots+2)
  UZ = eigOmega$vectors[, indsZ] 			# Linear part
  LZ = t(t(UZ)/sqrt(eigOmega$values[indsZ])) 	# Nonlinear part
  
  # Basis matrices
  PhiL = cbind(1, allTaus)				# Linear part
  PhiN = Phi%*%LZ						# Nonlinear part
  Phi = cbind(PhiL, PhiN)
  
  # Basis functions:	
  basisPhiL = function(x){cbind(1,x)}
  basisPhiN = function(x){bs(x,knots=intKnots,degree=3,Boundary.knots=c(a,b),intercept=TRUE)%*%LZ}
  basisPhi = function(x){cbind(1,x, bs(x,knots=intKnots,degree=3,Boundary.knots=c(a,b),intercept=TRUE)%*%LZ)}
  
  # Integrals:
  #JLL = inprod(blin, blin)
  JLL =  matrix(c(b-a, (b^2 - a^2)/2, (b^2 - a^2)/2, (b^3 - a^3)/3), nrow=2) 	# Linear x Linear
  JLN = inprod(blin, basis)%*%LZ								# Linear x Nonlinear
  JNN = crossprod(LZ, Jkl)%*%LZ									# Nonlinear x Nonlinear
  
  # Combine the integrals into one matrix:
  Jkl = cbind(rbind(JLL, t(JLN)), rbind(JLN, JNN))
  
  list(a=a, b=b, intKnots=intKnots, Phi=Phi, Jkl= Jkl, basisPhi = basisPhi)
}
#####################################################################################################
#####################################################################################################


#####################################################################################################
# getModelInfo(): computes many of the parameters required for the simulateSSM() 
#####################################################################################################
getModelInfo = function(model){
  nsim = 1
  ymiss <- is.na(model$y)
  storage.mode(ymiss) <- "integer"
  
  x <- array(abs(apply(model$H, 3, diag)) > model$tol, c(attr(model, "p"), attr(model, "n"))) & (!t(ymiss))
  x <- array(x, c(attr(model, "p"), attr(model, "n"), nsim))
  
  dfeps <- sum(x)/nsim
  
  x2 <- array(abs(apply(model$Q, 3, diag)) > model$tol, c(attr(model, "k"), (attr(model, "n") - 1) * attr(model, "tv")[5] + 1))
  x2 <- array(x2, c(attr(model, "k"), attr(model, "n"), nsim))
  
  dfeta <- sum(x2)/nsim
  
  nonzeroP1 <- which(diag(model$P1) > model$tol)
  nNonzeroP1 <- length(nonzeroP1)
  dfu <- dfeps + dfeta + nNonzeroP1
  
  nNonzeroP1inf = as.integer(sum(model$P1inf))
  c2 = numeric(1)
  
  zeroP1inf = which(diag(model$P1inf) > 0)
  
  list(ymiss=ymiss,  x=x, dfeps=dfeps, x2=x2, dfeta = dfeta, nonzeroP1 = nonzeroP1, nNonzeroP1 = nNonzeroP1, dfu = dfu, c2 = c2, nNonzeroP1inf = nNonzeroP1inf, zeroP1inf = zeroP1inf)
}
#####################################################################################################
#####################################################################################################



