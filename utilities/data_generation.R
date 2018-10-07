data_generation <- function(fit_diagn, switch, model_files){

  ################################################
  ####### DATA GENERATION SIMULATION STUDY #######
  ####### Author: Sara van Erpm            #######
  ################################################
  
  rm(list=ls()) # clears workspace
  require(MASS) # to generate from a multivariate normal distribution
 

  
  set.seed(12052017)
  nrep <- 500
  
  ## data generating function
  sim.regression1 <- function(nobs, cfs, sd, cov.mat){
    nvar <- length(cfs)  
    
    beta <- as.matrix(cfs)
    X <- mvrnorm(nobs, mu=rep(0, nvar), Sigma=cov.mat)
    y <- X %*% beta + rnorm(nobs,0,sd)
    
    return (list(x=X, y=y, coefficients=cfs, var=sd^2, covX=cov.mat))
  }
  
  ## generate data sets
  simdata <- list()
  
  # pairwise correlations between observed scores equal to 0.5:
  Sig <- matrix(0, ncol=38, nrow=38)
  Sig[1:38, 1:38] <- 0.5
  diag(Sig) <- 1
  for(i in 1:nrep){
    simdata[[i]] <- sim.regression1(nobs=2040, cfs=c(3, 1.5, 0, 0, 2, 0, 0, 0, rep(0, 30)), sd=sqrt(9), cov.mat=Sig)
  }
  
  ## split in training & test set:
  traintest1 <- function(dat, ntrain){
    trainX <- dat$x[1:ntrain, ]
    testX <- dat$x[(ntrain+1):nrow(dat$x), ]
    trainY <- dat$y[1:ntrain]
    testY <- dat$y[(ntrain+1):nrow(dat$y)]
    return(list(trainX=trainX, trainY=trainY, testX=testX, testY=testY, coeff=dat$coefficients, var=dat$var, covX=dat$covX))
  }
  
  simdata.split <- lapply(simdata, traintest1, ntrain=40)
  project_data <- simdata.split
  
  ## save 
  save(project_data, file="project_data.RData")
  
}

