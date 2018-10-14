rm(list=ls()) # clear workspace
set.seed(18082017)

library(rstan)
library(parallel)


# Model and replication parameters
args <- commandArgs(TRUE)
model_num <- as.double(args[1])
rep_index <- as.double(args[2])
model_num <- model_num + 1
rep_index <- rep_index + 1

# Diagnostics for fit part
load("fit_diagn_part")
num_fits <- fit_diagn_part$num_fits
reps <- num_fits*(rep_index-1)+1

# Models for evaluation
models <- fit_diagn_part$models

# Load generated data as project_data
load("project_data.RData") 

# Load corresponding Stan model
setwd("/scratch/work/kasevat1/BDA_SA/SM")
load(models[model_num])
setwd("/scratch/work/kasevat1/BDA_SA")

# Number of cores to use
cores <- fit_diagn_part$num_cores

for (rep in reps:(reps+num_fits-1)){
    
    # Initialize data
    x <- project_data[[rep]]

    if(model@model_name  == "regularized_horseshoe"){
      p0 <- 3
      tau0 <- p0/((ncol(x$trainX) - p0) * sqrt(nrow(x$trainX))) # tau0 is based on a prior guess of the # of relevant predictors p0
      standat <- list(N_train=nrow(x$trainX), p=ncol(x$trainX), y_train=c(x$trainY), X_train=x$trainX,
                      N_test=nrow(x$testX), X_test=x$testX, y_test=x$testY, 
                      scale_global= tau0, nu_global= 1, nu_local= 1, slab_scale=2, slab_df=4)
      
    
    }    		
    else{standat <- list(N_train=nrow(x$trainX), p=ncol(x$trainX), y_train=c(x$trainY), X_train=x$trainX,
                      N_test=nrow(x$testX), X_test=x$testX, y_test = x$testY)}
    
    # Fit the model
    fit.mcmc <- sampling(model, data=standat, iter=fit_diagn_part$iter, 
                         control=list(adapt_delta=fit_diagn_part$ad, max_treedepth=15),
                         cores = getOption("mc.cores", cores))
    #Extract summary
    fitsum <- summary(fit.mcmc)$summary
    rhat <- fitsum[which(fitsum[, "Rhat"] > 1.1), "Rhat"] # PSR > 1.1
    neffs <- fitsum[which(fitsum[, "n_eff"] > 0), "n_eff"] 
    sp <- get_sampler_params(fit.mcmc, inc_warmup=F)
    div <- sapply(sp, function(x) sum(x[, "divergent__"])) # divergent transitions
    
    # Fill in convergence values
    fit_diagn_part$conv_values[rep-reps+1, 1] <- length(rhat)
    fit_diagn_part$conv_values[rep-reps+1, 2] <- sum(div)
    neffs <- sort(neffs)
    fit_diagn_part$conv_values[rep-reps+1, 3] <- neffs[1]
    fit_diagn_part$neff_names[rep-reps+1, 1] <- names(neffs[1])
      
    
    #Calculte LPPDs
    log_lik <- extract(fit.mcmc, par = "test_log_lik")
    lik <- exp(log_lik[[1]])
    lik_means <- apply(lik, 2, mean)
    fit_diagn_part$lppds[rep-reps+1, 1] <- sum(log(lik_means))
      
    #Calculate MPSEs
    y_pred <- fitsum[grep("y_pred", rownames(fitsum)), "mean"]
    fit_diagn_part$pmses[rep-reps+1, 1] <- mean((y_pred - x$testY)^2)
    
}
  
  
setwd("/scratch/work/kasevat1/BDA_SA/FDPS")
# Save simulation data
save(fit_diagn_part, file = paste0("fdp", model_num-1, ".",  rep_index-1))
