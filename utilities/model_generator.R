model_generator <- function(project_data, model_files, fit_switch){


  # Load data
  load("project_data.RData")
  x <- project_data[[1]]
  
  
  for(model_file in model_files){
      
      # Initialize data for Stan model
      if(model_file == "regularized_horseshoe.stan"){
        p0 <- 3
        tau0 <- p0/((ncol(x$trainX) - p0) * sqrt(nrow(x$trainX))) # tau0 is based on a prior guess of the # of relevant predictors p0
        standat <- list(N_train=nrow(x$trainX), p=ncol(x$trainX), y_train=c(x$trainY), X_train=x$trainX,
                        N_test=nrow(x$testX), X_test=x$testX, y_test = x$testY, scale_global= tau0, nu_global= 1, nu_local= 1, slab_scale=2, slab_df=4)
        
      }
      else{standat <- list(N_train=nrow(x$trainX), p=ncol(x$trainX), y_train=c(x$trainY), X_train=x$trainX,
                           N_test=nrow(x$testX), X_test=x$testX, y_test = x$testY)}
      
  
      if(fit_switch == 0){
        # Save Stan models
        model <- stan_model(model_file)
        print(paste0("Stan model created for ", model_file))
        save(model, file = gsub(".stan",".SM", model_file))
      }

      else{
          # Save Stan fits
          load(model_file)
          fit.mcmc <- sampling(model, data=standat, iter=8000, control=list(adapt_delta=0.999
                                                                            , max_treedepth=15))
          save(fit.mcmc, file = gsub(".SM",".FIT", model_file))
      }
      
    
    
  }
  
  
}
