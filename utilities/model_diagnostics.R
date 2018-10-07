show_diagnostics <- function(fit_diagn, switch, show_neff){

  # Parameters
  num_models <- dim(fit_diagn$conv_values)[3]
  num_reps <- dim(fit_diagn$conv_values)[1]
  conv_values <- fit_diagn$conv_values
  neff_names <- fit_diagn$neff_names
  lppds <- fit_diagn$lppds
  pmses <- fit_diagn$pmses
  
  # Model names for predictive performance and convergence diagnostics
  model_names <- fit_diagn$models

  # Statistics for convergence diagnostics
  if(switch == 0){
    for(i in 1:2){
      
      # Initialize data frames
      stat.table <- data.frame(mean = 1:num_models, sd = 0, max = 0, min = 0, row.names = model_names)
    
      # Collect statistics
      for (k in 1:num_models){
        data_vec <- conv_values[, i, k]
        data_vec <- c(mean(data_vec), sd(data_vec), max(data_vec), min(data_vec))
        stat.table[k, 1:4] <-lapply(data_vec, round, 1) 
      }
    
      if(i == 2){print(kable(stat.table, format = "pandoc", caption = "Statistics of divergent transitions"))}
      else{print(kable(stat.table, format = "pandoc", caption = "Number of RHAT values above 1.1"))}
    }
    
    if(show_neff == TRUE){
      # Visualization of N_eff values
      par(mfrow = c(1, num_models),oma=c(0,0,2,0))
      for (k in 1:num_models){
        hist(conv_values[, 3, k], breaks = 20, xlab = "N_eff value", main = model_names[k], col = "black")
      }
      title("Histograms of distributions of minimum N_eff-values", outer = TRUE)  
      
      # Could be useful but not necessary in special assignment
      if(0){
      
        # Visualization of N_eff names
        par(mfrow = c(num_models, 1),oma=c(0,0,2,0))
        for (k in 1:num_models){
          neff_n <- as.data.frame(sort(table(neff_names[,k]), decreasing = TRUE)[1:5])
          plot(neff_n, xlab = "N_eff-variable")
          title(model_names[k])
        }
        title("Most frequently occuring minimum N_eff-variables", outer = TRUE)  
      }
    }
  }
  
  # Statistics for predictive perfromance
  if(switch == 1){
    
    # Visualization of PMSE and LPPD values
    PMSES <- data.frame()
    LPPDS <- data.frame()
    for(i in 1:dim(pmses)[2]){
      P <- data.frame(PMSE = pmses[, i])
      L <- data.frame(LPPD = lppds[, i])
      P$Model <- model_names[i]
      L$Model <- model_names[i]
      PMSES <- rbind(PMSES, P)
      LPPDS <- rbind(LPPDS, L)
      
      
    }
    
    print(ggplot(PMSES, aes(PMSE, fill = Model)) + geom_density(alpha = 0.2) + 
            ggtitle("Distribution of PMSE values") + theme(plot.title = element_text(hjust = 0.5))) 
    print(ggplot(LPPDS, aes(LPPD, fill = Model)) + geom_density(alpha = 0.2) + 
            ggtitle("Distribution of LPPD values") + theme(plot.title = element_text(hjust = 0.5))) 

  }
  
  # Model ranking
  if(switch == 2){
    for(i in 4:5){
      values <- fit_diagn[[i]]
      rank_val <- apply(abs(values), 1, rank)
      
      # Initialize data frames
      stat.table <- data.frame(mean = 1:num_models, sd = 0, max = 0, min = 0, row.names = model_names)
      
      # Collect statistics
      for (k in 1:num_models){
        data_vec <- rank_val[k, ]
        data_vec <- c(mean(data_vec), sd(data_vec), max(data_vec), min(data_vec))
        stat.table[k, 1:4] <- lapply(data_vec, round, 1) 
      }
      
      if(i == 4){print(kable(stat.table, format = "pandoc", caption = "Ranking values of PMSES (lower is better)"))}
      else{print(kable(stat.table, format = "pandoc", caption = "Ranking values of LPPDS (lower is better)"))}
    }
    
  }
  
}

dt_visual <- function(model_fit){
  
  
  # Load chosen horseshoe fit as fit.mcmc and extract summary (fitted with replication 1)
  load(model_fit)
  fitsum <- summary(fit.mcmc)$summary
  
  # Collect convergence values
  neffs <- fitsum[which(fitsum[, "n_eff"] > 0), "n_eff"] 
  rhat <- fitsum[which(fitsum[, "Rhat"] > 1.1), "Rhat"] 
  sp <- get_sampler_params(fit.mcmc, inc_warmup=F)
  div <- sapply(sp, function(x) sum(x[, "divergent__"])) 
  neffs_sorted <- sort(neffs)[1:6]
  neffindices <- match(neffs_sorted, neffs)
  
  # Print information about prior
  print(paste0("Model name: ", gsub(".FIT", " ", model_fit)))
  print(paste0("Number of RHAT values above 1.1: ", length(rhat)))
  print(paste0("Number of divergent transitions: ", sum(div)))
  print(paste0("Minimum N_eff value and corresponding variable: ", as.integer(neffs_sorted[1]), ", ", names(neffs_sorted[1])))
  
  # Investigate concentrations of divergences
  np <- nuts_params(fit.mcmc)
  color_scheme_set("darkgray")
  
  # Extract draws 
  draws <- as.array(fit.mcmc)
  
  # Normalize draws for each chain and each parameter
  for(i in 1:dim(draws)[3]){
    for(j in 1:dim(draws)[2]){
      draws.i <- draws[1:4000, j, i]
      M <- matrix(draws.i, 4000, 1)
      
      #Check maximum value
      maxv <- max(M)
      minv <- min(M)
      draws[1:4000, j, i] <- (draws[1:4000, j, i]-minv)/(maxv-minv)
      }
      
  }
  
  # Make sure draws are positive and take a natural logarithm (for mcmc_parcoord)
  par_draws <- log(draws +0.5)
  
  # Visualization of divergent trajectories
  div_style <- parcoord_style_np(div_color = "green", div_size = 0.2, div_alpha = 0.4)
  
  print(mcmc_parcoord(par_draws[1:2000, 1:4, neffindices], size = 0.05, alpha = 0.1,
                      np = np, np_style = div_style))
  
  # Pathological regions
  div_style <- pairs_style_np(div_color = "green", div_shape = 4, div_size = 4)
  scatter_list <- list()
  
  # Scatter plot
  y <- names(neffs_sorted[1:3])
  x <- names(neffs_sorted[4:6])
  for (i in 1:length(x)){
    scatter_list[[i]] <- mcmc_scatter(draws, pars = c(x[i], y[i]),
                                      np = np, np_style = div_style)
    
    
  }
  grid.arrange(scatter_list[[1]], scatter_list[[2]], scatter_list[[3]], ncol = 3)
}