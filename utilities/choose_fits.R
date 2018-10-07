choose_fits <- function(fit_diagn, models, num_rep){
 
  # Initialize fit diagnostics array 
  N <- length(models)
  chosen_fit_diagn <- list("models" = models, "conv_values" = array(rep(0, num_rep*3*N), dim=c(num_rep,3,N)), 
                      "neff_names" = matrix(0, nrow = num_rep, ncol = N), 
                      "lppds" = matrix(0, nrow = num_rep, ncol = N), 
                      "pmses" = matrix(0, nrow = num_rep, ncol = N))
    
  indices <- match(models, fit_diagn$models)
  
  # Fill diagnostics array
  chosen_fit_diagn$conv_values <- fit_diagn$conv_values[, , indices]
  chosen_fit_diagn$neff_names <- fit_diagn$neff_names[, indices]
  chosen_fit_diagn$lppds <- fit_diagn$lppds[, indices]
  chosen_fit_diagn$pmses <- fit_diagn$pmses[, indices]
    
  return(chosen_fit_diagn)
    

}