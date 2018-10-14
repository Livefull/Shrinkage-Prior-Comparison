rm(list=ls()) # clear workspace
set.seed(18082017)


# Initialize diagnostics lists
setwd("/scratch/work/kasevat1/BDA_SA")
load("fit_diagn")

# Get parameters
setwd("/scratch/work/kasevat1/BDA_SA/FDPS")
load(paste0("fdp", 0,".", 0))
num_fits <- fit_diagn_part$num_fits
model_num <- c(0:(dim(fit_diagn$lppds)[2]-1))
rep_len <- dim(fit_diagn$lppds)[1]/num_fits-1
rep_indices <- c(0:rep_len)

for(i in model_num){
  for(j in rep_indices){
    load(paste0("fdp", i, ".", j))
    reps <- j*num_fits+1
    
    # Fill diagnostics
    fit_diagn$conv_values[reps:(reps+num_fits-1), 1, i+1] <- fit_diagn_part$conv_values[, 1]
    fit_diagn$conv_values[reps:(reps+num_fits-1), 2, i+1] <- fit_diagn_part$conv_values[, 2]
    fit_diagn$conv_values[reps:(reps+num_fits-1), 3, i+1] <- as.integer(fit_diagn_part$conv_values[, 3])
    fit_diagn$neff_names[reps:(reps+num_fits-1), i+1] <- fit_diagn_part$neff_names
    fit_diagn$lppds[reps:(reps+num_fits-1), i+1] <- fit_diagn_part$lppds
    fit_diagn$pmses[reps:(reps+num_fits-1), i+1] <- fit_diagn_part$pmses
    
  }
}

setwd("/scratch/work/kasevat1/BDA_SA")
save(fit_diagn, file = "fit_diagnostics_new")
