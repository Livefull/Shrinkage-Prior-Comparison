args <- commandArgs(TRUE)

# Set of chosen models (keyword)
model_set <- args[1]

# Number of data replications for each model
num_rep <- as.double(args[2])

# Fits per array task
num_fits <- as.double(args[3])

# Cores per task
cores <- as.double(args[4])

# Time (hours x:00) needed per task
time <- args[5]

# Initialize fit diagnostics lists
setwd("/scratch/work/kasevat1/BDA_SA/SM")
chosen_models <- list.files()[grep(model_set, list.files())]
N <- length(chosen_models)
print("Chosen models:")
print(chosen_models)

fit_diagn_part <- list("ad"= 0.999, "iter" = 8000, "models" = chosen_models,  
                       "conv_values" = matrix(0, nrow = num_fits, ncol = N), 
                       "neff_names" = matrix(0, nrow = num_fits, ncol = 1), 
                       "lppds" = matrix(0, nrow = num_fits, ncol = 1), 
                       "pmses" = matrix(0, nrow = num_fits, ncol = 1), "num_fits" = num_fits, "num_cores" = cores)

fit_diagn <- list("models" = chosen_models, "conv_values" = array(rep(0, num_rep*3*N), dim=c(num_rep,3,N)), 
                  "neff_names" = matrix(0, nrow = num_rep, ncol = N), 
                  "lppds" = matrix(0, nrow = num_rep, ncol = N), 
                  "pmses" = matrix(0, nrow = num_rep, ncol = N))
  
# Triton paramaters
array_jobs <- N*num_rep/num_fits - 1
array_tasks <- num_rep/num_fits
  
setwd("/scratch/work/kasevat1/BDA_SA/")

# Write Triton sh file  
shfile<-file("triton_fit.slrm")
writeLines(c("#!/bin/bash"," ", paste0("#SBATCH --cpus-per-task=",cores), paste0("#SBATCH -t ", time, ":00:00"),
             "#SBATCH --mem-per-cpu=10000", paste0("#SBATCH --array=0-", array_jobs),
             "#SBATCH --constraint=hsw",
             "#SBATCH --output=outputs/fit_diagn_part_%a.out",
             " ", paste0("model=$(( $SLURM_ARRAY_TASK_ID / ", array_tasks," ))"), 
             paste0("replica=$(( $SLURM_ARRAY_TASK_ID % ", array_tasks, " ))"),
             " ", "module load r",
             "srun Rscript --default-packages=Rcpp ./fit_diagnostics_part.R $model $replica"), shfile)
close(shfile)

save(fit_diagn_part, file = "fit_diagn_part")
save(fit_diagn, file = "fit_diagn")
