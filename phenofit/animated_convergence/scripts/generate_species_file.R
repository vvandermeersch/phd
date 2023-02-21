# Script to create species file from RDS trace file

source(paste0(wd, "/functions/create_species_file.R"))
source(paste0(wd, "/functions/modify_species_file.R"))
source(paste0(wd, "/functions/linear_scaling.R"))

filename <- substring(rds, 1, nchar(rds)-4)

param_f <- which(parameters$param_fixed!="FIXED")

# If mixt bud
if(parameters$param_init["cpdbud"]=="2"){
  flower_param <- grepl("flower", names(param_f)) # get indices
  flower_param[max(which(flower_param))] <- FALSE # exclude last parameter of flower model (Fcrit)
  
  param_f <- param_f[!flower_param] # new fixed parameter indice list
}

param_lb <- as.numeric(parameters$param_lb[param_f])
param_ub <- as.numeric(parameters$param_ub[param_f])

param_f <- which(parameters$param_fixed!="FIXED") # inital fixed parameter indice list

x_inv <- inv_linearscaling(x, param_lb, param_ub, 
                           parameters$scale_factor)

# If mixt bud
if(parameters$param_init["cpdbud"]=="2"){
  len <- length(x_inv) #number of parameters found by cmaes algorithm 
  nb <- length(which(flower_param)) #number of parameters to copy from leaf model to flower model
  x_inv <- c(x_inv[1:(nb+1)], x_inv[1:nb], x_inv[(nb+2):len]) # new parameter list to modify
  
}

species_file <- file.path(wd, "calibration_files", paste0(filename, ".species"))

create_species_file(species_file, structure_file)
modify_species_file(parameters$param_init, param_fixed=NULL, species_file)

modify_species_file(as.character(x_inv), parameters$param_fixed, species_file)