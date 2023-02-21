

phenofit_ABC <- function(x) {
  
  # init
  wd <<- "C:/Users/vandermeersch/Documents/CEFE/thesis/phenofit/ABC_calibration"
  source(paste0(wd, "/functions/create_init_species_file.R"))
  structure_file <- paste0(wd, "/input_files/Fagus_sylvatica_EvolLett2019.species")
  source(paste0(wd, "/functions/command_file_setup.R"))
  source(paste0(wd, "/functions/read_species_file.R"))
  source(paste0(wd, "/functions/read_mean_fitness.R"))
  param_init <- read_species_file(paste0(wd, "/input_files/fagus_sylvatica/Fagus_sylvatica_init.species"))
  param_fixed <- read_species_file(paste0(wd, "/input_files/fagus_sylvatica/Fagus_sylvatica_fixed.species"))
  climate_folder <- "D:/climate/ERA5-Land/phenofit_format/fagus_sylvatica_extraction/1000pres_1000abs/subset_1"
  capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                       cd ="cd/d D:/applications/capsis4 && setmem 2000")
  load("D:/species/processed/fagus_sylvatica/1000pres_1000abs/occurrence_subset_1.Rdata")
  Yobs <- species_occurrence
  library(AUC)
  
  
  set.seed(x[1])
  i <- x[1]
  x <- x[2:length(x)]
  
  # create initial files
  command_file <- paste0(wd, "/process_files/", "CommandFile_id",i,".txt")
  output_folder <- paste0(wd,"/process_files/output/id",i,"/run")
  sp_file <- paste0("/process_files/params/", "parameters_id", i)
  species_file <- create_init_species_file(sp_file, param_init, structure_file)
  command_file_setup(command_file, species_file, output_folder, climate_folder,
                     climate_scenario="ERA5LAND", starting_year="1970", ending_year="2000", quiet_mode="true")
  run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptVictor", command_file)
  if(!is.na(capsis_settings$java8)){
    run_all <- paste(capsis_settings$cd_java8, capsis_settings$java8, 
                     capsis_settings$cd, run_capsis, sep=' && ')
  }else{
    run_all <- paste(capsis_settings$cd, run_capsis, sep=' && ')
  }
  
  # modify species file
  param_f <- which(param_fixed!="FIXED")
  # If mixt bud
  if(param_init["cpdbud"]=="2"){
    flower_param <- grepl("flower", names(param_f)) # get indices
    flower_param[max(which(flower_param))] <- FALSE # exclude last parameter of flower model (Fcrit)
    len <- length(x) #number of parameters found by algorithm
    nb <- length(which(flower_param)) #number of parameters to copy from leaf model to flower model
    x <- c(x[1:(nb+1)], x[1:nb], x[(nb+2):len]) # new parameter list to modify
  }
  modify_species_file(as.character(x), param_fixed, species_file)
  
  # run model
  out <- shell(run_all, intern = T)
  
  # read fitness and compute AUC
  Ysim <- read_mean_fitness(1, output_folder)
  obs <- as.factor(Yobs$pres)
  Ysim[is.na(Ysim)] <- 0 # NA fitness values set to zero
  roc_pred <- roc(Ysim, obs)
  auc_pred <- auc(roc_pred)
  
  # unlink(paste0(wd,"/process_files/output/id",i), recursive = TRUE)
  # unlink(species_file)
  # unlink(command_file)
  
  
  return(1-auc_pred)
  
}


define_priors <- function(param_lb, param_ub, param_distrib, param_fixed){
  
  param_f <- which(param_fixed!="FIXED")
  
  # If mixt bud
  if(param_init["cpdbud"]=="2"){
    flower_param <- grepl("flower", names(param_f)) # get indices
    flower_param[max(which(flower_param))] <- FALSE # exclude last parameter of flower model (Fcrit)
    
    param_f <- param_f[!flower_param] # new fixed parameter indice list
  }
  
  param_l <- as.numeric(param_lb[param_f])
  param_u <- as.numeric(param_ub[param_f])
  
  priors <- lapply(1:length(param_l), function(i){
    c(param_distrib[i], param_l[i], param_u[i])
  })
  
  return(priors)
}

