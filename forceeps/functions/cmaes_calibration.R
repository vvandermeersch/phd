source(file.path(wd, "functions", "create_setupfile.R"))
source(file.path(wd, "functions", "create_commandfile.R"))
source(file.path(wd, "functions", "create_speciesfile.R"))
source(file.path(wd, "functions", "modify_speciesfile.R"))
source(file.path(wd, "functions", "linear_scaling.R"))
source(file.path(wd, "functions", "auc.R"))
source(file.path(wd, "functions", "read_mean_outputvalue.R"))
source(file.path(wd, "functions", "cmaes_mod.R"))



cmaes_calibration <- function(nruns, obj_function, ..., 
                              parameters=list(init, lb, ub, fixed, scale_factor),
                              is_feasible = function(x){TRUE},
                              controls=list(sigma=NULL, mu=NULL, lambda=NULL, maxit=NULL),
                              parallel=list(ncores_runs=1, ncores_eval=1),
                              cmaes_settings=list(rand_init=FALSE, ipop=FALSE, ipop_factor=NULL),
                              capsis_settings,
                              species,
                              sim_options, commandfile_options
                              ){
  
  start_time_total <- Sys.time()
  
  # Print messages
  cat(paste0(Sys.time(),"\n"))
  cat("Starting CMA-ES calibration\n")
  cat("-------------------------------------\n")
  cat(paste0(nruns, " run(s) with ", parallel$ncores_eval, " parallel simulations\n"))
  cat("-------------------------------------\n")
  
  # Initialisation
  cmaes_folder <- file.path(wd, "calibration")
  
  unlink(file.path(cmaes_folder, "process_files"), recursive = TRUE)
  dir.create(file.path(cmaes_folder, "process_files"), showWarnings = FALSE)
  create_setupfile(sim_options$setup_file, sim_options)
  
  if(controls$lambda%%parallel$ncores_eval!=0){stop("Lambda doit etre un multiple de ncores_eval")}
  quot <- controls$lambda%/%parallel$ncores_eval
  
  # create command files and species files
  runlines <- list()
  ind_paths <- c()
  dir.create(file.path(cmaes_folder, "process_files", "runs"), showWarnings = FALSE)
  for(i in 1:parallel$ncores_eval){
    
    core_folder <- file.path(cmaes_folder, "process_files", "runs", paste0("core", i))
    dir.create(core_folder, showWarnings = FALSE)
    
    start <- (i-1)*quot + 1
    end <- i*quot
    
    runs <- c()
    
    for(j in start:end){
      commandfiles_dir <- file.path(core_folder, paste0("ind", j))
      dir.create(commandfiles_dir, showWarnings = FALSE)
      
      #create species file
      species_file <- file.path(commandfiles_dir, paste0(j, ".species"))
      create_speciesfile(file = species_file, species$structure_file, species_name = species$name)
      modify_speciesfile(parameters$init, param_fixed=NULL, file = species_file)
      
      # save species path
      ind_paths <- c(ind_paths, commandfiles_dir)
      
      # create command_file
      command_file <- file.path(commandfiles_dir, "commandfile")
      create_commandfile(command_file, commandfile_options, species_file = paste0(j, ".species"))
      
      # save runline
      if(!is.null(capsis_settings$cd_java8)){
        run <- paste(capsis_settings$cd_java8, capsis_settings$java8, 
                     capsis_settings$cd, paste(capsis_settings$forceeps_run, command_file), sep=' && ')
      }else{
        run <- paste(capsis_settings$cd, paste(capsis_settings$forceeps_run, command_file), sep=' && ')
      }
      
      runs <- c(runs, run)
      
    }
    
    runlines[i] <- list(runs)
  
  }
  
  # future settings
  plan(list( tweak(multisession, workers = parallel$ncores_runs),
             tweak(multisession, workers = parallel$ncores_eval)))

  output <- future_apply(array(1:nruns), 1, function(i){

    out_cmaes <- cmaes_vectorized(nruns=nruns, obj_function, ...,
                                  params = parameters, grid = commandfile_options$grid, 
                                  ind_paths = ind_paths, runlines = runlines,
                                  parameters=parameters, controls=controls, is_feasible = is_feasible)

    Sys.sleep(1)
    cat("---------------------------------------\n")
    cat(paste0("Run ",i,"/", nruns, " on host ", Sys.info()[["nodename"]], ", pid ", Sys.getpid(),"\n"))
    cat(paste("AUC =", round(1-out_cmaes$value,3),"\n"))
    cat(paste("Number of function evaluations :",out_cmaes[["counts"]][["function"]],"\n"))
    cat(paste("Runtime :",
              out_cmaes$runtime,"secondes\n"))


    # save best species file
    # dir.create(file.path(cmaes_folder, "output_files"), showWarnings = FALSE)
    # out_species_file <- file.path(cmaes_folder, "output_files", paste0("cmaes_fit_", Sys.Date(), ".txt"))
    # create_speciesfile(out_species_file, species$structure_file, species_name = species$name)
    # best_parameters_inv <- out_cmaes[["par"]]
    # best_parameters <- inv_linearscaling(best_parameters_inv, parameters$lb[!(names(parameters$lb) %in% parameters$fixed)], parameters$ub[!(names(parameters$lb) %in% parameters$fixed)],
    #                            parameters$scale_factor)
    # modify_speciesfile(best_parameters, param_fixed=parameters$fixed, out_species_file)

  }, future.seed=TRUE)

  
}


# Some modification to add runtime and linearscaling
cmaes_vectorized <- function(nruns, obj_function, ..., 
                             parameters=list(init, lb, ub, fixed, scale_factor), 
                             controls=list(sigma=NULL, mu=NULL, lambda=NULL, maxit=NULL),
                             is_feasible){
  
  start_time <- Sys.time()
  
  # Linear scaling
  
  # if kType is not fixed, we first need to remove the E/D letter
  if(!("kType" %in% parameters$fixed)){parameters$init$kType <- as.numeric(substr(parameters$init$kType, 2,2))}
  param_init_s <- unlist(linearscaling(parameters$init[!(names(parameters$init) %in% parameters$fixed)], 
                                parameters$lb[!(names(parameters$lb) %in% parameters$fixed)], parameters$ub[!(names(parameters$ub) %in% parameters$fixed)], 
                                parameters$scale_factor))
  
  param_ub_s <- unlist(linearscaling(parameters$ub[!(names(parameters$ub) %in% parameters$fixed)], 
                              parameters$lb[!(names(parameters$lb) %in% parameters$fixed)], parameters$ub[!(names(parameters$ub) %in% parameters$fixed)], 
                              parameters$scale_factor))
  
  param_lb_s <- unlist(linearscaling(parameters$lb[!(names(parameters$lb) %in% parameters$fixed)], 
                              parameters$lb[!(names(parameters$lb) %in% parameters$fixed)], parameters$ub[!(names(parameters$ub) %in% parameters$fixed)], 
                              parameters$scale_factor))
  
  cma_output <- cmaes_mod(par=param_init_s, fn=obj_function, ...,
                          lower=param_lb_s, upper=param_ub_s, 
                          control=list(sigma=controls$sigma, mu=controls$mu, lambda=controls$lambda, 
                                       maxit=controls$maxit, vectorized=T),
                          is_feasible = is_feasible,
                          log_file="C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps/calibration/logs/CMAES_log.txt")
  
  end_time <- Sys.time()
  
  return(c(cma_output, runtime=round(as.numeric(end_time - start_time, units="secs"),1)))
  
}






