library(future)
library(future.apply)
library(cmaes)



source(paste0(wd, "/functions/linear_scaling.R"))
source(paste0(wd, "/functions/create_init_species_file.R"))
source(paste0(wd, "/functions/command_file_setup.R"))
source(paste0(wd,"/functions/cmaes_mod.R"))

#cmaes_settings=list(rand_init=FALSE, ipop=FALSE, ipop_factor=NULL)
#parallel=list(ncores_runs=1, ncores_eval=1)
#parameters=list(param_init, param_lb, param_ub, scale_factor)
#controls=list(sigma=NULL, mu=NULL, lambda=NULL, maxit=NULL)
#species=list(structure="/input_files/Fagus_sylvatica_structure.species", file="/process_files/params/Fagus_sylvatica")
#folders=list(output, climate)
#simulation=list(command_file, climate_scenario, starting_year, ending_year, tmp_means)
#capsis_settings=list(java8=TRUE, cd_java8=NA, cd_capsis)



cmaes_custom <- function(nruns, obj_function, ..., 
                         parameters=list(param_init, param_fixed, param_lb, param_ub, scale_factor),
                         controls=list(sigma=NULL, mu=NULL, lambda=NULL, maxit=NULL),
                         parallel=list(ncores_runs=1, ncores_eval=1),
                         cmaes_settings=list(rand_init=FALSE, ipop=FALSE, ipop_factor=NULL),
                         species=list(structure, file),
                         folders=list(wd, climate),
                         simulation=list(climate_scenario, starting_year, ending_year, tmp_means),
                         capsis_settings=list(java8=NA, cd_java8=NA, cd_capsis)){
  
  
  
  start_time_total <- Sys.time()
  cat(paste0(Sys.time(),"\n"))
  cat("Starting CMA-ES calibration\n")
  cat("---------------------------------------\n")
  cat(paste0(nruns, " run(s) on ", parallel$ncores_runs, " node(s)\n"))
  cat(paste0("Each run will use ", parallel$ncores_eval, " cores\n"))
  cat("---------------------------------------\n")
  
  # Parameter initialisation
  structure_file <- species$structure
  sp_file <- paste0("/process_files/params/",species$file)
  species_files <- create_init_species_file(sp_file, parameters$param_init, structure_file, controls$lambda)
  
  
  # if OF eval is parallelized 
  if(parallel$ncores_eval>1){
    if(controls$lambda%%parallel$ncores_eval!=0){stop("Lambda doit être un multiple de ncores_eval")}
    run_all <- c()
    quot <- controls$lambda%/%parallel$ncores_eval
    for(i in 1:parallel$ncores_eval){
      # CommandFile setup
      command_file <- paste0(wd, "/process_files/", "CommandFile_core",i,".txt")
      output_folder <- paste0(folders$wd,"/process_files/output/core",i,"/run")
      species_files_i <- species_files[(1+(i-1)*quot):(i*quot)]
      command_file_setup(command_file, species_files_i, output_folder,
                         folders$climate, simulation$climate_scenario,
                         simulation$starting_year, simulation$ending_year,
                         simulation$tmp_means)
      
      # Command lines
      run_capsis <- paste("sh capsis.sh -p script phenofit4.myscripts.ScriptVictor", command_file)
      
      # Do we need to switch to Java8 ?
      if(!is.na(capsis_settings$java8)){
        run_all[i] <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')
      }else{
        run_all[i] <- paste(capsis_settings$cd_capsis, run_capsis, sep=' && ')
      }
      
    }
    
  }else{
    # CommandFile setup
    command_file <- paste0(wd, "/process_files/", "CommandFile.txt")
    output_folder <- paste0(folders$wd,"/process_files/output/core",1,"/run")
    command_file_setup(command_file, species_files, output_folder,
                       folders$climate, simulation$climate_scenario,
                       simulation$starting_year, simulation$ending_year,
                       simulation$tmp_means)
    
    # Command lines
    run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptVictor", command_file)
    
    # Do we need to switch to Java8 ?
    if(!is.na(capsis_settings$java8)){
      run_all <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')
    }else{
      run_all <- paste(capsis_settings$cd_capsis, run_capsis, sep=' && ')
    }
    
  }
  
  
  # future settings
  plan(list( tweak(multisession, workers = parallel$ncores_runs), 
             tweak(multisession, workers = parallel$ncores_eval))) 

  output <- future_apply(array(1:nruns), 1, function(i) {
    
    # if(cmaes_settings$rand_init){
    #   parameters$param_init <- apply(as.data.frame(cbind(parameters$param_lb, parameters$param_ub)),1,
    #                                  function(x) runif(1,min=x[1],max=x[2]))
    # }
    
    #if(cmaes_settings$ipop){}
    #p(message = paste0("Starting run ", i, "\n"), "sticky", amount = 0)
    eval(parse(text=paste0("it_run", i, "<<-1")))
    out_cmaes <- cmaes_vectorized(nruns=nruns, obj_function, ..., run=i, run_all = run_all, wd=folders$wd, 
                                  species_files=species_files, lambda = controls$lambda, param=parameters,
                                  parameters=parameters, controls=controls)
    
    
    Sys.sleep(1)
    cat("---------------------------------------\n")
    cat(paste0("Run ",i,"/", nruns, " on host ", Sys.info()[["nodename"]], ", pid ", Sys.getpid(),"\n"))
    cat(paste("AUC =", round(1-out_cmaes$value,3),"\n"))
    cat(paste("Number of function evaluations :",out_cmaes[["counts"]][["function"]],"\n"))
    cat(paste("Runtime :", 
              out_cmaes$runtime,"secondes\n"))
    
    
    return(out_cmaes)
    
    }, 
    future.seed=1)
  
  
  end_time_total <- Sys.time()
  
  cat("---------------------------------------\n")
  cat(paste("Total runtime :", 
                round(as.numeric(end_time_total - start_time_total, units="hours"),1),"hours"))
  
  return(output)
  
}




cmaes_vectorized <- function(nruns, obj_function, ..., 
                             parameters=list(param_init, param_fixed, param_lb, param_ub, scale_factor), 
                             controls=list(sigma=NULL, mu=NULL, lambda=NULL, maxit=NULL)){
  
  start_time <- Sys.time()
  
  
  # Linear scaling
  param_f <- which(parameters$param_fixed!="FIXED")
  param_init <- as.numeric(parameters$param_init[param_f])
  param_lb <- as.numeric(parameters$param_lb[param_f])
  param_ub <- as.numeric(parameters$param_ub[param_f])
  param_init_s <- linearscaling(param_init, param_lb, param_ub, 
                                scale_factor)
  param_ub_s <- linearscaling(param_ub, param_lb, param_ub, 
                              scale_factor)
  param_lb_s <- linearscaling(param_lb, param_lb, param_ub, 
                              scale_factor)
  
  cma_output <- cmaes_mod(par=param_init_s, fn=obj_function, ...,
                       lower=param_lb_s, upper=param_ub_s, 
                       control=list(sigma=controls$sigma, mu=controls$mu, lambda=controls$lambda, 
                                    maxit=controls$maxit, vectorized=T), 
                       log_file="/scratch/vvandermeersch/inverse_modelling_cluster2/logs/CMAES_log.txt")
  
  end_time <- Sys.time()

  return(c(cma_output, runtime=round(as.numeric(end_time - start_time, units="secs"),1)))
  
}




format_cmaes_results <- function(cluster_output){
  results <- data.frame()
  
  for(i in 1:length(cluster_output)){
    results[i, "OF_value"] <- cluster_output[[i]]["value"]
    for(j in 1:length(cluster_output[[i]][["par"]])){
      results[i, paste("par",j, sep = "_")] <- inv_linearscaling(cluster_output[[i]][["par"]][[j]],
                                                                 param_lb[j], param_ub[j], scale_factor)
    }
    results[i, "runtime"] <- cluster_output[[i]]["runtime"]
    
  }
  
  return(results)
}
