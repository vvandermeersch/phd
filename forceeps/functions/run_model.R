
run_model <- function(runlines, ncores){
  if(ncores>length(runlines)){stop("Number of cores must be less than or equal to the number of simulations !")}
  plan(multisession, workers = ncores)
  handlers("progress")
  prog <- progressor(length(runlines))
  prog(message = paste("Starting", length(runlines), "simulations on", ncores, "core(s)"), class = "sticky", amount = 0)
  if(ncores == 1){
    list_lines <- list(1:length(runlines))
  }else{
    list_lines <- split(1:length(runlines), cut(seq_along(1:length(runlines)), ncores, labels = FALSE))
  }
  out <- future_lapply(1:ncores, function(i){
    for(j in unlist(list_lines[i])){
      shell(runlines[j], intern=T)
      prog()
    }
  })
  plan(sequential)
  gc()
}


run_model_from_mat <- function(mat, ncores){
  runlines <- c()
  # create files 
  for(i in 1:nrow(mat)){
    ind_folder <- file.path(output_path, paste0("ind",i))
    dir.create(ind_folder, showWarnings = FALSE)
    
    # create species_file
    species_file <- file.path(ind_folder, paste0("ind", i, ".species"))
    create_speciesfile(file = species_file, species$structure_file, species_name = "Fagus sylvatica")
    modify_speciesfile(new_params = mat[i,], param_fixed=parameters_fixed, file = species_file)
    
    # create command_file
    command_file <- file.path(ind_folder, "commandfile")
    create_commandfile(command_file, commandfile_options, species_file = paste0("ind", i, ".species"))
    
    # save runline
    if(!is.null(capsis_settings$cd_java8)){
      run <- paste(capsis_settings$cd_java8, capsis_settings$java8, 
                   capsis_settings$cd, paste(capsis_settings$forceeps_run, command_file), sep=' && ')
    }else{
      run <- paste(capsis_settings$cd, paste(capsis_settings$forceeps_run, command_file), sep=' && ')
    }
    runlines <- c(runlines, run)
    
  }
  
  # run model
  with_progress(system.time(run_model(runlines, ncores = ncores)))
  
}
