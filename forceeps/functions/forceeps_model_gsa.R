
forceeps_model_gsa <- function(mat){
  
  print(mat)
  runlines <- c()
  # create files 
  for(i in 1:nrow(mat)){
    ind_folder <- file.path(wd, "sensitivity_analysis", "process_files5", paste0("ind",i))
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
  with_progress(system.time(run_model(runlines, ncores = 20)))
  
  pattern <- paste0(site, ".siteproductivity.txt$")
  files <- list.files(path = output_path, pattern = pattern, recursive = T, full.names = TRUE)
  
  cnames <- c("date", "patchId", "speciesId", "speciesShortName", "adultProdBasalArea", "adultProdBiomass", "adultTreeBasalArea", "adultTreeBiomass", 
              "deadBasalArea", "deadBiomass", "saplingBasalArea", "saplingBiomass", "adultTreeNumber", "deadNumber", "saplingNumber", "droughtIndexAnnual",
              "droughtIndexSeasonal")
  
  nyears <- commandfile_options$numberOfYears
  
  yearly_results <- vroom(files, show_col_types = FALSE, progress=FALSE, skip = 9, col_names=cnames) %>%
    dplyr::select(var)
  
  mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), 
                                                                each = nyears, len = nrow(yearly_results))), mean)[-1]
  
  print(unname(unlist(mean_results)))
  return(unname(unlist(mean_results)))
  
}







forceeps_model_gsa_matrix <- function(mat){
  
  print(mat)
  runlines <- c()
  # create files 
  for(i in 1:nrow(mat)){
    ind_folder <- file.path(wd, "sensitivity_analysis", "process_files5", paste0("ind",i))
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
  with_progress(system.time(run_model(runlines, ncores = 20)))
  
  pattern <- "\\.siteproductivity.txt$"
  files <- list.files(path = output_path, pattern = pattern, recursive = T, full.names = TRUE)
  
  print(length(files))
  
  cnames <- c("date", "patchId", "speciesId", "speciesShortName", "adultProdBasalArea", "adultProdBiomass", "adultTreeBasalArea", "adultTreeBiomass", 
              "deadBasalArea", "deadBiomass", "saplingBasalArea", "saplingBiomass", "adultTreeNumber", "deadNumber", "saplingNumber", "droughtIndexAnnual",
              "droughtIndexSeasonal")
  
  nyears <- commandfile_options$numberOfYears
  
  yearly_results <- vroom(files, show_col_types = FALSE, progress=FALSE, skip = 9, col_names=cnames) %>%
    dplyr::select(var)
  
  mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), 
                                                     each = nyears, len = nrow(yearly_results))), mean)[-1]
  
  return(t(matrix(unlist(mean_results), ncol=length(commandfile_options$grid), byrow = T)))
  
}


forceeps_model_gsa_auc<- function(mat, ncores = 20){
  
  print(mat)
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
  
  pattern <- "\\.siteproductivity.txt$"
  files <- list.files(path = output_path, pattern = pattern, recursive = T, full.names = TRUE)
  
  print(length(files))
  
  cnames <- c("date", "patchId", "speciesId", "speciesShortName", "adultProdBasalArea", "adultProdBiomass", "adultTreeBasalArea", "adultTreeBiomass", 
              "deadBasalArea", "deadBiomass", "saplingBasalArea", "saplingBiomass", "adultTreeNumber", "deadNumber", "saplingNumber", "droughtIndexAnnual",
              "droughtIndexSeasonal")
  
  nyears <- commandfile_options$numberOfYears
  
  yearly_results <- vroom(files, show_col_types = FALSE, progress=FALSE, skip = 9, col_names=cnames) %>%
    dplyr::select(var)
  
  mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), 
                                                     each = nyears, len = nrow(yearly_results))), mean)[-1]
  
  mean_results <- matrix(unlist(mean_results), ncol=length(commandfile_options$grid), byrow = T)
  
  obs <- as.factor(Yobs$pres)
  auc_pred <- sapply(1:nrow(mean_results), FUN= function(x){
    pred <- mean_results[x,]
    roc_pred <- roc(pred, obs)
    auc(roc_pred)
  })
    
  return(auc_pred)
  
}





