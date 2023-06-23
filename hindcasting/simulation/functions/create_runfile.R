
# Castanea

create_runfile <- function(nb_lines_per_file, output_dir, sim_options, species_file, data, capsis_settings, start = 1){
  
  for(i in 1:length(species_file)){
    
    commandfiles_dir <- file.path(output_dir, paste0("ind", i + (start - 1)))
    dir.create(commandfiles_dir, showWarnings = FALSE)
    
   .create_commandfiles(nb_lines_per_file, commandfiles_dir, sim_options, sim_name = paste0("ind", i + (start - 1), "_"),  species_file[i], data)
    
    run_file <- paste0(output_dir, "/runfile",  sim_options$command_file_suffix, ".txt")
    
    # if java8 change is needed
    if(!is.null(capsis_settings$cd_java8)){
      run <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd, capsis_settings$castanea_run, sep=' && ')
    }else{
      run <- paste(capsis_settings$cd, capsis_settings$castanea_run, sep=' && ')
    }
    
    if(i==1){
      header <- c("# Run file for Castanea", paste0('# Generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date()), "")
      write(header, run_file)
    }
    
    # number of lines per file
    nb_lines <- length(data$lat)
    if(nb_lines < nb_lines_per_file){
      stop("Total number of lines lower than number of lines per file you asked for !")
    }
    nb_files <- ceiling(nb_lines/nb_lines_per_file)
    
    for(i in 1:nb_files){
      command_file <- paste0(commandfiles_dir, "/commandfile",  sim_options$command_file_suffix, "_", i, ".txt")
      
      run_line <- paste(run, command_file)
      
      write(run_line, run_file,append=TRUE)
      
    }
    
  }
  
}

.create_commandfiles <- function(nb_lines_per_file, output_dir, sim_options, sim_name, species_file, data){
  
  header <- c("# Command file for Castanea", paste0('# Generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date()), "")
  
  filenames <- c("# The filenames below are relative to the folder containing this command file",
                 paste("castaneaFileName","=", species_file, sep="\t"),
                 paste("inventoryFileName","=", sim_options$inventory_file, sep="\t"),
                 "",
                 paste("climateBaseName","=", sim_options$climate_name, sep="\t"),
                 paste("climateExtension","=", sim_options$climate_ext, sep="\t"),
                 paste("simulationName","=", sim_name , sep="\t"),
                 "", "")
  
  colnames <- paste("#	siteId", "gridName", "latitude", "longitude", "soilHeight", "stone", "wfc", "wilt", "bulk", 
                    "SOLCLAYtop", "SOLCLAYsol", "SOLFINtop", "SOLFINsol", "SOLSANDtop", "SOLSANDsol", 
                    "numberOfYears", 
                    sep="\t")
  
  # number of lines per file
  nb_lines <- length(data$lat)
  if(nb_lines < nb_lines_per_file){
    stop("Total number of lines lower than number of lines per file you asked for !")
  }
  nb_files <- ceiling(nb_lines/nb_lines_per_file)
  
  if(nb_files == 1){
    list_lines <- list(1:nb_lines)
  }else{
    list_lines <- split(1:nb_lines, cut(seq_along(1:nb_lines), nb_files, labels = FALSE))
  }
  
  # loop on command files needed  
  for(i in 1:nb_files){
    
    site_id <- 1
    
    command_file <- paste0(output_dir, "/commandfile",  sim_options$command_file_suffix, "_", i, ".txt")
    
    write(header, command_file)
    write(filenames, command_file, append=TRUE)
    write(colnames, command_file, append=TRUE)
    
    # loop on lines to add to the files
    for(j in unlist(list_lines[i])){
      
      vals <- paste(site_id, format(data$grid[j], scientific = FALSE), data$lat[j], data$lon[j], data$depth[j], data$stone[j], data$wfc[j], data$wilt[j], data$bulk[j],
                    data$soil_prop$clay_top[j], data$soil_prop$clay_all[j], data$soil_prop$fin_top[j],
                    data$soil_prop$fin_all[j], data$soil_prop$sand_top[j], data$soil_prop$sand_all[j],
                    sim_options$nb_years,
                    sep="\t")

      write(vals, command_file,append=TRUE)
      
      site_id <- site_id + 1
    }
    
  }
  
}

