source(file.path(wd, "functions", "create_commandfiles.R"))

#nb_lines : number of lines in the runfile

# capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd", 
#                      cd ="cd/d D:/applications/capsis4 && setmem 30000",
#                      castanea_run = "capsis -clfd -p script castaneaonly.myscripts.Marge_EU", nb_lines)

create_runfile <- function(nb_lines_per_file, output_dir, sim_options, species_file, data, capsis_settings, start = 1){
  
  for(i in 1:length(species_file)){
    
    commandfiles_dir <- file.path(output_dir, paste0("ind", i + (start - 1)))
    dir.create(commandfiles_dir, showWarnings = FALSE)
    
    create_commandfiles(nb_lines_per_file, commandfiles_dir, sim_options, sim_name = paste0("ind", i + (start - 1), "_"),  species_file[i], data)
    
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
