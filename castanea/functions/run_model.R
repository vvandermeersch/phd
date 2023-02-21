
run_model <- function(runlines, ncores, log){
  if(ncores>nrow(runlines)){stop("Number of cores must be less than or equal to the number of simulations !")}
  plan(multisession, workers = ncores)
  #handlers("progress")
  #prog <- progressor(nrow(runlines))
  #prog(message = paste("Starting", nrow(runlines), "simulations on", ncores, "core(s)"), class = "sticky", amount = 0)
  line <- paste("Starting", nrow(runlines), "simulations on", ncores, "core(s)")
  write(line,file=log,append=T)
  if(ncores == 1){
    list_lines <- list(1:nrow(runlines))
  }else{
    list_lines <- split(1:nrow(runlines), cut(seq_along(1:nrow(runlines)), ncores, labels = FALSE))
  }
  out <- future_lapply(1:ncores, function(i){
    for(j in unlist(list_lines[i])){
      shell(runlines[j,], intern=T)
      line <- paste("Simulation", j, "on core", i, "done !")
      write(line,file=log,append=TRUE)
      #prog()
    }
  })
  plan(sequential)
  gc()
}


run_model_from_mat <- function(mat, ncores, log){

  line <- paste(Sys.time())
  write(line,file=log,append=F)
  line <- "-------------------------------------"
  write(line,file=log,append=T)
  
  runlines <- c()
  
  # create files 
  for(i in 1:nrow(mat)){
    ind_folder <- file.path(output_path, paste0("ind",i))
    dir.create(ind_folder, showWarnings = FALSE)
    
    # create species_file
    species_file <- file.path(ind_folder, paste0("ind_", i, ".txt"))
    create_speciesfile(file = species_file, species$structure_file, species_name = species$name)
    modify_speciesfile(new_params = mat[i,], param_fixed=parameters_fixed, file = species_file)
    
  }
  
  species_file_list <-  sapply(1:nrow(mat), function(x){paste0("ind_", x, ".txt")})
  
  create_runfile(capsis_settings$nb_lines_per_file, output_dir = output_path, species_file = species_file_list, 
                 sim_options = sim_options, data = data , capsis_settings = capsis_settings)
  
  runlines <- read.table(file.path(output_path,"runfile.txt"), sep='\t')
  
  # run model
  with_progress(system.time(run_model(runlines, ncores = ncores, log = log)))
  
}
