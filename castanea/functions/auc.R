suppressPackageStartupMessages(library(AUC))

auc_of <- function(x, Yobs, lambda, params, grid, species_file_paths, run_file_paths){
  
  # update species_file
  for(i in 1:lambda){
    x_inv <- inv_linearscaling(x[,i], params$lb[!(names(params$ub) %in% params$fixed)], 
                               params$ub[!(names(params$ub) %in% params$fixed)],
                               params$scale_factor)
    
    modify_speciesfile(x_inv, params$fixed, species_file_paths[i])
  }
  
  
  # Run Capsis simulations
  temp <- future_lapply(run_file_paths, function(x){
    runlines <- read.table(x, sep='\t')
    for(i in 1:nrow(runlines)){
      
      shell(runlines[i,], intern=T)
      #out <- system(x, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    
    
  })
  
  
  # Read fitness and mean between two years
  Ysim <- read_mean_outputvalue(grid, lambda, output_folder = 'D:/applications/capsis4/var', var = "BiomassOfReserves")
  
  # Compute AUC
  obs <- as.factor(Yobs$pres)
  auc_pred <- apply(Ysim, 1, FUN= function(x){
    x[is.na(x)] <- 0 # NA fitness values set to zero
    roc_pred <- roc(x, obs)
    auc(roc_pred)
  })
  
  return(1-auc_pred)
}
