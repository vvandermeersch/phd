suppressPackageStartupMessages(library(AUC))

auc_of <- function(x, Yobs, params, grid, ind_paths, runlines){
  
  # update species_file
  for(i in 1:length(ind_paths)){
    x_inv <- inv_linearscaling(x[,i], params$lb[!(names(params$ub) %in% params$fixed)], 
                               params$ub[!(names(params$ub) %in% params$fixed)],
                               params$scale_factor)
    
    modify_speciesfile(x_inv, params$fixed, paste0(ind_paths[i], "/", i, ".species"))
  }
  
  # Run Capsis simulations
  temp <- future_lapply(runlines, function(x){
    for(i in 1:length(x)){
      shell(x[i], intern=T)
      #out <- system(x, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    
    
  })
  
  
  # Read fitness and mean between two years
  Ysim <- read_mean_outputvalue(grid, ind_paths, var = "adultTreeBasalArea")
  
  # Compute AUC
  obs <- as.factor(Yobs$pres)
  auc_pred <- apply(Ysim, 1, FUN= function(x){
    x[is.na(x)] <- 0 # NA fitness values set to zero
    roc_pred <- roc(x, obs)
    auc(roc_pred)
  })
  
  return(1-auc_pred)
}
