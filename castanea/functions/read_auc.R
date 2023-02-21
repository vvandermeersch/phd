library(AUC)

read_auc <- function(inds, ncores){
  
  if(ncores > 1){list_sim <- split(inds, cut(seq_along(1:length(inds)), ncores, labels = FALSE))
  }else{list_sim <- list(1:length(inds))}
  
  plan(multisession, workers = ncores)
  handlers("progress")
  prog <- progressor(length(inds))
  
  auc_results <- future_sapply(1:ncores, function(i){
    auc_results_i <- c()
    for(j in list_sim[[i]]){
      dir_temp <- "D:/applications/capsis4/var"
      pattern <- paste0("\\_ind", j, "_yearlyResults.log$")
      files_temp <- list.files(path = dir_temp, pattern = pattern, recursive = T, full.names = TRUE)
      yearly_results <- vroom(files_temp, show_col_types = FALSE, progress=FALSE, skip = 0, delim = ';') 
      mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), 
                                                         each = nyears, len = nrow(yearly_results))), mean)[-1]
      roc_pred <- roc(t(mean_results), obs)
      auc_results_i <- c(auc_results_i, auc(roc_pred))
      
      prog()
    }
    return(auc_results_i)
  })
  plan(sequential)
  gc()
  
  return(auc_results)
  
}
