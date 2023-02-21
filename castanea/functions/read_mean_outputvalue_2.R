library(vroom)

read_mean_outputvalue_2 <- function(grid, output_folder = 'D:/applications/capsis4/var', var = "BiomassOfReserves"){
  
  lambda <- 1
  
  len <- length(grid)/20
  grid_list <- split(grid, ceiling(seq_along(grid)/len))
  
  temp <- lapply(grid_list, function(gridl){
    
    filenames <- sapply(gridl, function(i){
      ind <- paste0("ind", lambda, "_")
      file.path(output_folder, paste0(i, "_", ind, "yearlyResults.log"))})
    
    yearly_results <- vroom(filenames, col_select = all_of(var), show_col_types = FALSE, progress=FALSE)
    
    if(nrow(yearly_results) == 0){
      yearly_results[1:(length(gridl)*31), var] <- NA
    }
      
    nyears <- nrow(yearly_results)/length(gridl)
    mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), each = nyears, len = nrow(yearly_results))), mean)[-1]
    
    rm(yearly_results)
    
    gc()
    
    return(mean_results)
    
  })
  
  output <- matrix(unlist(temp), ncol=length(grid), nrow=lambda, byrow=TRUE)
  
  return(output)
  
}
