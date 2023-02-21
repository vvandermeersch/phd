

diag_climate<- function(grid, climate_folder = 'D:/climate/ERA5-Land/castanea_format', var = "tj"){
  
  output <- matrix(ncol=length(grid), nrow=lambda, byrow=TRUE)
  
  filenames <- sapply(grid, function(i){
    file.path(output_folder, paste0(i, ".txt"))})
  
  climate_results <- vroom(filenames, col_select = c(var), show_col_types = FALSE, progress=FALSE)
  
  nyears <- nrow(yearly_results)/length(grid)
  
  mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), each = nyears, len = nrow(yearly_results))), mean)[-1]
  
  output[l,] <- t(mean_results)
  
  
  
  return(output)
}