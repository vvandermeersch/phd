

diag_climate<- function(grid, climate_folder = 'D:/climate/ERA5-Land/castanea_format', var = "tj"){
  
  filenames <- sapply(grid, function(i){
    file.path(climate_folder, paste0(i, ".txt"))})
  
  climate_values <- vroom(filenames, col_select = c(var), show_col_types = FALSE, progress=FALSE, skip=12, n_max = 365)
  
  mean_values <- aggregate(climate_values, list(rep(1:(nrow(climate_values) %/% 365 + 1), each = 365, len = nrow(climate_values))), mean)[-1]

  return(mean_values)
}