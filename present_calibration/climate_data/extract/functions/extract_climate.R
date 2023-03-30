

extract_climate <- function(species_data, climate_data){
  
  climate_ext <- inner_join(species_data, climate_data, by = c("lat", "lon")) #species_data first to keep the same order (important for AUC computation)
  
  return(climate_ext)
  
}


extract_all_climate <- function(climate_data, filter = NULL){
  
  climate_data$lat <- round(climate_data$lat, 1)
  climate_data$lon <- round(climate_data$lon, 1)
  
  if(!is.null(filter)){
    
    
    # keep only cell in filter
    climate_data <- climate_data %>% left_join(filter, by = c("lon", "lat")) %>% 
      drop_na(value) %>% 
      dplyr::select(-c("value"))
  }
  
  # to reduce file size
  climate_ext <- round(climate_data, 3)
  
  return(climate_ext)
  
}
