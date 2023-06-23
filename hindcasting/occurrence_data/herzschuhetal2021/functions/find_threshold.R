
find_threshold <- function(years, window, field, dataset_count, factor = 0.05){
  
  cat("Computing threshold...\n")
  
  maxprop <- 0
  
  for(year in years){
    
    # sum by site within a time interval
    x1 <- (year-window)/1000 
    x2 <- (year+window)/1000 
    y1 <- dataset_count$`Age min [ka]`
    y2 <- dataset_count$`Age max [ka]`
    data_yr <- dataset_count %>%
      dplyr::filter(x1 <= y2 & y1 <= x2) %>%
      dplyr::select(3, 6:7, 15:223) %>%
      dplyr::group_by(`ID (Site)`, Longitude, Latitude) %>% 
      dplyr::summarise(across(everything(), sum), .groups = "drop_last")
    
    # compute proportion
    data_yr$pollen_count <- rowSums(data_yr[,4:212])
    data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
    species_prop <- values(data_yr_pts[,field])/values(data_yr_pts[,"pollen_count"])
    
    cat(paste0("Year ", year,", max proportion = ", max(species_prop),"\n"))
    
    maxprop <- ifelse(max(species_prop) > maxprop, max(species_prop), maxprop)
    
  }
  
  threshold <- factor*maxprop 
  
  cat(paste0("Threshold = ", threshold,"\n"))
  
  return(threshold)
}


# Quercus particular case
find_threshold_quercus <- function(years, window, field, dataset_count, factor = 0.05){
  
  cat("Computing threshold...\n")
  
  maxprop <- 0
  
  for(year in years){
    
    # sum by site within a time interval
    x1 <- (year-window)
    x2 <- (year+window)
    y1 <- dataset_count$minAgeBP
    y2 <- dataset_count$maxAgeBP
    
    data_yr <- dataset_count %>%
      dplyr::filter(x1 <= y2 & y1 <= x2) %>%
      dplyr::select(1, 4:5, 12:221) %>%
      dplyr::group_by(siteid, long, lat) %>% 
      dplyr::summarise(across(everything(), sum), .groups = "drop_last")
    
    
    # compute proportion
    data_yr$pollen_count <- rowSums(data_yr[,4:212])
    data_yr_pts <- vect(data_yr, geom=c("long", "lat"))
    species_prop <- values(data_yr_pts[,field])/values(data_yr_pts[,"pollen_count"])
    
    cat(paste0("Year ", year,", max proportion = ", max(species_prop),"\n"))
    
    maxprop <- ifelse(max(species_prop) > maxprop, max(species_prop), maxprop)
    
  }
  
  threshold <- factor*maxprop 
  
  cat(paste0("Threshold = ", threshold,"\n"))
  
  return(threshold)
}
