
find_threshold <- function(years, window, field, dataset_count, factor = 0.05){
  
  cat("Computing threshold...\n")
  
  maxprop <- 0
  
  for(year in years){
    
    # mean by site within a time interval, weighted by time distance and incertitude
    x1 <- (year-window)/1000 
    x2 <- (year+window)/1000 
    y1 <- dataset_count$`minimum_Age (cal. ka BP)`
    y2 <- dataset_count$`maximum_Age (cal. ka BP)`
    
    dist <- abs(dataset_count$`median_Age (cal. ka BP)`-year/1000)
    incert <- y2-y1
    
    # scale both weight variables (so they have the same importance) - between 0.001 and 1.001 (no zero weight)
    dataset_count$dist_w <- 1.001-((dist-min(dist))/(max(dist)-min(dist)))
    dataset_count$incert_w <- 1.001-((incert-min(incert))/(max(incert)-min(incert)))
    
    data_yr <- dataset_count %>%
      dplyr::filter(x1 <= y2 & y1 <= x2) %>%
      dplyr::select(3, 6:7, 15:226) %>%
      dplyr::group_by(Site_ID, `Longitude (DD)`, `Latitude (DD)`) %>% 
      dplyr::summarise(across(1:208, list(wmean = ~ weighted.mean(., w = incert_w*dist_w))), .groups = "drop_last")
    
    # compute proportion
    data_yr$pollen_count <- rowSums(data_yr[,4:211])
    data_yr_pts <- vect(data_yr, geom=c("Longitude (DD)", "Latitude (DD)"))
    species_prop <- values(data_yr_pts[,paste0(field, "_wmean")])/values(data_yr_pts[,"pollen_count"])
    
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
