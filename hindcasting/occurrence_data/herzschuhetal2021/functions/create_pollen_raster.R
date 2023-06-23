

create_pollen_raster <- function(year, window, 
                                 field, dataset_count,  
                                 grid,
                                 method = "average",
                                 threshold = NULL){
  
  cat(paste0("Year ", year,"\n"))
  
  # mean by site within a time interval, weighted by time distance and incertitude
  x1 <- (year-window)/1000 
  x2 <- (year+window)/1000 
  y1 <- dataset_count$`Age min [ka]`
  y2 <- dataset_count$`Age max [ka]`
  
  dist <- abs(dataset_count$`Age [ka BP] (median)`-year/1000)
  incert <- y2-y1
  
  # scale both weight variables (so they have the same importance)
  dataset_count$dist_w <- 1-((dist-min(dist))/(max(dist)-min(dist)))
  dataset_count$incert_w <- 1-((incert-min(incert))/(max(incert)-min(incert)))
  
  data_yr <- dataset_count %>%
    dplyr::filter(x1 <= y2 & y1 <= x2) %>%
    dplyr::select(3, 6:7, 15:227) %>%
    dplyr::group_by(`ID (Site)`, Longitude, Latitude) %>% 
    dplyr::summarise(across(1:209, list(wmean = ~ weighted.mean(., w = incert_w*dist_w))), .groups = "drop_last")
  
  data_yr$pollen_count <- rowSums(data_yr[,4:212])
  data_yr$relative_abundance <- unlist(data_yr[,paste0(field, "_wmean")])/data_yr$pollen_count
  
  if(method == "average"){
    
    # If multiple sites fell within the same grid cell, their species pollen proportion are averaged 
    data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
    species_raster <- rasterize(data_yr_pts, grid, field = "relative_abundance", fun = "mean")
    
    cat(paste0("   Mean pollen proportion: ", round(as.numeric(global(species_raster, mean, na.rm = T)), 3),"\n"))
    
  }else if(method == "one_is_enough"){
    
    # If at least one site has a rel. abundance > threshold, we consider the species present in the cell
    data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
    data_yr_pts$pres <- as.numeric(data_yr_pts$relative_abundance>threshold)
    
    species_raster <- rasterize(data_yr_pts, grid, field = "pres", fun = "max")
    
    cat(paste0("   Number of presence: ", length(species_raster[species_raster==1]),"\n"))
    
  }
  
  return(species_raster)
}



# Quercus: particular case
create_pollen_raster_quercus <- function(year, window, 
                                 field, dataset_count,  
                                 grid,
                                 method = "average",
                                 threshold = NULL){
  
  cat(paste0("Year ", year,"\n"))
  
  # mean by site within a time interval, weighted by time distance and incertitude
  x1 <- (year-window)
  x2 <- (year+window)
  y1 <- dataset_count$minAgeBP
  y2 <- dataset_count$maxAgeBP
  
  dist <- abs(dataset_count$medianAgeBP-year)
  incert <- y2-y1
  
  # scale both weight variables (so they have the same importance)
  dataset_count$dist_w <- 1-((dist-min(dist))/(max(dist)-min(dist)))
  dataset_count$incert_w <- 1-((incert-min(incert))/(max(incert)-min(incert)))
  
  data_yr <- dataset_count %>%
    dplyr::filter(x1 <= y2 & y1 <= x2) %>%
    dplyr::select(1, 4:5, 12:225) %>%
    dplyr::group_by(siteid, long, lat) %>% 
    dplyr::summarise(across(1:210, list(wmean = ~ weighted.mean(., w = incert_w*dist_w))), .groups = "drop_last")
  
  data_yr$pollen_count <- rowSums(data_yr[,4:213])
  data_yr$relative_abundance <- unlist(data_yr[,paste0(field, "_wmean")])/data_yr$pollen_count
  
  if(method == "average"){
    
    # If multiple sites fell within the same grid cell, their species pollen proportion are averaged 
    data_yr_pts <- vect(data_yr, geom=c("long", "lat"))
    species_raster <- rasterize(data_yr_pts, grid, field = "relative_abundance", fun = "mean")
    
    cat(paste0("   Mean pollen proportion: ", round(as.numeric(global(species_raster, mean, na.rm = T)), 3),"\n"))
    
  }else if(method == "one_is_enough"){
    
    # If at least one site has a rel. abundance > threshold, we consider the species present in the cell
    data_yr_pts <- vect(data_yr, geom=c("long", "lat"))
    data_yr_pts$pres <- as.numeric(data_yr_pts$relative_abundance>threshold)
    
    species_raster <- rasterize(data_yr_pts, grid, field = "pres", fun = "max")
    
    cat(paste0("   Number of presence: ", length(species_raster[species_raster==1]),"\n"))
    
  }
  
  return(species_raster)
}



