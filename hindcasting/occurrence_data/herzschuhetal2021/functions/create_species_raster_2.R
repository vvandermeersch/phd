
create_species_raster <- function(year, window, threshold,
                                  field, dataset_count,  
                                  grid, conservative = TRUE){
  
  cat(paste0("Year ", year,"\n"))
  
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
  
  if(conservative){
    # conservative approach - following Fitzpatrick et al. 2018
    # If multiple sites fell within the same grid cell, their pollen abundances are averaged 
    data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
    species_count <- rasterize(data_yr_pts, grid, field = field, fun = "mean") 
    data_yr$pollen_count <- rowSums(data_yr[,4:212])
    data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
    all_count <- rasterize(data_yr_pts, grid, field = "pollen_count", fun = "mean")
    
    # create proportion raster
    species_prop <- species_count/all_count
    
    # threshold scaled to 5% of the maximum abundance - following Nieto-Lugilde et al. (2015), Maguire et al. (2016)
    # threshold <- as.numeric(global(species_prop, fun = "max", na.rm = T))*0.05
    m <- c(0, threshold, 0,
           threshold, 1, 1)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    
    # binarize proportion raster
    species_pres <- classify(species_prop, rclmat, include.lowest=TRUE)
  }else{
    # non-restrictive approach
    # the species is considered present in the cell if there is at least one site >threshold
    data_yr$pollen_count <- rowSums(data_yr[,4:212])
    data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
    
    # compute proportion
    species_prop <- values(data_yr_pts[,field])/values(data_yr_pts[,"pollen_count"])
    
    # threshold scaled to 5% of the maximum abundance - following Nieto-Lugilde et al. (2015), Maguire et al. (2016)
    # threshold <- max(species_prop)*0.05
    data_yr_pts$pres <- as.numeric(species_prop>=threshold)
    
    species_pres <-  rasterize(data_yr_pts, grid, field = "pres", fun = "max")
    
  }
  
  cat(paste0("   Nb of presence: ", nrow(species_pres[species_pres==1]),"\n"))
  
  return(species_pres)
}

create_species_raster_qilex <- function(year, field = 'Quercus.ilex', dataset_count, grid){
  
  # sum by site within a time interval
  data_yr <- dataset_count[dataset_count$minAgeBP <= year & dataset_count$maxAgeBP >= year,] %>%
    dplyr::select(2, 4:5, 12:221) %>%
    group_by(datasetid, long, lat) %>% 
    summarise(across(everything(), sum))
  
  # If multiple sites fell within the same grid cell, their pollen abundances are averaged - following Fitzpatrick et al. 2018
  data_yr_pts <- vect(data_yr, geom=c("long", "lat"))
  species_count <- rasterize(data_yr_pts, grid, field = field, fun = "mean") 
  data_yr$pollen_count <- rowSums(data_yr[,4:212])
  data_yr_pts <- vect(data_yr, geom=c("long", "lat"))
  all_count <- rasterize(data_yr_pts, grid, field = "pollen_count", fun = "mean")
  
  # create proportion raster
  species_prop <- species_count/all_count
  
  # threshold scaled to 5% of the maximum abundance - following Nieto-Lugilde et al. (2015), Maguire et al. (2016)
  # threshold <- as.numeric(global(species_prop, fun = "max", na.rm = T))*0.05
  m <- c(0, threshold, 0,
         threshold, 1, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  # binarize proportion raster
  species_pres <- classify(species_prop, rclmat, include.lowest=TRUE)
  
  return(species_pres)
}
