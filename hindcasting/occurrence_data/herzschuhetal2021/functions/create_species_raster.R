
create_species_raster <- function(year, field, dataset_count, grid){
  
  # sum by site within a time interval
  data_yr <- dataset_count[dataset_count$`Age min [ka]` <= year/1000 & dataset_count$`Age max [ka]` >= year/1000,] %>%
    dplyr::select(4, 6:7, 15:223) %>%
    group_by(`ID (Dataset)`, Longitude, Latitude) %>% 
    summarise(across(everything(), sum))
  
  # If multiple sites fell within the same grid cell, their pollen abundances are averaged - following Fitzpatrick et al. 2018
  data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
  species_count <- rasterize(data_yr_pts, grid, field = field, fun = "mean") 
  data_yr$pollen_count <- rowSums(data_yr[,4:212])
  data_yr_pts <- vect(data_yr, geom=c("Longitude", "Latitude"))
  all_count <- rasterize(data_yr_pts, grid, field = "pollen_count", fun = "mean")
  
  # create proportion raster
  species_prop <- species_count/all_count
  
  # threshold scaled to 5% of the maximum abundance - following Nieto-Lugilde et al. (2015), Maguire et al. (2016)
  threshold <- as.numeric(global(species_prop, fun = "max", na.rm = T))*0.05
  m <- c(0, threshold, 0,
         threshold, 1, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  # binarize proportion raster
  species_pres <- classify(species_prop, rclmat, include.lowest=TRUE)
  
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
  threshold <- as.numeric(global(species_prop, fun = "max", na.rm = T))*0.05
  m <- c(0, threshold, 0,
         threshold, 1, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  # binarize proportion raster
  species_pres <- classify(species_prop, rclmat, include.lowest=TRUE)
  
  return(species_pres)
}
