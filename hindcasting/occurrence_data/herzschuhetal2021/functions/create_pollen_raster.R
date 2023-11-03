

create_pollen_raster <- function(year, window, 
                                 field, dataset_count,  
                                 grid,
                                 method = "average",
                                 threshold = NULL){
  
  cat(paste0("Year ", year,"\n"))
  
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
    dplyr::select(3, 6:7, 15:length(dataset_count)) %>%
    dplyr::group_by(Site_ID, `Longitude (DD)`, `Latitude (DD)`) %>% 
    dplyr::summarise(across(ends_with("(#)"), list(wmean = ~ weighted.mean(., w = incert_w*dist_w))), .groups = "drop_last")
  
  data_yr$pollen_count <- rowSums(data_yr[,4:length(data_yr)])
  data_yr$relative_abundance <- unlist(data_yr[,paste0(field, "_wmean")])/data_yr$pollen_count
  
  if(method == "average"){
    
    # If multiple sites fell within the same grid cell, their species pollen proportion are averaged 
    data_yr_pts <- vect(data_yr, geom=c("Longitude (DD)", "Latitude (DD)"))
    species_raster <- rasterize(data_yr_pts, grid, field = "relative_abundance", fun = "mean")
    
    cat(paste0("   Mean pollen proportion: ", round(as.numeric(global(species_raster, mean, na.rm = T)), 3),"\n"))
    
  }else if(method == "one_is_enough"){
    
    # If at least one site has a rel. abundance >= threshold, we consider the species present in the cell
    data_yr_pts <- vect(data_yr, geom=c("Longitude (DD)", "Latitude (DD)"))
    data_yr_pts$pres <- as.numeric(data_yr_pts$relative_abundance>=threshold)
    
    species_raster <- rasterize(data_yr_pts, grid, field = "pres", fun = "max")
    
    # records outside grid (coastal cells) - circle interpolation and keep only the nearest cell
    t <- try({
      rec_outside <- as.points(mask(species_raster, grid, inverse = TRUE)) 
      rec_outside_buffer <- mask(rasterizeWin(rec_outside, grid, field = "pres_max", win = "circle", pars = 0.25, fun = "max"), 
                                 grid)
      nearest_cell <- class::knn1(crds(rec_outside_buffer, na.rm = T), crds(rec_outside), factor(seq_len(nrow(crds(rec_outside_buffer, na.rm = T)))))
      rec_out_pts <- vect(crds(rec_outside_buffer, na.rm = T)[nearest_cell,])
      rec_out_pts$pres <- rec_outside$pres_max
      rec_out_raster <- rasterize(rec_out_pts, grid, field = "pres", fun = "max")
    },silent = TRUE)
    if(inherits(t, "try-error")){
      rec_out_raster <- species_raster
      values(rec_out_raster) <- NA
    }

  
    species_raster <- mask(species_raster, grid)
    cat(paste0("   Number of presence inside grid: ", length(species_raster[species_raster==1]),"\n"))
    cat(paste0("   Number of presence near coastal cells: ", length(rec_out_raster[rec_out_raster==1]),"\n"))
    
    species_raster <- max(species_raster, rec_out_raster, na.rm = T)
  }
  
  return(species_raster)
}
