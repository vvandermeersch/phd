create_chrono_maps <- function(pollen_data, macrofossil_data, other_data, years, ice_frac = F){
  
  p <- list()
  
  if(ice_frac){
    hadcm3b_dir <- "D:/climate/HadCM3B_60Kyr_Climate/raw/IceFrac"
    file_prefix <- "regrid_icefrac_"
    eu <- as(extent(-14, 40, 34, 72), 'SpatialPolygons')
  }
  
  
  for(i in 1:length(years)){
    
    plot <- ggplot() +
      geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey', size = 0.1) +
      geom_point(data = pollen_data[pollen_data$time == years[i],], aes(x = long, y = lat), col = "orange", shape = 8) +
      geom_point(data = macrofossil_data[macrofossil_data$time == years[i],], aes(x = long, y = lat), col = "brown", shape = 18) +
      geom_point(data = other_data[other_data$time == years[i],], aes(x = long, y = lat), col = "darkgreen", shape = 20) +
      theme_void() +
      annotate("text", x = -4, y = 65, label = paste(years[i], "BP"), colour = 'darkgrey', size = 3) +
      ylab("") +
      xlab("")
    
    if(ice_frac){
      file <- find_file_name(years[i])
      r <- stack(file.path(hadcm3b_dir, paste0(file_prefix, file$name, "kyr.nc")))
      rsub <- subset(r, file$yr_pos)
      crs(eu) <- crs(rsub)
      rsub <- crop(rsub, eu)
      dfsub <- as.data.frame(rsub, xy = T)
      names(dfsub) <- c("x", "y", "ice")
      dfsub[dfsub$ice ==0,] <- NA # remove no-ice tiles
      plot <- plot + geom_raster(data = dfsub, aes(x=x, y= y, fill = ice), na.rm = T, alpha = 0.4) + theme(legend.position="none")
      
    }
    
    p[[i]] <- plot
    
  }
  
  do.call(plot_grid,p)
  
}