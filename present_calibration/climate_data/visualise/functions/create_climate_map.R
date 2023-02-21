library(colorspace)

create_climate_map <- function(var, year, days, pd_folder, clim_name = "ERA5LAND_", citsw = FALSE, method=NULL){
  
  if(!is.null(method)){
    pd_folder <- paste0(pd_folder, "pet_", method, "/")
  }
  
  clim_file <- paste0(pd_folder, clim_name , var, "_", year, "_dly.fit")
  clim <- fread(clim_file, showProgress=F)
  colnames(clim) <- c("lat", "lon", paste0("Day ", as.character(1:(ncol(clim)-2))))
  clim$lat <- round(clim$lat, 1)
  clim$lon <- round(clim$lon, 1)
  
  days <- days +2
  col <-  c(1,2, days)
  clim <- clim[, ..col]
  colnames(clim)[1:2] <- c("lat", "lon")
  min_val <- min(clim[,-c("lat", "lon")])
  max_val <- max(clim[,-c("lat", "lon")])
  
  clim_temp <- melt(clim[, -c("lat", "lon")])
  clim <- cbind(clim[, c("lat", "lon")], clim_temp)
  
  cities <- data.frame(lat = c(43.6,37.4,50.5,59.9), lon = c(3.8,-5.9,30.5,10.7))
  
  clim_map <- ggplot(data=clim, aes(x = lon, y = lat)) + 
    geom_tile(aes(fill = value)) +
    facet_wrap(~variable) +
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank()) +
    ylab("") +
    xlab("") +
    scale_fill_continuous_divergingx(palette = 'Spectral', rev=TRUE, mid = 10, limits=c(min_val,max_val))
  
  
  if(var == "tmp"){
    clim_map <- clim_map +
      ggtitle(paste("Mean temperature (°C)"))
  }else if(var =="glo"){
    clim_map <- clim_map +
      ggtitle(paste("Global radiation (MJ/m²)"))
  }else if(var =="pre"){
    clim_map <- clim_map +
      ggtitle(paste("Precipitation (mm)"))
  }else if(var =="pet" & !is.null(method)){
    clim_map <- clim_map +
      ggtitle(paste("Potential evaporation (mm) - Penman Monteith"))
  }else if(var =="pet" & is.null(method)){
    clim_map <- clim_map +
      ggtitle(paste("Potential evaporation (mm) - ERA5-Land"))
  }
  
  if(citsw){
    clim_map <- clim_map + geom_point(data = cities, aes(x = lon, y = lat))
  }
  
  
  return(clim_map)  

}