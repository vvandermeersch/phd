# Function not used

# Load NC file as raster reference
raster_file <- paste0("surface_solar_radiation_downwards", "_", 2000, "_", "06", ".nc")
raster_ref <- brick(paste0(rd_folder, raster_file), varname="rsds_accumulated")


create_raster <- function(var, year, day, raster_ref=raster_ref){
  
  clim_file <- paste0(processeddata_folder, "ERA5LAND_", var, "_", year, "_dly.fit")
  clim <- fread(clim_file, showProgress=F)
  colnames(clim) <- c("lat", "lon", paste0("day", as.character(1:(ncol(clim)-2))))
  clim$lat <- round(clim$lat, 1)
  clim$lon <- round(clim$lon, 1)
  
  # make the SpatialPointsDataFrame object
  coords <- clim[ , c("lon", "lat")]   # coordinates
  data   <- clim[ , -c(1,2)]          # data
  crs    <- crs(raster_ref) # proj4string of coords
  spdf <- SpatialPointsDataFrame(coords = coords,
                                 data = data, 
                                 proj4string = crs)
  names(spdf) <- as.character(1:ncol(spdf))
  
  clim_raster <- rasterize(spdf, raster_ref, field = as.character(day), fun='last')
  
  
  
  
}