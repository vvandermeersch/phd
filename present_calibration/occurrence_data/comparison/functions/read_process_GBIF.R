library(sf)

read_process_GBIF <- function(shp, filter=NULL){
  shp <- shp[shp$basisOfRecord != "PRESERVED_SPECIMEN",]
  shp <- data.frame(country = shp$countryCode, 
                               lat = shp$decimalLatitude, lon = shp$decimalLongitude,
                               species=shp$species, issue = shp$issue)
  shp <- shp[!is.na(shp$lon),]
  shp_sf <- st_as_sf(shp, coords = c("lon","lat"), crs=st_crs(4326))
  
  if(length(filter) != 0){
    shp_sf <- shp_sf[st_intersects(shp_sf, filter) %>% lengths > 0,] # faster than st_intersection with points
  }
  
  return(shp_sf)
}