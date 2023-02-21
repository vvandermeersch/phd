# Load presence and background points
library(sf)
library(rnaturalearth)
library(tidyverse)

# load species presence records
load(file.path(sp_data_dir, sp_name, paste0(nb_pres, "pres_", nb_pres, "abs"), paste0("occurrence_subset_", subset, ".Rdata")))
sp_presence <- species_occurrence[species_occurrence$pres == 1,]


if(pseudo_abs){
  
  sp_absence <- species_occurrence[species_occurrence$pres == 0,]
  
  ## merge presence and pseudo-absence points
  pr_ab <- rbind(sp_presence, sp_absence)
  pr_ab$lat <- round(pr_ab$lat, 1)
  pr_ab$lon <- round(pr_ab$lon, 1)
  
}else{
  
  # add background data
  ## load all study points
  alt_file <- paste0("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_Altitude.fit")
  alt <- fread(alt_file, showProgress=F)
  colnames(alt) <- c("lat", "lon", "alt")
  alt$lat <- round(alt$lat, 1)
  alt$lon <- round(alt$lon, 1)
  ## keep only points in sampled area... (i.e. EuForest + GBIF filtered area)
  ERA5_points <- st_as_sf(alt, coords = c("lon", "lat"), crs = 4326) # all points
  ## sampled area
  countries_EUForest <- c("Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",
                          "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Moldova", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                          "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
  other_countries <- c("Ukraine", "Bosnia and Herzegovina", "Republic of Serbia", "Macedonia", "Greece",
                       "Kosovo", "Albania", "Montenegro")
  countries <- c(countries_EUForest, other_countries)
  world_map <- ne_countries(scale="medium",returnclass = 'sf')
  eu_map <- world_map %>% filter(sovereignt %in% countries)
  eu_map_cropped <- eu_map %>% 
    st_crop(st_bbox(c(xmin = -12, xmax = 40, ymax = 71, ymin = 34), crs = st_crs(4326))) %>% 
    dplyr::select(c(sovereignt))
  ## ERA5 points in sampled area
  ERA5_points_sampledarea <- ERA5_points[st_intersects(ERA5_points, eu_map_cropped) %>% lengths > 0,] 
  ERA5_points_sampledarea <- ERA5_points_sampledarea %>%
    mutate(lat = unlist(map(ERA5_points_sampledarea$geometry,2)),
           lon = unlist(map(ERA5_points_sampledarea$geometry,1))) %>%
    st_drop_geometry()
  ## sample 50000 background points
  bg_points <- sample(1:nrow(ERA5_points_sampledarea), nb_background)
  background <- ERA5_points_sampledarea[bg_points, -c("alt")]
  background$pres <- 0
  ## merge presence and background points
  pr_bg <- rbind(sp_presence, background)
  pr_bg$lat <- round(pr_bg$lat, 1)
  pr_bg$lon <- round(pr_bg$lon, 1)

}


# load all species presence/absence
assign("presabs_points", get(load(file.path(sp_data_dir, sp_name, paste0(sp_name, "_presabs.Rdata")))))

