library(terra)
library(rnaturalearth)
library(sf)
library(tidyverse)


# Europe map
countries_EUForest <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Moldova", "Netherlands", "Norway", "Portugal",
                        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom") # removed Belarus and Poland
other_countries <- c("Bosnia and Herzegovina", "Republic of Serbia", "Macedonia", "Greece",
                     "Kosovo", "Albania", "Montenegro", "Malta", "Liechtenstein", "Luxembourg")
countries <- c(countries_EUForest, other_countries)
world_map <- ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(sovereignt %in% countries)
eu_map_cropped <- eu_map %>% 
  st_crop(st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = st_crs(4326)))

eu_map_cropped <- vect(eu_map_cropped)

sp_presabs <- readRDS(file.path(sp_folder, "quercus_robur/quercus_robur_presabs.rds"))
sp_presabs  <- vect(sp_presabs ,geom=c('lon','lat'))
sp_presabs_woUkraine <- mask(sp_presabs, eu_map_cropped)
plot(eu_map_cropped)
points(sp_presabs_woUkraine)

sp_presabs_woUkraine <- as.data.frame(sp_presabs_woUkraine, geom = "XY")
names(sp_presabs_woUkraine) <- c("pres", "lon", "lat")

saveRDS(sp_presabs_woUkraine, file.path(sp_folder, "quercus_robur/quercus_robur_presabs_woUkrainePolandBelarus.rds"))
