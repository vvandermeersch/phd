library(rnaturalearth)
library(sf)

## Europe map
countries_EUForest <- c("Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Moldova", "Netherlands", "Norway", "Poland", "Portugal",
                        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
other_countries <- c("Bosnia and Herzegovina", "Republic of Serbia", "Macedonia", "Greece",
                     "Kosovo", "Albania", "Montenegro", "Malta", "Liechtenstein", 
                     "Luxembourg", "Ukraine", "Turkey", "Georgia", "Russia")
countries <- c(countries_EUForest, other_countries)
world_map <- ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(sovereignt %in% countries)
eu_map <- st_make_valid(eu_map)
eu_map_cropped <- eu_map %>% 
  st_crop(st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = st_crs(4326)))

rm(eu_map, world_map, other_countries, countries_EUForest, countries)
