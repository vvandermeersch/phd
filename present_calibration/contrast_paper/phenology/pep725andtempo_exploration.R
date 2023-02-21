library(data.table)
library(dplyr)
library(ggplot2)
library(raster)

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/present_calibration/contrast_paper/phenology"


# europe map # ----
library(rnaturalearth)
library(sf)
library(tidyverse)

# Europe map
countries_EUForest <- c("Austria", "Belarus", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Moldova", "Netherlands", "Norway", "Poland", "Portugal",
                        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
other_countries <- c("Bosnia and Herzegovina", "Republic of Serbia", "Macedonia", "Greece",
                     "Kosovo", "Albania", "Montenegro", "Malta", "Liechtenstein", "Luxembourg")
countries <- c(countries_EUForest, other_countries)
world_map <- ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(sovereignt %in% countries)
eu_map_cropped <- eu_map %>% 
  st_crop(st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = st_crs(4326)))

###########
# PEP 725 #
###########

dir <- "D:/phenology/PEP725/fagus sylvatica"

# load phenology records
records <- fread(file.path(dir, "Fagus_records_merged.csv"))
# keep only leaf unfolding records between 1970 and 2010
records <- records[records$BBCH %in% c(86) & records$YEAR < 2001 & records$YEAR > 1970,]
# mean by stations
records <- records %>% group_by(PEP_ID) %>%
  dplyr::summarize(MEAN_DAY = mean(DAY, na.rm=TRUE),
                   NB_YEAR = n())

# load stations
stations <- fread(file.path(dir, "Fagus_stations_merged.csv"))
stations <- stations %>% dplyr::select(PEP_ID, LON, LAT)

# join records and stations
records_loc <- left_join(records, stations, by = "PEP_ID")

# map
# ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey', size = 0.1) +
#   geom_point(data = records_loc, aes(x = LON, y = LAT, col = MEAN_DAY)) +
#   theme_void() +
#   ylab("") +
#   xlab("")

# map, 5 observations min
pep725_3obsmin_map <- ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'white', alpha=0.1, color='grey', size = 0.1) +
  geom_point(data = records_loc[records_loc$NB_YEAR > 4,], aes(x = LON, y = LAT, 
                                                               col = MEAN_DAY, size = NB_YEAR)) +
  theme_void() +
  ylab("") +
  xlab("") +
  scale_color_gradient2(name = stringr::str_wrap("Leaf unfolding day", width = 30), 
                        low = "#0A9396", mid ="#e9d8a6", high = "#bb3e03", 
                        limits = c(80,160), 
                        midpoint = 120) +
  ylim(41,64) +
  xlim(-11,41)


# rasterize observations accordign to ERA_Land grid
era5_land_grid <- raster("D:/climate/ERA5-Land/raw/2m_dewpoint_temperature_1969_01.nc")
records_loc_sp <- SpatialPointsDataFrame(records_loc[records_loc$NB_YEAR > 4,c("LON", "LAT"),], 
                                         records_loc[records_loc$NB_YEAR > 4,"MEAN_DAY"])
r_s <- rasterize(records_loc_sp, era5_land_grid, fun="sum", na.rm = TRUE)
r_c <- rasterize(records_loc_sp, era5_land_grid, fun="count", na.rm = TRUE)
records_r <- r_s/r_c

# map raster
records_r_df <- as.data.frame(records_r, xy = TRUE)
# ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'white', alpha=0.1, color='grey', size = 0.1) +
#   geom_raster(data = records_r_df, aes(x = x, y = y, fill = MEAN_DAY)) +
#   theme_void() +
#   ylab("") +
#   xlab("") +
#   scale_fill_gradient2(name = stringr::str_wrap("Leaf unfolding day", width = 30), 
#                         low = "#0A9396", mid ="#e9d8a6", high = "#bb3e03", 
#                         limits = c(80,160), 
#                         midpoint = 120, na.value = NA) +
#   ylim(45,60) +
#   xlim(-5,30)




#########
# TEMPO #
#########

dir <- "D:/phenology/tempo/fagus sylvatica"

# load phenology records
records_tempo <- fread(file.path(dir, "pheno-pmp.txt"))
# keep only records between 1970 and 2010
records_tempo <- records_tempo[records_tempo$ANNEE < 2001 & records_tempo$ANNEE > 1970,]
# -9999 to NA
records_tempo <- data.frame(records_tempo)
records_tempo[records_tempo == -9999] <- NA
# mean by stations
records_tempo <- records_tempo %>% group_by(CODE_POSTE) %>%
  dplyr::summarize(MEAN_DAY = mean(c_across(starts_with("X")), na.rm = TRUE),
                   NB_YEAR = n())

# load stations
stations_tempo  <- fread(file.path(dir, "sites.csv"))
stations_tempo  <- stations_tempo  %>% dplyr::select("site id", latitude, longitude)

# join records and stations
records_tempo_loc <- left_join(records_tempo, stations_tempo, by = c("CODE_POSTE" = "site id"))

# map
# ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey', size = 0.1) +
#   geom_point(data = records_tempo_loc, aes(x = longitude, y = latitude, col = MEAN_DAY)) +
#   theme_void() +
#   ylab("") +
#   xlab("")

# map, 5 observations min
tempo_3obsmin_map <- ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'white', alpha=0.1, color='grey', size = 0.1) +
  geom_point(data = records_tempo_loc[records_tempo_loc$NB_YEAR > 4,], aes(x = longitude, y = latitude, 
                                                               col = MEAN_DAY, size = NB_YEAR)) +
  theme_void() +
  ylab("") +
  xlab("") +
  scale_color_gradient2(name = stringr::str_wrap("Leaf unfolding day", width = 30), 
                        low = "#0A9396", mid ="#e9d8a6", high = "#bb3e03", 
                        limits = c(80,160), 
                        midpoint = 120) +
  ylim(42,52) +
  xlim(-5,10)

# rasterize observations accordign to ERA_Land grid
era5_land_grid <- raster("D:/climate/ERA5-Land/raw/2m_dewpoint_temperature_1969_01.nc")
records_tempo_loc_sp <- SpatialPointsDataFrame(records_tempo_loc[records_tempo_loc$NB_YEAR > 2,c("longitude", "latitude"),], 
                                         records_tempo_loc[records_tempo_loc$NB_YEAR > 2,"MEAN_DAY"])
r_s <- rasterize(records_tempo_loc_sp, era5_land_grid, fun="sum", na.rm = TRUE)
r_c <- rasterize(records_tempo_loc_sp, era5_land_grid, fun="count", na.rm = TRUE)
records_tempo_r <- r_s/r_c

# map raster
records_tempo_r_df <- as.data.frame(records_tempo_r, xy = TRUE)
# ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'white', alpha=0.1, color='grey', size = 0.1) +
#   geom_raster(data = records_tempo_r_df, aes(x = x, y = y, fill = MEAN_DAY)) +
#   theme_void() +
#   ylab("") +
#   xlab("") +
#   scale_fill_gradient2(name = stringr::str_wrap("Leaf unfolding day", width = 30), 
#                        low = "#0A9396", mid ="#e9d8a6", high = "#bb3e03", 
#                        limits = c(80,160), 
#                        midpoint = 120, na.value = NA) +
#   ylim(42,52) +
#   xlim(-5,10)



########
# BOTH #
########


# save a raster of both database, 5 obs per station min
names(records_tempo_loc) <- c("CODE_POSTE", "MEAN_DAY", "NB_YEAR", "LAT", "LON")
records_all <- rbind(records_tempo_loc[-c(1)], records_loc[-c(1)])
records_all_sp <- SpatialPointsDataFrame(records_all[records_all$NB_YEAR > 4,c("LON", "LAT"),], 
                                         records_all[records_all$NB_YEAR > 4,"MEAN_DAY"])
r_s <- rasterize(records_all_sp, era5_land_grid, fun="sum", na.rm = TRUE)
r_c <- rasterize(records_all_sp, era5_land_grid, fun="count", na.rm = TRUE)
records_all_r <- r_s/r_c
saveRDS(records_all_r, file = file.path(wd, "pheno_raster.rds"))


records_all_r_df <- as.data.frame(records_all_r, xy = TRUE)

raster_map <- ggplot() +
  geom_sf(data = eu_map_cropped, fill = NA, alpha=0.1, color='grey', size = 0.1) +
  geom_raster(data = records_all_r_df, aes(x = x, y = y, fill = MEAN_DAY)) +
  theme_void() +
  ylab("") +
  xlab("") +
  scale_fill_gradient2(name = stringr::str_wrap("Leaf unfolding day", width = 30), 
                       low = "#0A9396", mid ="#e9d8a6", high = "#bb3e03", 
                       limits = c(80,160), 
                       midpoint = 120, na.value = NA,
                       guide = guide_colourbar(title.position = "top", frame.colour = "black", 
                                               frame.linewidth = 0.5, ticks = FALSE)) +
  theme(legend.title.align = 0.5, legend.direction = "horizontal", legend.position = "bottom", 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(1, 'cm')) +
  ylim(41,60) +
  xlim(-9,20)
