library(spThin)
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(patchwork)

EUForests <- read.csv("C:/Users/33651/Documents/Projets/CEFE/thesis/sample_thinning/data/EuForestspecies.csv")

full_dataset_Abiesalba <- EUForests[EUForests$SPECIES.NAME=="Abies alba",] # 9383 observations
full_dataset_Abiesalba_sf <- st_as_sf(full_dataset_Abiesalba, coords = c("X","Y"), crs=st_crs(3035))
full_dataset_Abiesalba_sf <- st_transform(full_dataset_Abiesalba_sf, crs = st_crs(4326))

full_dataset_Abiesalba_sf <- full_dataset_Abiesalba_sf %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

###### spThin ######

thinned_dataset_Abiesalba_5km <-
  thin( loc.data = full_dataset_Abiesalba_sf, 
        lat.col = "lat", long.col = "lon", 
        spec.col = "SPECIES.NAME", 
        thin.par = 5, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = FALSE, 
        write.log.file = FALSE) # 4205 observations with a 5km radius

thinned_dataset_Abiesalba_10km <-
  thin( loc.data = full_dataset_Abiesalba_sf, 
        lat.col = "lat", long.col = "lon", 
        spec.col = "SPECIES.NAME", 
        thin.par = 10, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = FALSE, 
        write.log.file = FALSE) # 2197 observations with a 10km radius

thinned_dataset_Abiesalba_25km <-
  thin( loc.data = full_dataset_Abiesalba_sf, 
        lat.col = "lat", long.col = "lon", 
        spec.col = "SPECIES.NAME", 
        thin.par = 25, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = FALSE, 
        write.log.file = FALSE) # 730 observations with a 25km radius



# Which radius ? Examples with trees :
# 3km in Rico et al. (2021, PeerJ) with Magnolia trees : "a larger radius would have significantly reduce the final number of occurrences"
# 4km in Fremout et al. (2020, GCB) with tropical dry forest trees
# 25km in Grossman (2021, Forests) with temperate maples





###### Spatial agregation ######

EEA_grid_1km <- st_read("C:/Users/33651/Documents/Projets/CEFE/thesis/sample_thinning/data/EEA_reference_grids", layer = "europe_1km")
EEA_grid_1km <- st_transform(EEA_grid_1km, crs = st_crs(4326))
EEA_grid_1km_Abiesalba  <- EEA_grid_1km[unique(unlist(st_intersects(full_dataset_Abiesalba_sf, EEA_grid_1km))),]

EEA_grid_10km <- st_read("C:/Users/33651/Documents/Projets/CEFE/thesis/sample_thinning/data/EEA_reference_grids", layer = "europe_10km")
EEA_grid_10km <- st_transform(EEA_grid_10km, crs = st_crs(4326))
EEA_grid_10km_Abiesalba  <- EEA_grid_10km[unique(unlist(st_intersects(full_dataset_Abiesalba_sf, EEA_grid_10km))),]

p_grid_10km <- ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.2) +
  geom_sf(data = EEA_grid_10km_Abiesalba, lwd=0, fill="darkgreen") +
  theme_void() +
  labs(title="10km grid, 3222 obs.")


###### Maps ######

countries <- as.character(unique(full_dataset_Abiesalba$COUNTRY))
countries[4] <- "Czech Rep."
countries[12] <- "Poland"

world_map <- ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(name %in% countries)
eu_map_cropped <- eu_map %>% st_crop(st_bbox(c(xmin = -12, xmax = 34, ymax = 71, ymin = 34), crs = st_crs(4326)))

thinned_dataset_Abiesalba_5km_sf <- st_as_sf(data.frame(thinned_dataset_Abiesalba_5km[1]), coords = c("Longitude","Latitude"), crs=st_crs(4326))
thinned_dataset_Abiesalba_10km_sf <- st_as_sf(data.frame(thinned_dataset_Abiesalba_10km[1]), coords = c("Longitude","Latitude"), crs=st_crs(4326))
thinned_dataset_Abiesalba_25km_sf <- st_as_sf(data.frame(thinned_dataset_Abiesalba_25km[1]), coords = c("Longitude","Latitude"), crs=st_crs(4326))

p_spThin_5km <- ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.2) +
  geom_sf(data = full_dataset_Abiesalba_sf, size = 0.5, color = "darkgreen") +
  geom_sf(data = thinned_dataset_Abiesalba_5km_sf, size = 0.5, color = "red") +
  theme_void() + 
  labs(title="5km radius, 4205 obs.")

p_spThin_10km <- ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.2) +
  geom_sf(data = full_dataset_Abiesalba_sf, size = 0.5, color = "darkgreen") +
  geom_sf(data = thinned_dataset_Abiesalba_10km_sf, size = 0.5, color = "red") +
  theme_void() + 
  labs(title="10km radius, 2197 obs.")

p_spThin_25km <- ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.2) +
  geom_sf(data = full_dataset_Abiesalba_sf, size = 0.5, color = "darkgreen") +
  geom_sf(data = thinned_dataset_Abiesalba_25km_sf, size = 0.5, color = "red") +
  theme_void() + 
  labs(title="25km radius, 730 obs.")

p_grid_10km <- ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.2) +
  geom_sf(data = full_dataset_Abiesalba_sf, size = 0.5, color = "darkgreen") +
  geom_sf(data = EEA_grid_10km_Abiesalba, colour = NA, fill="red") +
  theme_void() +
  labs(title="10km grid, 3222 obs.")

p_spThin_5km + p_spThin_10km + p_spThin_25km + p_grid_10km +
  plot_annotation(
    tag_levels="A",
    tag_prefix = "Fig. "
  ) &
  theme(plot.tag=element_text(size=10), plot.title=element_text(size=11, hjust=0.5))
