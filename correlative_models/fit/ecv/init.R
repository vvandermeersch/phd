#-------------------------------------------------#
# Script to initialize data for ecv model fitting #
#-------------------------------------------------#




### 0. Initial setup --------------- #

# Libraries
library(dplyr)
library(terra)
library(blockCV)
library(data.table)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(dismo)
library(precrec)
library(randomForest)
library(AUC)
library(Matrix)
library(ecospat)
library(biomod2)
library(rpart)

# Folders
sp_dir <- "D:/species/processed"
pred_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"




### 1. Prepare data --------------- #

# Load predictors
load(file.path(pred_dir, "predictors_data.Rdata"))
predictors_data <- predictors_data %>% 
  dplyr::select(all_of(c("lat", "lon", covars)))

# Create SpatRasters of predictors
r_pred <- rast(lapply(1:length(covars), 
               function(i) rast(predictors_data %>% 
                 dplyr::select(all_of(c("lon", "lat", covars[i]))), crs = "EPSG:4326")))

# Load species presence points
pres_points <- assign("presabs_points", get(load(file.path(sp_dir, sp_name, paste0(sp_name, "_presabs.Rdata"))))) %>%
  dplyr::filter(pres == 1)

# Add background data
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
## sample background points
bg_points <- sample(1:nrow(ERA5_points_sampledarea), nb_background)
background <- ERA5_points_sampledarea[bg_points, -c("alt")]
background$pres <- 0

# Merge species presence and background points 
presbg_points <- rbind(pres_points, background)
saveRDS(presbg_points, file.path(wd, "calibration_points.rds"))
presbg_points_sf <- sf::st_as_sf(presbg_points, coords = c("lon", "lat"), crs = "EPSG:4326")
model_data <- terra::extract(r_pred, presbg_points_sf, df = TRUE, ID = FALSE)
model_data$pres <- as.numeric(presbg_points_sf$pres)
saveRDS(model_data, file.path(wd, "calibration_predictors.rds"))

# Data for every species points
presabs_points_sf <- sf::st_as_sf(presabs_points, coords = c("lon", "lat"), crs = "EPSG:4326")
presabs_data <- terra::extract(r_pred, presabs_points_sf, df = TRUE, ID = FALSE)
presabs_data$pres <- as.numeric(presabs_points_sf$pres)

# Data for all Europe
europe_points_sf <- sf::st_as_sf(alt, coords = c("lon", "lat"), crs = "EPSG:4326")
europe_data <- terra::extract(r_pred, europe_points_sf, df = TRUE, ID = FALSE)


### 2. Spatial and environmental clustering --------------- #
e_clust <- cv_cluster(x = presbg_points_sf,
                      column = "pres",
                      r = r_pred,
                      k = nfold, 
                      scale = TRUE)

folds <- e_clust$folds_list




### 3. Normalise covariates --------------- #
model_data_norm <- model_data
presabs_data_norm <- presabs_data
europe_data_norm <- europe_data
meanv_l <- c()
sdv_l <- c()
i <- 1
for(v in covars){
  meanv_l[i] <- mean(model_data_norm[, v])
  sdv_l[i] <- sd(model_data_norm[, v])
  model_data_norm[, v] <- (model_data_norm[, v] - meanv_l[i]) / sdv_l[i]
  presabs_data_norm[, v] <- (presabs_data_norm[, v] - meanv_l[i]) / sdv_l[i]
  europe_data_norm[, v] <- (europe_data_norm[, v] - meanv_l[i]) / sdv_l[i]
  i <- i+1
}


### 4. Do some cleaning
rm(ERA5_points, ERA5_points_sampledarea, eu_map, eu_map_cropped, europe_points_sf, predictors_data, 
   pres_points, presabs_points, presabs_points_sf, presbg_points_sf, r_pred, world_map)
gc()
