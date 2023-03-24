
#--------------------------------#
# Random Forest paleosimulations #
#--------------------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit"
sim_dir <- "D:/simulations/csdm/random_forest/paleo"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

library(randomForest)
library(dplyr)

# Setup
species <- "fagus_sylvatica"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
dir.create(file.path(sim_dir, species), showWarnings = F)

# Load model
rfmod <- readRDS(file.path(model_dir, species, "random_forest_finalcov_fullmodel.rds"))

# Load soil predictors (present data used throughout past simulations)
soil_predictors <- readRDS(file.path(clim_dir, "soil_predictors.rds"))

# Simulation loop
for(year in c(5000, 5500, 6000)){
  clim_predictors <- readRDS(file.path(clim_dir, paste0("predictors_", year, "BP.rds")))
  predictors <- left_join(clim_predictors, soil_predictors) %>%
    na.omit()
  
  sim <- predictors[, c("lat", "lon")]
  
  predictors <- predictors %>% 
    dplyr::select(all_of(covars))
  
  # normalize predictors
  i <- 1
  for(v in covars){
    predictors[, v] <- (predictors[, v] - rfmod$meanv_l[i]) / rfmod$sdv_l[i]
    i <- i+1
  }
  
  # make and save predictions
  sim$pred <- as.numeric(predict(rfmod$model, predictors, type = "prob")[,"1"])
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
}


plot(rast(sim[c("lon", "lat", "pred")]))
plot(rast(rfmod[["europe_pred"]][c("lon", "lat", "pred")]))

plot(rast(predictors[c("lon", "lat", "w_bal")]))
plot(rast(predictors_data[c("lon", "lat", "w_bal")]))

ggplot() +
  geom_raster(data = predictors_data, aes(x = lon, y = lat, fill = bio12)) +
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(240,2000, 3400), limits = c(240,3400)) +
  theme_minimal()
  

ggplot() +
  geom_raster(data = predictors, aes(x = lon, y = lat, fill = bio6)) +
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(-15,0,15), limits = c(-30,15)) +
  theme_minimal()

ggplot() +
  geom_raster(data = predictors, aes(x = lon, y = lat, fill = sum_apsep_GDD5)) +
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(1000, 2500, 4000), limits = c(0,5000)) +
  theme_minimal() +
  labs(fill = "GDD")


ggplot() +
  geom_raster(data = tmean, aes(x = x, y = y, fill = mean)) +
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(-10,0,10), limits = c(-10,20)) +
  theme_minimal()

r_tmeanbias <- rast(clim_predictors[,c("lon", "lat", "tmean")])

r_tmean <- crop(mean(rast(file.path("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw", "temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                         paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", "2000_0kyr", ".nc")),
               subds = "temp_mm_1_5m", lyrs = 24001:24012, opts="HONOUR_VALID_RANGE=NO")), ext(r_tmeanbias))
crs(r_tmeanbias) <- crs(r_tmean)
dif <- r_tmeanbias - r_tmean


ggplot() +
  geom_raster(data = pet, aes(x = lon, y = lat, fill = pet)) + 
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(60, 350, 620), limits = c(60,620))

ggplot() +
  geom_raster(data = pet, aes(x = lon, y = lat, fill = glo)) + 
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(870, 1300, 1800), limits = c(500,1800))

ggplot() +
  geom_raster(data = predictors_data, aes(x = lon, y = lat, fill = glo)) + 
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(870, 1300, 1800), limits = c(500,1800))

ggplot() +
  geom_raster(data = predictors_data, aes(x = lon, y = lat, fill = w_bal)) +
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(-400, 0, 400), limits = c(-600,400)) +
  theme_minimal()

ggplot() +
  geom_raster(data = predictors, aes(x = lon, y = lat, fill = w_bal)) +
  scale_fill_gradientn(colours = c("#ff9ea0",  "#43aa8b",  "#577590"), breaks = c(-400, 0, 400), limits = c(-600,400)) +
  theme_minimal()


predictors_r <- rast(predictors_data[, c("lon", "lat", covars)])
plot(predictors_r)
predictors_r <- terra::resample(predictors_r, predictors2_r, method = "average")

predictors2_r <- rast(predictors[, c("lon", "lat", covars)])
extent <- ext(-5, 5, 45, 50)
predictors2_r <- crop(predictors2_r, extent)
predictors_r <- crop(predictors_r, extent)

plot(subset(predictors2_r - predictors_r,c(1,2,3,4,5,6)))






pred <- rast(sim[c("lon", "lat", "pred")])
pred_era5 <- rast(rfmod[["europe_pred"]][c("lon", "lat", "pred")])

pred_era5 <- terra::resample(pred_era5, pred, method = "average")
pred_era5 <- crop(pred_era5, extent)
pred <- crop(pred, extent)
plot(pred - pred_era5)
