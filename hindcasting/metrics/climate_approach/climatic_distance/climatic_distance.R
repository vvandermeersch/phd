# Calculating climatic distance
## using SDM climate predictors

library(terra)
library(future.apply)
library(pracma)
library(dplyr)

covars <- c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")

### ERA5-Land present data as baseline conditions (// calibration)
load("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors/predictors_data.Rdata")
era5_predictors <- rast(lapply(covars, 
                         function(i) rast(predictors_data %>% 
                                            dplyr::select(all_of(c("lon", "lat", i))), crs = "EPSG:4326")))

### HadCM3B
hadcm3b_predictors <- readRDS("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/dscl_15min/predictors_2000BP.rds")
hadcm3b_predictors <- rast(lapply(covars, 
                               function(i) rast(hadcm3b_predictors %>% 
                                                  dplyr::select(all_of(c("lon", "lat", i))), crs = "EPSG:4326")))


### reduce spatial resolution of ERA5-Land (not used)
# era5_predictors <- resample(era5_predictors, hadcm3b_predictors, method = "average")

.era5_predictors <- wrap(era5_predictors)

plan(multisession, workers = 10)
past_climdist <- future_sapply(seq(500,18000,500), function(yr){
  era5_predictors <- rast(.era5_predictors)
  hadcm3b_predictors <- readRDS(file.path("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/dscl_15min", paste0("predictors_", yr, "BP.rds")))
  hadcm3b_predictors <- rast(lapply(covars, 
                                    function(i) rast(hadcm3b_predictors %>% 
                                                       dplyr::select(all_of(c("lon", "lat", i))), crs = "EPSG:4326")))
  r_climdist <- climatic_dissimilarity(focal = hadcm3b_predictors, baseline = era5_predictors,
                                       method = "mahalanobis")
  
  
  # ice filter (less than 50% ice cover, as in Burke et al. 2019)
  yr_ICE6G <- yr/1000
  extent <- ext(c(-10,35,36,71))
  ice_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_HR <- resample(ice_HR, hadcm3b_predictors)
  ice_HR[ice_HR >= 0.5] <- NA
  hadcm3b_predictors <- mask(hadcm3b_predictors, ice_HR)
  r_climdist_icefilter <- climatic_dissimilarity(focal = hadcm3b_predictors, baseline = era5_predictors,
                                       method = "mahalanobis")
  
  
  return(c(year = yr, 
           mean = as.numeric(global(r_climdist, mean, na.rm = T)$mean), 
           median= as.numeric(global(r_climdist, median, na.rm = T)$global), 
           q25 = as.numeric(global(r_climdist, quantile, na.rm = T)$X25), 
           q75 = as.numeric(global(r_climdist, quantile, na.rm = T)$X75),
           mean_icefilter = as.numeric(global(r_climdist_icefilter, mean, na.rm = T)$mean), 
           median_icefilter= as.numeric(global(r_climdist_icefilter, median, na.rm = T)$global), 
           q25_icefilter = as.numeric(global(r_climdist_icefilter, quantile, na.rm = T)$X25), 
           q75_icefilter = as.numeric(global(r_climdist_icefilter, quantile, na.rm = T)$X75)
           ))
})
mahal_climatic_distance <- as.data.frame(t(past_climdist))
saveRDS(mahal_climatic_distance, file ="C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/mahal_clim_distance.rds")



pollen_folder <- "D:/species/pollen/processed/fagus_sylvatica_15min"
past_climdist <- future_sapply(seq(500,18000,500), function(yr){
  era5_predictors <- rast(.era5_predictors)
  hadcm3b_predictors <- readRDS(file.path("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/dscl_15min", paste0("predictors_", yr, "BP.rds")))
  
  pollen_points <- readRDS(file.path(pollen_folder, paste0("pres_", yr, "BP.rds")))
  hadcm3b_predictors <- inner_join(hadcm3b_predictors, pollen_points)
  
  hadcm3b_predictors <- rast(lapply(covars, 
                                    function(i) rast(hadcm3b_predictors %>% 
                                                       dplyr::select(all_of(c("lon", "lat", i))), crs = "EPSG:4326")))
  
  r_climdist <- climatic_dissimilarity(focal = hadcm3b_predictors, baseline = era5_predictors,
                                       method = "mahalanobis")
  
  # ice filter (less than 50% ice cover, as in Burke et al. 2019)
  yr_ICE6G <- yr/1000
  extent <- ext(c(-10,35,36,71))
  ice_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_HR <- resample(ice_HR, hadcm3b_predictors)
  ice_HR[ice_HR >= 0.5] <- NA
  hadcm3b_predictors <- mask(hadcm3b_predictors, ice_HR)
  r_climdist_icefilter <- climatic_dissimilarity(focal = hadcm3b_predictors, baseline = era5_predictors,
                                                 method = "mahalanobis")
  
  return(c(year = yr, 
           mean = as.numeric(global(r_climdist, mean, na.rm = T)$mean), 
           median= as.numeric(global(r_climdist, median, na.rm = T)$global), 
           q25 = as.numeric(global(r_climdist, quantile, na.rm = T)$X25), 
           q75 = as.numeric(global(r_climdist, quantile, na.rm = T)$X75),
           mean = as.numeric(global(r_climdist_icefilter, mean, na.rm = T)$mean), 
           median= as.numeric(global(r_climdist_icefilter, median, na.rm = T)$global), 
           q25 = as.numeric(global(r_climdist_icefilter, quantile, na.rm = T)$X25), 
           q75 = as.numeric(global(r_climdist_icefilter, quantile, na.rm = T)$X75)
  ))
})
mahal_climatic_distance_pollenpoints <- as.data.frame(t(past_climdist))
saveRDS(mahal_climatic_distance_pollenpoints, 
        file ="C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/mahal_clim_distance_pollenpoints.rds")
