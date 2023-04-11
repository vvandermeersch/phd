
#--------------------------------#
# Random Forest paleosimulations #
#--------------------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv_5180/random_forest/fit"
sim_dir <- "D:/simulations/csdm/random_forest/paleo_cal5180"
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
for(year in seq(500,9000,500)){
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


