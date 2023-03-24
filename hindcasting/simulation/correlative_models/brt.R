
#----------------------#
# BRT paleosimulations #
#----------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit"
sim_dir <- "D:/simulations/csdm/brt/paleo"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

library(gbm)

# Setup
species <- "fagus_sylvatica"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
dir.create(file.path(sim_dir, species), showWarnings = F)

# Load model
brtmod <- readRDS(file.path(model_dir, species, "brt_finalcov_fullmodel.rds"))

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
    meanv <- mean(predictors[, v])
    sdv <- (var(predictors[, v]))^(1/2)
    predictors[, v] <- (predictors[, v] - meanv) / sdv
    i <- i+1
  }
  
  # make and save predictions
  sim$pred <-  predict(brtmod$model, predictors, n.trees = brtmod$model$gbm.call$best.trees, type = "response")
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
}
