
#----------------------#
# GAM paleosimulations #
#----------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit"
sim_dir <- "D:/simulations/csdm/gam/paleo"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

library(mgcv)

# Setup
species <- "fagus_sylvatica"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
dir.create(file.path(sim_dir, species), showWarnings = F)

# Load model
gammod <- readRDS(file.path(model_dir, species, "gam_finalcov_fullmodel.rds"))

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
    predictors[, v] <- (predictors[, v] - gammod$meanv_l[i]) / gammod$sdv_l[i]
    i <- i+1
  }
  
  # make and save predictions
  sim$pred <-  as.numeric(predict(gammod$model, predictors, type = "response"))
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
}
