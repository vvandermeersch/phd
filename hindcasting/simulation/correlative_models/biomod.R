
#-------------------------#
# Biomod paleosimulations #
#-------------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/biomod/fit"
sim_dir <- "D:/simulations/csdm/biomod/paleo"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

library(biomod2)
library(dplyr)

# Setup
species <- "fagus_sylvatica"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
dir.create(file.path(sim_dir, species), showWarnings = F)

# Load model
biomod <- readRDS(file.path(model_dir, species, "biomod_finalcov_fullmodel.rds"))

# Load soil predictors (present data used throughout past simulations)
soil_predictors <- readRDS(file.path(clim_dir, "soil_predictors.rds"))


# Simulation loop
for(year in c(500, 1000, 1500, 2000, 3000, 3500, 4000, 4500, 5000, 5500, 6000)){
  clim_predictors <- readRDS(file.path(clim_dir, paste0("predictors_", year, "BP.rds")))
  predictors <- left_join(clim_predictors, soil_predictors) %>%
    na.omit()
  
  sim <- predictors[, c("lat", "lon")]
  
  predictors <- predictors %>% 
    dplyr::select(all_of(covars))
  
  # make and save predictions
  myBiomodProj <- BIOMOD_Projection(bm.mod = biomod$model_parts,
                                    new.env = predictors,
                                    proj.name = "valavietal",
                                    selected.models = "all",
                                    binary.meth = "ROC",
                                    compress = TRUE,
                                    clamping.mask = TRUE)
  myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                               bm.em = biomod$model,
                                               selected.models = "all")
  myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
  sim$pred <-  myEnProjDF$pred/1000
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
}
