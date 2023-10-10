
#----------------------------#
# Lasso GLM paleosimulations #
#----------------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit"
sim_dir <- "D:/simulations/csdm/lasso_glm/paleo/025deg"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/025deg"

library(Matrix)
library(glmnet)
library(dplyr)

# Setup
species <- "quercus_pubescens"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
source("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/valavi_et_al/ecm1486-sup-0003-datas1/DataS1/modelling_codes/prediction_helper.R")
dir.create(file.path(sim_dir, species), showWarnings = F)

# Load model
glmmod <- readRDS(file.path(model_dir, species, "lasso_glm_finalcov_fullmodel.rds"))

# Simulation loop
for(year in seq(250,18000,250)){
  predictors <- readRDS(file.path(clim_dir, paste0("predictors_", year, "BP.rds")))
  
  sim <- predictors[, c("lat", "lon")]
  
  predictors <- predictors %>% 
    dplyr::select(all_of(covars))
  
  # create quadratic terms and sparse matrix (full data)
  quad <- predict.make_quadratic(glmmod$quad_obj, newdata = predictors)
  new_vars <- names(quad)[names(quad) != "pres"]
  sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
  
  # make and save predictions
  sim$pred <- as.numeric(predict(glmmod$model, sparse, type = "response", s = "lambda.min"))
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
  
}

