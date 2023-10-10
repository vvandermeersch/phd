
#----------------------#
# BRT paleosimulations #
#----------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit"
sim_dir <- "D:/simulations/csdm/brt/paleo/025deg"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/025deg"

library(gbm)
library(dplyr)
library(foreach)

# Setup
species <- "quercus_petraea"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
dir.create(file.path(sim_dir, species), showWarnings = F)

# Load model
brtmod <- readRDS(file.path(model_dir, species, "brt_finalcov_fullmodel.rds"))

# Simulation loop
var_importance <- sapply(seq(250,18000,250), function(year){
  predictors <- readRDS(file.path(clim_dir, paste0("predictors_", year, "BP.rds")))
  
  sim <- predictors[, c("lat", "lon")]
  
  predictors <- predictors %>% 
    dplyr::select(all_of(covars))
  
  # normalize predictors
  i <- 1
  for(v in covars){
    predictors[, v] <- (predictors[, v] - brtmod$meanv_l[i]) / brtmod$sdv_l[i]
    i <- i+1
  }
  
  # make and save predictions
  sim$pred <-  predict(brtmod$model, predictors, n.trees = brtmod$model$gbm.call$best.trees, type = "response")
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
  # var_importance <- bm_VariablesImportance(bm.model=brtmod$model, predictors, variables = c(bc_covars, cc_covars), 
  #                                          method = "full_rand", nb.rep = 3, do.progress = FALSE)
  # 
  # var_importance <- var_importance %>% 
  #   group_by(expl.var) %>%
  #   summarize(var.imp = mean(var.imp))
  # 
  # return(c(year, t(var_importance$var.imp)))
  
})

var_importance <- as.data.frame(t(var_importance))
names(var_importance) <- c("year", bc_covars, cc_covars)
saveRDS(var_importance, file.path(sim_dir, species, "variable_importance.rds"))

