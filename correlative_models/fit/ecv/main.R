#-------------------------------#
# Main script to run all models #
#-------------------------------#

# Run correlative models with blockCV environmental clustering 
# Evaluate the ability of the model for situations of extrapolation (in predictor space)

# Author: V. Van der Meersch
# Date: 02/03/2023



wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv"
set.seed(1997)



### 0. Parameters --------------- #

sp_name <- "picea_abies" # species name
nb_background <- 50000 # number of background points
nfold <- 5

cov_type <- "finalcov" # name of covariate set
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)



### 1. Initial setup --------------- #

source(file.path(wd, "init.R"))



### 2. Run models --------------- #

## Lasso GLM
source(file.path(wd, "lasso_glm", "lasso_glm.R"))

## GAM
source(file.path(wd, "gam", "gam.R"))

## Random forest
source(file.path(wd, "random_forest", "random_forest.R"))

## Boosted regression tree
source(file.path(wd, "brt", "brt.R"))

## Additional test : equal-sample Random Forest
# source(file.path(wd, "random_forest_es", "random_forest_es.R"))


