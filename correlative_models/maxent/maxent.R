#############################################################
# Script to perform a MaxEnt run as in Valavi et al. (2022) #
#############################################################

# smoothing parameter estimation method: REML
# down-weigthing scheme: background points have a total weight equal to the total weight of presences

# this script is largely inspired by Valavi et al. R code 

# load packages and functions
library(disdat)
library(dismo)
library(abind)
library(dplyr)
library(data.table)
library(AUC)

# increase the RAM for Java
options(java.parameters = "-Xmx2g" )


# folders
wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models"
sp_data_dir <- "D:/species/processed"
clim_data_dir <- "D:/climate/ERA5-Land/bioclim_format"
soil_data_dir <- "D:/soil/processed"


# species settings
#sp_name <- "fagus_sylvatica"
#nb_pres <- 1000
#nb_background <- 50000
#subset <- 1


# load presence and background points
source(file.path(wd, "scripts", "calibration_points.R"))


# load covariates
#bc_covars <- c("bio1", "bio4", "bio5", "bio6", "bio7", "bio12")
#cc_covars <- c()
covars <- c(bc_covars, soil_covars, cc_covars)
source(file.path(wd, "scripts", "predictors_data.R"))
# cov_type <- "defpred" # default predictors
# cov_type <- "custpred" # custom predictors


# model
t0 <- proc.time()
mod_maxent <- dismo::maxent(x = model_data[, 2:ncol(model_data)],
                            p = model_data$pres,
                            removeDuplicates = FALSE,
                            path = file.path(wd, "maxent","process_files"),
                            args = c("nothreshold"))
runtime <- proc.time() - t0

# evaluation
## extract the relevant covariates
testing_data <- left_join(presabs_points, predictors_data, by = c("lon", "lat"))%>% 
  dplyr::select(c("pres", all_of(covars)))
## prediction
prediction_cal <- predict(mod_maxent, model_data, args = c("outputformat=cloglog"))
prediction_tot <- predict(mod_maxent, testing_data, args = c("outputformat=cloglog"))
## AUC
auc_cal <- auc(roc(prediction_cal, as.factor(model_data$pres)))
auc_tot <- auc(roc(prediction_tot, as.factor(presabs_points$pres)))
## Best threshold
youden_index <- sensitivity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$measure +
  specificity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

# Europe data and prediction
europe_data <- predictors_data %>%
  dplyr::select(c("lon", "lat", all_of(covars))) %>%
  na.omit()
europe_prediction <- predict(mod_maxent, europe_data, args = c("outputformat=cloglog"))

# save file
outfile <- list()
outfile$covars <- covars
outfile$auc_cal <- auc_cal
outfile$auc_tot <- auc_tot
outfile$runtime <- runtime[3]
outfile$best_threshold <- best_threshold
outfile$type <- "maxent"
outfile$model <- mod_maxent
outfile$occ_data <- occ_data
outfile$europe_prediction <- europe_prediction
dir.create(paste0(wd, "/maxent/fit/", sp_name), showWarnings = FALSE)
saveRDS(outfile, file = paste0(wd, "/maxent/fit/", sp_name, "/maxent_", cov_type,"_", Sys.Date(), ".rds"))
