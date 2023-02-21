####################################################################
# Script to perform a custom ensemble approach as in Valavi et al. #
####################################################################

# un-weighted ensemble
# presence-only data

# this script is largely inspired by Valavi et al. R code 

# load packages and functions
library(tidyverse)
library(ROCR)
library(scales)
library(AUC)

# folders
wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models"
sp_data_dir <- "D:/species/processed"
wd_pred <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"

# model types
model_types <- c("gam", "lasso_glm", "maxent", "random_forest", "brt")

# load covariates
#bc_covars <- c("bio1", "bio4", "bio5", "bio6", "bio7", "bio12")
#soil_covars <- c("pH")
#cc_covars <- c()
covars <- c(bc_covars, soil_covars, cc_covars)
# cov_type <- "defpred" # default predictors
# cov_type <- "custpred" # custom predictors

# load predictions
gam <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_", cov_type,"_", date, ".rds"))
lasso_glm <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", date, ".rds"))
maxent <- readRDS(paste0(wd, "/maxent/fit/", sp_name, "/maxent_", cov_type,"_", date, ".rds"))
rf <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_", cov_type,"_", date, ".rds"))
brt <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_", cov_type,"_", date, ".rds"))

# mean prediction (no weight)
europe_prediction <- t(colMeans(rbind(gam$europe_prediction, lasso_glm$europe_prediction, maxent$europe_prediction,
                  rf$europe_prediction, brt$europe_prediction)))
load(file.path(wd_pred, "predictors_data.Rdata"))
europe_prediction <- data.frame(lat = predictors_data$lat, lon = predictors_data$lon, pred = c(europe_prediction))

# evaluation data
## extract the relevant covariates
testing_prediction <- left_join(presabs_points, europe_prediction, by = c("lon", "lat"))%>% 
  dplyr::select(c("pred"))
auc_tot <- auc(roc(t(testing_prediction), as.factor(presabs_points$pres)))
## Best threshold
youden_index <- sensitivity(t(testing_prediction), as.factor(presabs_points$pres), perc.rank = F)$measure +
  specificity(t(testing_prediction), as.factor(presabs_points$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(t(testing_prediction), as.factor(presabs_points$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]


outfile <- list()
outfile$covars <- covars
outfile$auc_cal <- NA
outfile$auc_tot <- auc_tot
outfile$best_threshold <- best_threshold
outfile$type = "custom_ensemble"
outfile$runtime <- sum(gam$runtime, lasso_glm$runtime, maxent$runtime, rf$runtime, brt$runtime)
outfile$model <- "Valavi et al. ensemble"
outfile$europe_prediction <- europe_prediction$pred
dir.create(paste0(wd, "/custom_ensemble/fit/", sp_name), showWarnings = FALSE)
saveRDS(outfile, file = paste0(wd, "/custom_ensemble/fit/", sp_name, "/custom_ensemble_", cov_type,"_", date, ".rds"))

          