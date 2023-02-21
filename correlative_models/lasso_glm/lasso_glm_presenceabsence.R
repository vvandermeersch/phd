############################################################################
# Script to perform a Lasso regression with presence/(pseudo-)absence data #
############################################################################

# regularization approach: lasso penalty for the coefficients (resulting in sparser models) 
# linear and quadratic terms allowed
# down-weigthing scheme : background points have a total weight equal to the total weight of presences

# this script is largely inspired by Valavi et al. R code 

# load packages and functions
library(abind)
library(dplyr)
library(data.table)
library(Matrix)
library(AUC)
source("C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models/valavi_et_al/ecm1486-sup-0003-datas1/DataS1/modelling_codes/prediction_helper.R")

# folders
wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models"
sp_data_dir <- "D:/species/processed"
clim_data_dir <- "D:/climate/ERA5-Land/bioclim_format"
soil_data_dir <- "D:/soil/processed"

# species settings
#sp_name <- "fagus_sylvatica"
#nb_pres <- 1000
#subset <- 1


# load presence and background points
pseudo_abs <- TRUE
source(file.path(wd, "scripts", "calibration_points.R"))

# load covariates
#bc_covars <- c("bio1", "bio4", "bio5", "bio6", "bio7", "bio12")
#soil_covars <- c("WHC", "bld")
#cc_covars <- c()
covars <- c(bc_covars, soil_covars, cc_covars)
source(file.path(wd, "scripts", "predictors_data.R"))
# cov_type <- "defpred" # default predictors
# cov_type <- "custpred" # custom predictors

# Europe data
europe_data <- predictors_data %>%
  dplyr::select(c("lon", "lat", all_of(covars))) %>%
  na.omit()


# create quadratic terms
quad_obj <- make_quadratic(model_data, cols = covars)
# predict quadratic objects
quad <- predict.make_quadratic(quad_obj, newdata = model_data)
# create sparse matrix
new_vars <- names(quad)[names(quad) != "pres"]
sparse <- sparse.model.matrix(~. -1, quad[, new_vars])

# model
t0 <- proc.time()
mod_lasso <- glmnet::cv.glmnet(x = sparse,
                               y = model_data$pres,
                               family = "binomial",
                               alpha = 1,
                               standardize = TRUE)
runtime <- proc.time() - t0

# evaluation
## extract the relevant covariates
testing_data <- left_join(presabs_points, predictors_data, by = c("lon", "lat"))%>% 
  dplyr::select(c("pres", all_of(covars)))
## create quadratic objects and sparse matrix
quad_obj <- make_quadratic(testing_data, cols = covars)
testing_quad <- predict.make_quadratic(quad_obj, newdata = testing_data)
testing_sparse <- sparse.model.matrix(~. -1, testing_quad[, new_vars])
## create quadratic objects and sparse matrix
quad_obj <- make_quadratic(europe_data, cols = covars)
europe_quad <- predict.make_quadratic(quad_obj, newdata = europe_data)
europe_sparse <- sparse.model.matrix(~. -1, europe_quad[, new_vars])
## prediction
prediction_cal <- as.numeric(predict(mod_lasso, sparse, type = "response", s = "lambda.min"))
prediction_tot <- as.numeric(predict(mod_lasso, testing_sparse, type = "response", s = "lambda.min"))
europe_prediction <-as.numeric(predict(mod_lasso, europe_sparse, type = "response", s = "lambda.min")) 
## AUC
auc_cal <- auc(roc(prediction_cal, as.factor(model_data$pres)))
auc_tot <- auc(roc(prediction_tot, as.factor(presabs_points$pres)))
## Best threshold
youden_index <- sensitivity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$measure +
  specificity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

# save file
outfile <- list()
outfile$covars <- covars
outfile$auc_cal <- auc_cal
outfile$auc_tot <- auc_tot
outfile$runtime <- runtime[3]
outfile$best_threshold <- best_threshold
outfile$type <- "lasso_glm"
outfile$model <- mod_lasso
outfile$occ_data <- occ_data
outfile$europe_prediction <- europe_prediction
dir.create(paste0(wd, "/lasso_glm/fit/", sp_name), showWarnings = FALSE)
saveRDS(outfile, file = paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_pa_", cov_type,"_", Sys.Date(), ".rds"))
