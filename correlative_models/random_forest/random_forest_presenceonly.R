##################################################################
# Script to perform a down-sampled RF as in Valavi et al. (2022) #
##################################################################

# down-sampling approach : each tree is fitted with a bootstrap sample of presences and the same number of background points

# this script is largely inspired by Valavi et al. R code 

# load packages and functions
library(disdat)
library(randomForest)
library(abind)
library(dplyr)
library(data.table)
library(AUC)

# folders
wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models"
sp_data_dir <- "D:/species/processed"
clim_data_dir <- "D:/climate/ERA5-Land/bioclim_format"
soil_data_dir <- "D:/soil/processed"

# species settings
#sp_name <- "fagus_sylvatica"
#nb_pres <- 1000
#nb_background <- 50000
#subset <- 1

# load presence and background points
pseudo_abs <- FALSE
source(file.path(wd, "scripts", "calibration_points.R"))


# load covariates
#bc_covars <- c("bio1", "bio4", "bio5", "bio6", "bio7", "bio12")
#soil_covars <- c("WHC", "bld")
#cc_covars <- c()
covars <- c(bc_covars, soil_covars, cc_covars)
source(file.path(wd, "scripts", "predictors_data.R"))
# cov_type <- "defpred" # default predictors
# cov_type <- "custpred" # custom predictors

# evaluation data
## extract the relevant covariates
testing_data <- left_join(presabs_points, predictors_data, by = c("lon", "lat"))%>% 
  dplyr::select(c("pres", all_of(covars)))

# Europe data
europe_data <- predictors_data %>%
  dplyr::select(c("lon", "lat", all_of(covars))) %>%
  na.omit()


# normalising covariates
meanv_l <- c()
sdv_l <- c()
i <- 1
for(v in covars){
  meanv_l[i] <- mean(model_data[, v])
  sdv_l[i] <- sd(model_data[, v])
  model_data[, v] <- (model_data[, v] - meanv_l[i]) / sdv_l[i]
  testing_data[, v] <- (testing_data[, v] - meanv_l[i]) / sdv_l[i]
  europe_data[, v] <- (europe_data[, v] - meanv_l[i]) / sdv_l[i]
  i <- i+1
}

# convert the response to factor for RF model to return probabilities
model_data$pres <- as.factor(model_data$pres)

# calculating the class weights and sample size
prNum <- as.numeric(table(model_data$pres)["1"]) # number of presences
bgNum <- as.numeric(table(model_data$pres)["0"]) # number of backgrounds
# cwt <- c("1" = 1, "0" = prNum / bgNum)
samsize <- c("0" = prNum, "1" = prNum)

# model
t0 <- proc.time()
mod_rf <- randomForest(pres ~ ., 
                       data = model_data,
                       ntree = 1000,
                       sampsize = samsize,
                       replace = TRUE)
runtime <- proc.time() - t0

# evaluation
## prediction
prediction_cal <- as.numeric(predict(mod_rf, model_data, type = "prob")[,"1"])
prediction_tot <- as.numeric(predict(mod_rf, testing_data, type = "prob")[,"1"])
europe_prediction <- as.numeric(predict(mod_rf, europe_data, type = "prob")[,"1"])
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
outfile$type = "random_forest"
outfile$model <- mod_rf
outfile$occ_data <- occ_data
outfile$meanv_l <- meanv_l
outfile$sdv_l <- sdv_l
outfile$europe_prediction <- europe_prediction
dir.create(paste0(wd, "/random_forest/fit/", sp_name), showWarnings = FALSE)
saveRDS(outfile, file = paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_", cov_type,"_", Sys.Date(), ".rds"))


