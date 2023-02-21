# Load relevant predictors for modelling

## Compute predictors
compute <- FALSE # compute can be quite long
wd_pred <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"
if(compute){
  source(file.path(wd_pred, "compute_predictors.R"))
}else{
  load(file.path(wd_pred, "predictors_data.Rdata"))
}


## extract the relevant lines for modelling
if(pseudo_abs){
  model_data <- left_join(pr_ab, predictors_data, by = c("lon", "lat"))
}else{
  model_data <- left_join(pr_bg, predictors_data, by = c("lon", "lat"))
}


## choose relevant covariates
occ_data <- model_data[, c("lat", "lon", "pres")]
model_data <- model_data[, c("pres", covars)]