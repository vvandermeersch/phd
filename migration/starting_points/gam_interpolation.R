library(dplyr)
library(randomForest)
library(terra)
library(precrec)
library(AUC)

# load pollen records
pollen_dir <- "D:/species/pollen/processed/fagus_sylvatica/025deg/001adp_thr_1000yrunc"
refugia <- lapply(seq(11500,11500, 500), function (i) {
  ref <- readRDS(file.path(pollen_dir, paste0("pres_",i, "BP.rds")))
  return(ref)})
refugia <- unique(as.data.frame(do.call(rbind, refugia)))
refugia <- refugia %>%
  dplyr::group_by(lat, lon) %>%
  summarize(pres = max(pres)) %>%
  as.data.frame()


# prepare covariates
year <- 11500
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/dscl_15min"
predictors <- readRDS(file.path(clim_dir, paste0("predictors_", year, "BP.rds")))
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <-  c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)

full_data_norm <- predictors %>% 
  dplyr::select(all_of(c("lat", "lon", covars)))
meanv_l <- c()
sdv_l <- c()
i <- 1
for(v in covars){
  meanv_l[i] <- mean(full_data_norm[, v])
  sdv_l[i] <- sd(full_data_norm[, v])
  full_data_norm[, v] <- (full_data_norm[, v] - meanv_l[i]) / sdv_l[i]
  i <- i+1
}
model_data_norm <- dplyr::inner_join(full_data_norm, refugia) %>%
  dplyr::select(-c("lat", "lon"))


# calculate case weights (down-weighted)
prNum <- as.numeric(table(model_data_norm$pres)["1"]) # number of presences
bgNum <- as.numeric(table(model_data_norm$pres)["0"]) # number of backgrounds
# wt <- ifelse(model_data_norm$pres == 1, 1, prNum / bgNum)
wt <- ifelse(model_data_norm$pres == 1, 1, 100)

# run model
gam_formula <- paste("pres ~", paste(paste0("s(", covars, ")"), collapse = " + "))
t0 <- proc.time()
mod_gam <- mgcv::gam(formula = as.formula(gam_formula), 
                     data = model_data_norm,
                     family = binomial(link = "logit"),
                     weights = wt,
                     method = "REML")
runtime <- proc.time() - t0

# predict on all species points
full_data_norm$pred <- as.numeric(predict(mod_gam, full_data_norm, type = "response"))
plot(rast(full_data_norm[c("lon","lat","pred")]))
points(refugia[refugia$pres ==1, "lat"] ~ refugia[refugia$pres ==1, "lon"])
model_data_norm <- left_join(model_data_norm, full_data_norm)

# AUC
eval_obj <- evalmod(scores = model_data_norm$pred, labels = model_data_norm$pres)
auc_all <- precrec::auc(eval_obj)
cat(paste0("      AUC = ", round(auc_all$aucs[1],2), "\n"))

# best threshold
youden_index <- sensitivity(model_data_norm$pred, as.factor(model_data_norm$pres), perc.rank = F)$measure +
  specificity(model_data_norm$pred, as.factor(model_data_norm$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(model_data_norm$pred, as.factor(model_data_norm$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

# 
full_data_norm$pres <- 0
full_data_norm[full_data_norm$pred >= best_threshold, "pres"] <- 1
plot(rast(full_data_norm[c("lon","lat","pred")]))
points(full_data_norm[full_data_norm$pres ==1, "lat"] ~ full_data_norm[full_data_norm$pres ==1, "lon"])


