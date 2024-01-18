
#-----------------------------------------------------------------#
# Script to perform a Lasso regression as in Valavi et al. (2022) #
#-----------------------------------------------------------------#

# regularization approach: lasso penalty for the coefficients (resulting in sparser models) 
# linear and quadratic terms allowed
# down-weighting scheme : background points have a total weight equal to the total weight of presences
# this script is largely inspired by Valavi et al. R code 


# 1. Common parameters
source("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/valavi_et_al/ecm1486-sup-0003-datas1/DataS1/modelling_codes/prediction_helper.R")

# create quadratic terms and sparse matrix (all species points)
# quad_obj <- make_quadratic(presabs_data, cols = covars)
# quad <- predict.make_quadratic(quad_obj, newdata = presabs_data)
# new_vars <- names(quad)[names(quad) != "pres"]
# sp_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
# sp_data <- presabs_data
# 
# # create quadratic terms and sparse matrix (all Europe)
# quad_obj <- make_quadratic(europe_data, cols = covars)
# quad <- predict.make_quadratic(quad_obj, newdata = europe_data)
# new_vars <- names(quad)[names(quad) != "pres"]
# all_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
# all_data <- europe_data 

# dataframe to combine test predictions
# (as in Valavi et al. (2023), we accumulate predictions to test data)
accumulate_data <- data.frame(pres = model_data$pres, pred = NA)


# 2. Run calibrations
cat("Lasso GLM calibrations\n\n")
cat("   Fivefold environmental cross-validation\n")
for(k in seq_len(length(folds))){
  cat(paste0("    - Fold ", k, "\n"))
  train_set <- unlist(folds[[k]][1]) # training set indices
  test_set <- unlist(folds[[k]][2]) # testing set indices
  
  train_data <- model_data[train_set, ]
  test_data <- model_data[test_set, ] 
  
  # create quadratic terms and sparse matrix (training data)
  quad_obj <- make_quadratic(train_data, cols = covars)
  quad <- predict.make_quadratic(quad_obj, newdata = train_data)
  new_vars <- names(quad)[names(quad) != "pres"]
  train_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
  
  # create quadratic terms and sparse matrix (testing data)
  # quad_obj <- make_quadratic(test_data, cols = covars)
  quad <- predict.make_quadratic(quad_obj, newdata = test_data)
  new_vars <- names(quad)[names(quad) != "pres"]
  test_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
  
  # calculate case weights (down-weighted)
  prNum <- as.numeric(table(train_data$pres)["1"]) # number of presences
  bgNum <- as.numeric(table(train_data$pres)["0"]) # number of backgrounds
  wt <- ifelse(train_data$pres == 1, 1, prNum / bgNum)
  
  # run model
  t0 <- proc.time()
  mod_lasso <- glmnet::cv.glmnet(x = train_sparse,
                                 y = train_data$pres,
                                 family = "binomial",
                                 alpha = 1,
                                 weights = wt,
                                 standardize = TRUE)
  runtime <- proc.time() - t0
  
  # predict test set
  test_data$pred <- as.numeric(predict(mod_lasso, test_sparse, type = "response", s = "lambda.min"))
  eval_obj <- evalmod(scores = test_data$pred, labels = test_data$pres)
  auc_test <- precrec::auc(eval_obj)
  cat(paste0("      Testing AUC = ", round(auc_test$aucs[1],2), "\n"))
  
  # adding test predictions
  accumulate_data[test_set, "pred"] <- test_data$pred
  
  # predict on all species points
  quad <- predict.make_quadratic(quad_obj, newdata = presabs_data)
  new_vars <- names(quad)[names(quad) != "pres"]
  sp_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
  sp_data <- presabs_data
  sp_data$pred <- as.numeric(predict(mod_lasso, sp_sparse, type = "response", s = "lambda.min"))
  eval_obj <- evalmod(scores = sp_data$pred, labels = sp_data$pres)
  auc_all <- precrec::auc(eval_obj)
  cat(paste0("      Total AUC = ", round(auc_all$aucs[1],2), "\n"))
  
  # best threshold
  youden_index <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure +
    specificity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure - 1
  thresholds <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$cutoffs
  best_threshold <- thresholds[which(youden_index == max(youden_index))]
  
  # predict on all Europe
  quad <- predict.make_quadratic(quad_obj, newdata = europe_data)
  new_vars <- names(quad)[names(quad) != "pres"]
  all_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
  all_data <- europe_data 
  all_data$pred <- as.numeric(predict(mod_lasso, all_sparse, type = "response", s = "lambda.min"))
  
  # save file
  outfile <- list()
  outfile$species <- sp_name # species name
  outfile$name <- "Lasso GLM (down-weighted)" # model name
  outfile$modality <- paste0("ecv - fold ", k) # modelling modality 
  outfile$fit_date <- Sys.Date() # date
  outfile$model <- mod_lasso # model object
  outfile$runtime <- runtime[3] # runtime
  outfile$auc_test <- auc_test # auc on testing set
  outfile$auc_all <- auc_all # auc on every species points
  outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
  outfile$europe_pred <- data.frame(lat = alt$lat, lon = alt$lon, pred = all_data$pred) # prediction on every Europe cells
  outfile$cov_norm <- FALSE # are covariates normalized before calibration ?
  outfile$meanv_l <- NULL # mean parameter list for normalization
  outfile$sdv_l <- NULL # standard deviation parameter list for normalization
  
  # save file
  dir.create(paste0(wd, "/lasso_glm/fit/", sp_name), showWarnings = FALSE)
  saveRDS(outfile, file = paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type, "_fold", k,".rds"))
  
  gc()
  
}

# combined set of test observations and predictions to evaluate the performance once
eval_obj <- evalmod(scores = accumulate_data$pred, labels = accumulate_data$pres)
auc_accumulate <- precrec::auc(eval_obj)
boyce_accumulate <- ecospat.boyce(fit = accumulate_data$pred, obs = accumulate_data[accumulate_data$pres == 1,]$pred, 
                                  nclass=0, window.w="default", res=100, 
                                  PEplot = F, rm.duplicate = F,  method = 'pearson' )
youden_index <- sensitivity(accumulate_data$pred, as.factor(accumulate_data$pres), perc.rank = F)$measure +
  specificity(accumulate_data$pred, as.factor(accumulate_data$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(accumulate_data$pred, as.factor(accumulate_data$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]
accumulate_data$pred_pres <- ifelse(accumulate_data$pred < best_threshold,0 , 1)
tp <- nrow(accumulate_data[accumulate_data$pred_pres == 1 & accumulate_data$pres == 1,])
fp <- nrow(accumulate_data[accumulate_data$pred_pres == 1 & accumulate_data$pres == 0,])
tn <- nrow(accumulate_data[accumulate_data$pred_pres == 0 & accumulate_data$pres == 0,])
fn <- nrow(accumulate_data[accumulate_data$pred_pres == 0 & accumulate_data$pres == 1,])
mig_sens = tp/(tp+fn)
mig_spec = tn/(tn+fp)
tss_accumulate  = mig_sens + mig_spec - 1
sorensen_accumulate = 2*tp/(fn + 2*tp + fp)


# 3. Full model (all the available training data is used)
# "this approach favours final prediction quality over perfect accuracy of error estimates", see Roberts et al. (2017)

full_data <- model_data
cat("\n   Full model\n")
cat(paste0("      AUC on combined set of test predictions = ", round(auc_accumulate$aucs[1],2),"\n"))
cat(paste0("      Boyce index on combined set of test predictions = ", round(boyce_accumulate$cor,2),"\n"))
cat(paste0("      TSS on combined set of test predictions = ", round(tss_accumulate,2),"\n"))
cat(paste0("      Sorensen index on combined set of test predictions = ", round(sorensen_accumulate,2),"\n"))

# create quadratic terms and sparse matrix (full data)
quad_obj <- make_quadratic(full_data, cols = covars)
quad <- predict.make_quadratic(quad_obj, newdata = full_data)
new_vars <- names(quad)[names(quad) != "pres"]
sparse <- sparse.model.matrix(~. -1, quad[, new_vars])

# calculate case weights (down-weighted)
prNum <- as.numeric(table(full_data$pres)["1"]) # number of presences
bgNum <- as.numeric(table(full_data$pres)["0"]) # number of backgrounds
wt <- ifelse(full_data$pres == 1, 1, prNum / bgNum)

# run model
t0 <- proc.time()
mod_lasso <- glmnet::cv.glmnet(x = sparse,
                               y = full_data$pres,
                               family = "binomial",
                               alpha = 1,
                               weights = wt,
                               standardize = TRUE)
runtime <- proc.time() - t0

# predict on all species points
quad <- predict.make_quadratic(quad_obj, newdata = presabs_data)
new_vars <- names(quad)[names(quad) != "pres"]
sp_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
sp_data <- presabs_data
sp_data$pred <- as.numeric(predict(mod_lasso, sp_sparse, type = "response", s = "lambda.min"))
eval_obj <- evalmod(scores = sp_data$pred, labels = sp_data$pres)
auc_all <- precrec::auc(eval_obj)
cat(paste0("      Total AUC = ", round(auc_all$aucs[1],2), "\n\n"))

# best threshold
youden_index <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure +
  specificity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

# tss
sp_data$pred_pres <- ifelse(sp_data$pred < best_threshold,0 , 1)
tp <- nrow(sp_data[sp_data$pred_pres == 1 & sp_data$pres == 1,])
fp <- nrow(sp_data[sp_data$pred_pres == 1 & sp_data$pres == 0,])
tn <- nrow(sp_data[sp_data$pred_pres == 0 & sp_data$pres == 0,])
fn <- nrow(sp_data[sp_data$pred_pres == 0 & sp_data$pres == 1,])
mig_sens = tp/(tp+fn)
mig_spec = tn/(tn+fp)
tss_all  = mig_sens + mig_spec - 1
sorensen_all = 2*tp/(fn + 2*tp + fp)
cat(paste0("      Total TSS = ", round(tss_all,2), "\n\n"))
cat(paste0("      Total Sorensen index = ", round(sorensen_all,2), "\n\n"))

# predict on all Europe
quad <- predict.make_quadratic(quad_obj, newdata = europe_data)
new_vars <- names(quad)[names(quad) != "pres"]
all_sparse <- sparse.model.matrix(~. -1, quad[, new_vars])
all_data <- europe_data 
all_data$pred <- as.numeric(predict(mod_lasso, all_sparse, type = "response", s = "lambda.min"))

# save file
outfile <- list()
outfile$species <- sp_name # species name
outfile$name <- "Lasso GLM (down-weighted)" # model name
outfile$modality <- "ecv - full model" # modelling modality 
outfile$fit_date <- Sys.Date() # date
outfile$model <- mod_lasso # model object
outfile$runtime <- runtime[3] # runtime
outfile$auc_test <- auc_accumulate # AUC on  combined set of test predictions
outfile$tss_test <- tss_accumulate # TSS on  combined set of test predictions
outfile$sorensen_test <- sorensen_accumulate # Sorensen on  combined set of test predictions
outfile$boyceindex_test <- boyce_accumulate # Boyce Index on  combined set of test predictions
outfile$auc_all <- auc_all # AUC on every species points
outfile$tss_all <- tss_all # TSS on every species points
outfile$sorensen_all <- sorensen_all # Sorensen on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- data.frame(lat = alt$lat, lon = alt$lon, pred = all_data$pred) # prediction on every Europe cells
outfile$cov_norm <- FALSE # are covariates normalized before calibration ?
outfile$meanv_l <- NULL # mean parameter list for normalization
outfile$sdv_l <- NULL # standard deviation parameter list for normalization
outfile$quad_obj <- quad_obj # only for Lasso GLM

saveRDS(outfile, file = paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type, "_fullmodel.rds"))

gc()
