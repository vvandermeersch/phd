
#-----------------------------------------------------------------#
# Script to perform an equal-sample RF as in Valavi et al. (2021) #
#-----------------------------------------------------------------#

# see https://rvalavi.github.io/SDMwithRFs/

# equal-sample approach : the model is fitted on 10 datasets each with the same number of presences and background samples
# this script is largely inspired by Valavi et al. R code 

# 1. Common parameters
ntrees <- 1000

# all species points
sp_data <- presabs_data_norm # (use normalized covariates)

# all Europe
all_data <- europe_data_norm # (use normalized covariates)

# dataframe to combine test predictions
# (as in Valavi et al. (2023), we accumulate predictions to test data)
accumulate_data <- data.frame(pres = model_data$pres, pred = NA)


# 2. Run calibrations
cat("Starting Random Forest calibrations...\n\n")
cat("   Fivefold environmental cross-validation\n")
for(k in seq_len(length(folds))){
  cat(paste0("    - Fold ", k, "\n"))
  train_set <- unlist(folds[[k]][1]) # training set indices
  test_set <- unlist(folds[[k]][2]) # testing set indices
  
  train_data <- model_data_norm[train_set, ] # (use normalized covariates)
  test_data <- model_data_norm[test_set, ] 
  
  # convert the response to factor for RF model to return probabilities
  train_data$pres <- as.factor(train_data$pres)
  test_data$pres <- as.factor(test_data$pres)
  
  # equal number of presence and background points, 10 repeats
  prNum <- as.numeric(table(train_data$pres)["1"]) # number of presences
  
  # run models
  t0 <- proc.time()
  rf_eql_models <- list()
  for(t in 1:10){
    eqsamp <- c(which(train_data$pres == 1), sample(which(train_data$pres == 0), prNum))
    rf_eql <- randomForest(pres ~ .,
                           data = train_data[eqsamp,],
                           ntree = 1000)
    rf_eql_models[[t]] <- rf_eql
  }
  runtime <- proc.time() - t0
  
  # predict test set
  rf_eql_preds <- lapply(rf_eql_models, function(x) as.numeric(predict(x, test_data, type = "prob")[,"1"]))
  test_data$pred <- do.call("cbind",rf_eql_preds) %>% rowMeans()
  eval_obj <- evalmod(scores = test_data$pred, labels = test_data$pres)
  auc_test <- precrec::auc(eval_obj)
  cat(paste0("      Testing AUC = ", round(auc_test$aucs[1],2), "\n"))
  
  # adding test predictions
  accumulate_data[test_set, "pred"] <- test_data$pred
  
  # predict on all species points
  rf_eql_preds <- lapply(rf_eql_models, function(x) as.numeric(predict(x, sp_data, type = "prob")[,"1"]))
  sp_data$pred <- do.call("cbind",rf_eql_preds) %>% rowMeans()
  eval_obj <- evalmod(scores = sp_data$pred, labels = sp_data$pres)
  auc_all <- precrec::auc(eval_obj)
  cat(paste0("      Total AUC = ", round(auc_all$aucs[1],2), "\n"))
  
  # best threshold
  youden_index <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure +
    specificity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure - 1
  thresholds <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$cutoffs
  best_threshold <- thresholds[which(youden_index == max(youden_index))]
  
  # predict on all Europe
  rf_eql_preds <- lapply(rf_eql_models, function(x) as.numeric(predict(x, all_data, type = "prob")[,"1"]))
  all_data$pred <- do.call("cbind",rf_eql_preds) %>% rowMeans()
  
  # create file
  outfile <- list()
  outfile$species <- sp_name # species name
  outfile$name <- "Random Forest (equal-sampled)" # model name
  outfile$modality <- paste0("ecv - fold ", k) # modelling modality 
  outfile$fit_date <- Sys.Date() # date
  outfile$model <- NULL # model object
  outfile$runtime <- runtime[3] # runtime
  outfile$auc_test <- auc_test # auc on testing set
  outfile$auc_all <- auc_all # auc on every species points
  outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
  outfile$europe_pred <- data.frame(lat = alt$lat, lon = alt$lon, pred = all_data$pred) # prediction on every Europe cells
  outfile$cov_norm <- TRUE # are covariates normalized before calibration ?
  outfile$meanv_l <- meanv_l # mean parameter list for normalization
  outfile$sdv_l <- sdv_l # standard deviation parameter list for normalization
  
  # save file
  dir.create(paste0(wd, "/random_forest_es/fit/", sp_name), showWarnings = FALSE)
  saveRDS(outfile, file = paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_", cov_type, "_fold", k,".rds"))
  
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


# 3. Full model (all the available training data is used)
# "this approach favours final prediction quality over perfect accuracy of error estimates", see Roberts et al. (2017)

full_data <- model_data_norm # (use normalized covariates)
cat("\n   Full model\n")
cat(paste0("      AUC on  combined set of test predictions = ", round(auc_accumulate$aucs[1],2),"\n"))
cat(paste0("      Boyce index on combined set of test predictions = ", round(boyce_accumulate$cor,2),"\n"))
cat(paste0("      TSS on combined set of test predictions = ", round(tss_accumulate,2),"\n"))

# convert the response to factor for RF model to return probabilities
full_data$pres <- as.factor(full_data$pres)

# equal number of presence and background points, 10 repeats
prNum <- as.numeric(table(train_data$pres)["1"]) # number of presences

# run models
t0 <- proc.time()
rf_eql_models <- list()
for(t in 1:10){
  eqsamp <- c(which(full_data$pres == 1), sample(which(full_data$pres == 0), prNum))
  rf_eql <- randomForest(pres ~ .,
                         data = full_data,
                         ntree = 1000)
  rf_eql_models[[t]] <- rf_eql
}
runtime <- proc.time() - t0

# predict on all species points
rf_eql_preds <- lapply(rf_eql_models, function(x) as.numeric(predict(x, sp_data, type = "prob")[,"1"]))
sp_data$pred <- do.call("cbind",rf_eql_preds) %>% rowMeans()
eval_obj <- evalmod(scores = sp_data$pred, labels = sp_data$pres)
auc_all <- precrec::auc(eval_obj)
cat(paste0("      Total AUC = ", round(auc_all$aucs[1],2), "\n"))

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
cat(paste0("      Total TSS = ", round(tss_all,2), "\n\n"))

# predict on all Europe
rf_eql_preds <- lapply(rf_eql_models, function(x) as.numeric(predict(x, all_data, type = "prob")[,"1"]))
all_data$pred  <- do.call("cbind",rf_eql_preds) %>% rowMeans()

# save file
outfile <- list()
outfile$species <- sp_name # species name
outfile$name <- "Random Forest (equal-sampled)" # model name
outfile$modality <- "ecv - full model" # modelling modality 
outfile$fit_date <- Sys.Date() # date
outfile$model <- rf_eql_models # model object (10 models)
outfile$runtime <- runtime[3] # runtime
outfile$auc_test <- auc_accumulate # AUC on  combined set of test predictions
outfile$tss_test <- tss_accumulate # TSS on  combined set of test predictions
outfile$boyceindex_test <- boyce_accumulate # Boyce Index on  combined set of test predictions
outfile$auc_all <- auc_all # AUC on every species points
outfile$tss_all <- tss_all # TSS on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- data.frame(lat = alt$lat, lon = alt$lon, pred = all_data$pred) # prediction on every Europe cells
outfile$cov_norm <- TRUE # are covariates normalized before calibration ?
outfile$meanv_l <- meanv_l # mean parameter list for normalization
outfile$sdv_l <- sdv_l # standard deviation parameter list for normalization

saveRDS(outfile, file = paste0(wd, "/random_forest_es/fit/", sp_name, "/random_forest_", cov_type, "_fullmodel.rds"))

gc()