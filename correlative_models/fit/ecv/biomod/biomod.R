
#----------------------------------------------------------------#
# Script to perform a Biomod ensemble as in Valavi et al. (2022) #
#----------------------------------------------------------------#

# this script is largely inspired by Valavi et al. R code 


# 1. Common parameters

dirname <- file.path(wd, "biomod", "fit")

# preparing data as required by biomod
myBiomodData <- myBiomodOption <- myBiomodModelOut <- NULL
myRespName <- "pres"
myResp <- as.numeric(model_data_norm[,myRespName])
myResp[which(myResp == 0)] <- NA
myExpl <- data.frame(model_data_norm[,covars]) # (use normalized covariates)
myRespXY <- presbg_points[, c("lon", "lat")]

# create biomod data format
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.name = myRespName,
                                     resp.xy = myRespXY,
                                     PA.nb.absences = 50000,
                                     PA.strategy = 'random',
                                     na.rm = TRUE,
                                     dir.name = dirname)

myBiomodData@data.species <- c(rep(1, length(which(myResp == 1))), rep(0, nb_background))

# all species points
sp_data <- presabs_data_norm # (use normalized covariates)

# all Europe
all_data <- europe_data_norm # (use normalized covariates)

# dataframe to combine test predictions
# (as in Valavi et al. (2023), we accumulate predictions to test data)
accumulate_data <- data.frame(pres = model_data$pres, pred = NA)

# set options for biomod using Valavi formula
myBiomodOption <- BIOMOD_ModelingOptions(
  CTA = list(rpart.control(cp = 0.001)),
  GAM = list( algo = 'GAM_mgcv',
              type = 's_smoother',
              k = -1,
              interaction.level = 0,
              myFormula = NULL, # will be set to: pres ~ s(bio6) + s(bio12) + s(WHC) + s(pH) + s(sum_apsep_GDD5) + s(w_bal)
              family = binomial(link = 'logit'),
              method = 'GCV.Cp',
              optimizer = c('outer','newton'),
              select = FALSE,
              knots = NULL,
              paraPen = NULL,
              control = list(nthreads = 1, irls.reg = 0, epsilon = 1e-07, maxit = 200, trace = FALSE,
                             mgcv.tol = 1e-07, mgcv.half = 15, rank.tol = 1.49011611938477e-08,
                             nlm = list(ndigit=7, gradtol=1e-06, stepmax=2, steptol=1e-04, iterlim=200, check.analyticals=0),
                             optim = list(factr=1e+07),
                             newton = list(conv.tol=1e-06, maxNstep=5, maxSstep=2, maxHalf=30, use.svd=0), outerPIsteps = 0,
                             idLinksBases = TRUE, scalePenalty = TRUE, keepData = FALSE, scale.est = "fletcher",
                             edge.correct = FALSE)),
  MAXENT = list( path_to_maxent.jar = "C:/Users/vandermeersch/Documents/R/R-4.1.2/library/dismo/java", # change it to maxent directory
                 memory_allocated = 512,
                 background_data_dir = 'default',
                 maximumbackground = 50000,
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = TRUE,
                 threshold = TRUE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1,
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 betamultiplier = 1,
                 defaultprevalence = 0.5)
  
)


# 2. Run calibrations
t0 <- proc.time()
# model fitting c('GLM','GBM','GAM','CTA','ANN','FDA','MARS','RF','MAXENT')
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    models = c('GLM','GBM','GAM','CTA','ANN','FDA',  'RF','MAXENT', 'MARS'),
                                    bm.options = myBiomodOption,
                                    data.split.table = e_clust$biomod_table,
                                    metric.eval = c("ROC"), 
                                    save.output = TRUE,
                                    scale.models = FALSE,
                                    modeling.id = paste0(cov_type,"_", Sys.Date()),
                                    nb.cpu = 1, seed.val = 1996)
myBiomodEval <- get_evaluations(myBiomodModelOut)
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = "PA_dataset+repet",
                                      metric.eval = c("ROC"),
                                      metric.select.thresh = NULL, 
                                      em.algo = c('EMmean'))
runtime <- proc.time() - t0
# note on em.by :
# - 'PA_dataset+repet': preserve the direct relationship between the models and the error estimates 
#   (i.e. the 'final models' are exactly those evaluated), see Roberts et al. 2017
# - 'all': ensemble_models will be evaluated in the full dataset

# combined set of test observations and predictions to evaluate the performance once
test_predictions <- get_predictions(myBiomodEM, model.as.col = T)
# keep only test predictions for each run
for(k in seq_len(length(folds))){
  train_set <- unlist(folds[[k]][1])
  test_predictions[train_set,k] <- NA 
}
accumulate_data$pred <- rowMeans(test_predictions, na.rm = T)
eval_obj <- evalmod(scores = accumulate_data$pred, labels = accumulate_data$pres)
auc_accumulate <- precrec::auc(eval_obj)
boyce_accumulate <- ecospat.boyce(fit = accumulate_data$pred, obs = accumulate_data[accumulate_data$pres == 1,]$pred, 
                                  nclass=0, window.w="default", res=100, 
                                  PEplot = F, rm.duplicate = F,  method = 'pearson' )

# save file
outfile <- list()
outfile$species <- sp_name # species name
outfile$name <- "Biomod" # model name
outfile$modality <- paste0("ecv - folds") # modelling modality 
outfile$fit_date <- Sys.Date() # date
outfile$model_parts <- myBiomodModelOut 
outfile$model <- myBiomodEM # model object (ensemble)
outfile$runtime <- runtime[3] # runtime
outfile$auc_test <- auc_accumulate # auc on testing set
outfile$auc_all <- NULL # auc on every species points
outfile$best_threshold <- NULL # best threshold to discriminate probabilites
outfile$europe_pred <- NULL # prediction on every Europe cells
outfile$cov_norm <- TRUE # are covariates normalized before calibration ?
outfile$meanv_l <- meanv_l  # mean parameter list for normalization
outfile$sdv_l <- sdv_l # standard deviation parameter list for normalization

# save file
dir.create(paste0(wd, "/biomod/fit/", sp_name), showWarnings = FALSE)
saveRDS(outfile, file = paste0(wd, "/biomod/fit/", sp_name, "/biomod_", cov_type, "_folds.rds"))

gc()



# 3. Full model (all the available training data is used)
# "this approach favours final prediction quality over perfect accuracy of error estimates", see Roberts et al. (2017)

cat("\n   Full model\n")
cat(paste0("      AUC on combined set of test predictions = ", round(auc_accumulate$aucs[1],2),"\n"))
cat(paste0("      Boyce index on combined set of test predictions = ", round(boyce_accumulate$cor,2),"\n"))

# run model
t0 <- proc.time()
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    models = c('GLM','GAM', 'CTA','GBM', 'ANN','FDA',  'RF','MAXENT', 'MARS'), 
                                    bm.options = myBiomodOption,
                                    nb.rep = 1, 
                                    data.split.perc = 100,
                                    metric.eval = c("ROC"), 
                                    save.output = TRUE,
                                    scale.models = FALSE,
                                    do.full.models = TRUE,
                                    modeling.id = paste0(cov_type,"_", Sys.Date()),
                                    nb.cpu = 1)
myBiomodEval <- get_evaluations(myBiomodModelOut)
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = "all",
                                      metric.eval = 'ROC',
                                      metric.select.thresh = NULL, 
                                      em.algo = c('EMmean'))


runtime <- proc.time() - t0

# predict on all species points
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  new.env = as.data.frame(sp_data[, covars]),
                                  proj.name = "valavietal",
                                  selected.models = "all",
                                  binary.meth = "ROC",
                                  compress = TRUE,
                                  clamping.mask = TRUE)
myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                             bm.em = myBiomodEM,
                                             selected.models = "all")
myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
sp_data$pred <- myEnProjDF$pred
eval_obj <- evalmod(scores = sp_data$pred, labels = sp_data$pres)
auc_all <- precrec::auc(eval_obj)
cat(paste0("      Total AUC = ", round(auc_all$aucs[1],2), "\n\n"))

# best threshold
youden_index <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure +
  specificity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(sp_data$pred, as.factor(sp_data$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

# predict on all Europe
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  new.env = as.data.frame(all_data[, covars]),
                                  proj.name = "valavietal",
                                  selected.models = "all",
                                  binary.meth = "ROC",
                                  compress = TRUE,
                                  clamping.mask = TRUE)
myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                             bm.em = myBiomodEM,
                                             selected.models = "all")
myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
all_data$pred <- myEnProjDF$pred

# save file
outfile <- list()
outfile$species <- sp_name # species name
outfile$name <- "Biomod" # model name
outfile$modality <- "ecv - full model" # modelling modality 
outfile$fit_date <- Sys.Date() # date
outfile$model_parts <- myBiomodModelOut 
outfile$model <- myBiomodEM # model object (ensemble)
outfile$runtime <- runtime[3] # runtime
outfile$auc_test <- auc_accumulate # auc on  combined set of test predictions
outfile$boyceindex_test <- boyce_accumulate # Boyce Index on  combined set of test predictions
outfile$auc_all <- auc_all # auc on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- data.frame(lat = alt$lat, lon = alt$lon, pred = all_data$pred) # prediction on every Europe cells
outfile$cov_norm <- TRUE # are covariates normalized before calibration ?
outfile$meanv_l <- meanv_l  # mean parameter list for normalization
outfile$sdv_l <- sdv_l # standard deviation parameter list for normalization

saveRDS(outfile, file = paste0(wd, "/biomod/fit/", sp_name, "/biomod_", cov_type, "_fullmodel.rds"))

gc()


