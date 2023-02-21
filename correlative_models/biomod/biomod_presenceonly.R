##################################################################
# Script to perform a Biomod ensemble as in Valavi et al. (2022) #
##################################################################

# this script is largely inspired by Valavi et al. R code 

# load packages and functions
library(biomod2)
library(rpart)
library(AUC)
library(data.table)

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

# biomod store out data in the working directory
# setwd(paste0(wd, "/biomod/fit/", sp_name))
dirname <- paste0(wd, "/biomod/fit/", sp_name)

# preparing data as required by biomod
myBiomodData <- myBiomodOption <- myBiomodModelOut <- NULL
myRespName <- "pres"
myResp <- as.numeric(model_data[,myRespName])
myResp[which(myResp == 0)] <- NA
myExpl <- data.frame(model_data[,covars])
myRespXY <- pr_bg[, c("lon", "lat")]

# create biomod data format
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.name = myRespName,
                                     resp.xy = myRespXY,
                                     PA.nb.absences = 50000,
                                     PA.strategy = 'random',
                                     na.rm = TRUE,
                                     dir.name = dirname)

myBiomodData@data.species <- c(rep(1, 1000), rep(0, 50000))

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
  MAXENT.Phillips = list( path_to_maxent.jar = "C:/Users/vandermeersch/Documents/R/R-4.1.2/library/dismo/java", # change it to maxent directory
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

t0 <- proc.time()
# model fitting c('GLM','GBM','GAM','CTA','ANN','FDA','MARS','RF','MAXENT.Phillips')
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    models = c('GLM','GBM','GAM','CTA','ANN','FDA','MARS','RF','MAXENT.Phillips'),
                                    bm.options = myBiomodOption,
                                    nb.rep = 0, 
                                    data.split.perc = 100, 
                                    metric.eval = c("ROC"), 
                                    save.output = TRUE,
                                    scale.models = FALSE,
                                    do.full.models = TRUE,
                                    modeling.id = paste0(cov_type,"_", Sys.Date()),
                                    nb.cpu = 1)
# ensemble modelling using mean probability
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      metric.eval = c("ROC"),
                                      metric.select.thresh = NULL, 
                                      prob.mean = TRUE,
                                      prob.cv = FALSE,
                                      prob.ci = FALSE,
                                      prob.median = FALSE,
                                      committee.averaging = FALSE,
                                      prob.mean.weight = FALSE)
runtime <- proc.time() - t0




# evaluation
## prediction on calibration points
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  new.env = as.data.frame(model_data[, covars]),
                                  proj.name = "valavietal",
                                  selected.models = "all",
                                  binary.meth = "ROC",
                                  compress = TRUE,
                                  clamping.mask = TRUE)
myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                             bm.em = myBiomodEM,
                                             selected.models = "all")
myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
prediction_cal <- myEnProjDF[,1]
## prediction on all pres/abs points
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  new.env = as.data.frame(testing_data[, covars]),
                                  proj.name = "valavietal",
                                  selected.models = "all",
                                  binary.meth = "ROC",
                                  compress = TRUE,
                                  clamping.mask = TRUE)
myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                             bm.em = myBiomodEM,
                                             selected.models = "all")
myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
prediction_tot <- myEnProjDF[,1]

## AUC
auc_cal <- auc(roc(prediction_cal, as.factor(model_data$pres)))
auc_tot <- auc(roc(prediction_tot, as.factor(presabs_points$pres)))
## Best threshold
youden_index <- sensitivity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$measure +
  specificity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(prediction_tot, as.factor(presabs_points$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

## prediction over all Europe
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  new.env = as.data.frame(europe_data[, covars]),
                                  proj.name = "valavietal",
                                  selected.models = "all",
                                  binary.meth = "ROC",
                                  compress = TRUE,
                                  clamping.mask = TRUE)
myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                             bm.em = myBiomodEM,
                                             selected.models = "all")
myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
europe_prediction <- myEnProjDF[,1]

# save file
outfile <- list()
outfile$covars <- covars
outfile$auc_cal <- auc_cal
outfile$auc_tot <- auc_tot
outfile$runtime <- runtime[3]
outfile$best_threshold <- best_threshold
outfile$type = "biomod"
outfile$models <- myBiomodModelOut
outfile$model_ensemble <- myBiomodEM
outfile$occ_data <- occ_data
outfile$meanv_l <- meanv_l
outfile$sdv_l <- sdv_l
outfile$europe_prediction <- europe_prediction
dir.create(paste0(wd, "/biomod/fit/", sp_name), showWarnings = FALSE)
saveRDS(outfile, file = paste0(wd, "/biomod/fit/", sp_name, "/biomod_", cov_type,"_", Sys.Date(), ".rds"))




