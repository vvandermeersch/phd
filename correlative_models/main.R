############################################
# Script to perform all correlative models #
############################################


wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models"


# species settings
sp_name <- "fagus_sylvatica"
nb_pres <- 1000
nb_background <- 50000
subset <- 1

#-----#
# GAM #
#-----#

# GAM with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "gam", "gam_presenceonly.R"))
gam_defpred <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_", cov_type,"_", Sys.Date(), ".rds"))

# GAM with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "gam", "gam_presenceonly.R"))
gam_custpred <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_", cov_type,"_", Sys.Date(), ".rds"))

# GAM with default predictors and presence-(pseudo-)absence data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "gam", "gam_presenceabsence.R"))
gam_pa_defpred <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_pa_", cov_type,"_", Sys.Date(), ".rds"))

# GAM with custom predictors and presence-(pseudo-)absence data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "gam", "gam_presenceabsence.R"))
gam_pa_custpred <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_pa_", cov_type,"_", Sys.Date(), ".rds"))


# GAM with literature predictors and presence-(pseudo-)absence data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH", "carbon")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "gam", "gam_presenceabsence.R"))
gam_pa_litpred <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_pa_", cov_type,"_", Sys.Date(), ".rds"))

# GAM with literature predictors and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "gam", "gam_presenceonly.R"))
gam_litpred <- readRDS(paste0(wd, "/gam/fit/", sp_name, "/gam_", cov_type,"_", Sys.Date(), ".rds"))


#-----------#
# Lasso GLM #
#-----------#

# Lasso GLM with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_defpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_custpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with default predictors and presence-(pseudo-)absence data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "lasso_glm", "lasso_glm_presenceabsence.R"))
lasso_pa_defpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with custom predictors and presence-(pseudo-)absence data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceabsence.R"))
lasso_pa_custpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with custom predictors and presence-(pseudo-)absence data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceabsence.R"))
lasso_pa_custpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with literature predictors and presence-(pseudo-)absence data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH", "carbon")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceabsence.R"))
lasso_pa_litpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with literature predictors and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_litpred <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))


#--------#
# MaxEnt #
#--------#

# MaxEnt with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "maxent", "maxent.R"))
maxent_defpred <- readRDS(paste0(wd, "/maxent/fit/", sp_name, "/maxent_", cov_type,"_", Sys.Date(), ".rds"))

# MaxEnt with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "maxent", "maxent.R"))
maxent_custpred <- readRDS(paste0(wd, "/maxent/fit/", sp_name, "/maxent_", cov_type,"_", Sys.Date(), ".rds"))

# MaxEnt with literature predictors and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "maxent", "maxent.R"))
maxent_litpred <- readRDS(paste0(wd, "/maxent/fit/", sp_name, "/maxent_", cov_type,"_", Sys.Date(), ".rds"))

#---------------#
# Random Forest #
#---------------#

# Random Forest with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "random_forest", "random_forest_presenceonly.R"))
rf_defpred <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_", cov_type,"_", Sys.Date(), ".rds"))

# Random Forest with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "random_forest", "random_forest_presenceonly.R"))
rf_custpred <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_", cov_type,"_", Sys.Date(), ".rds"))

# Random Forest with default predictors and presence-(pseudo-)absence data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "random_forest", "random_forest_presenceabsence.R"))
rf_pa_defpred <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Random Forest with custom predictors and presence-(pseudo-)absence data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "random_forest", "random_forest_presenceabsence.R"))
rf_pa_custpred <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Random Forest with literature predictors and presence-(pseudo-)absence data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH", "carbon")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "random_forest", "random_forest_presenceabsence.R"))
rf_pa_litpred <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_pa_", cov_type,"_", Sys.Date(), ".rds"))

# Random Forest with literature predictor and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "random_forest", "random_forest_presenceonly.R"))
rf_litpred <- readRDS(paste0(wd, "/random_forest/fit/", sp_name, "/random_forest_", cov_type,"_", Sys.Date(), ".rds"))

#-------------------------#
# Boosted regression tree #
#-------------------------#

# BRT with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "brt", "brt_presenceonly.R"))
brt_defpred <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_", cov_type,"_", Sys.Date(), ".rds"))

# BRT with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "brt", "brt_presenceonly.R"))
brt_custpred <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_", cov_type,"_", Sys.Date(), ".rds"))

# BRT with default predictors and presence-(pseudo-)absence data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "brt", "brt_presenceabsence.R"))
brt_pa_defpred <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_pa_", cov_type,"_", Sys.Date(), ".rds"))

# BRT with custom predictors and presence-(pseudo-)absence data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "brt", "brt_presenceabsence.R"))
brt_pa_custpred <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_pa_", cov_type,"_", Sys.Date(), ".rds"))

# BRT with literature predictors and presence-(pseudo-)absence data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH", "carbon")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "brt", "brt_presenceabsence.R"))
brt_pa_litpred <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_pa_", cov_type,"_", Sys.Date(), ".rds"))

# BRT with literature predictors and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "brt", "brt_presenceonly.R"))
brt_litpred <- readRDS(paste0(wd, "/brt/fit/", sp_name, "/brt_", cov_type,"_", Sys.Date(), ".rds"))

#-----------------#
# Biomod ensemble #
#-----------------#

# Biomod with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "biomod", "biomod_presenceonly.R"))
biomod_defpred <- readRDS(paste0(wd, "/biomod/fit/", sp_name, "/biomod_", cov_type,"_", Sys.Date(), ".rds"))

# Biomod with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "biomod", "biomod_presenceonly.R"))
biomod_custpred <- readRDS(paste0(wd, "/biomod/fit/", sp_name, "/biomod_", cov_type,"_", Sys.Date(), ".rds"))

# Biomod with literature predictors and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "biomod", "biomod_presenceonly.R"))
biomod_litpred <- readRDS(paste0(wd, "/biomod/fit/", sp_name, "/biomod_", cov_type,"_", Sys.Date(), ".rds"))

#-----------------#
# Custom ensemble #
#-----------------#
date <- "2022-12-14"

# Ensemble with default predictors and presence-only data
cov_type <- "defpred"
bc_covars <- c("bio5", "bio6", "bio4", "bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
cc_covars <- c()
source(file.path(wd, "custom_ensemble", "custom_ensemble.R"))
custom_ensemble_defpred <- readRDS(paste0(wd, "/custom_ensemble/fit/", sp_name, "/custom_ensemble_", cov_type,"_", date, ".rds"))

# Ensemble with custom predictors and presence-only data
cov_type <- "custpred"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "custom_ensemble", "custom_ensemble.R"))
custom_ensemble_custpred <- readRDS(paste0(wd, "/custom_ensemble/fit/", sp_name, "/custom_ensemble_", cov_type,"_", date, ".rds"))

# Ensemble with literature predictors and presence-only data
cov_type <- "litpred"
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
source(file.path(wd, "custom_ensemble", "custom_ensemble.R"))
custom_ensemble_litpred <- readRDS(paste0(wd, "/custom_ensemble/fit/", sp_name, "/custom_ensemble_", cov_type,"_", date, ".rds"))

