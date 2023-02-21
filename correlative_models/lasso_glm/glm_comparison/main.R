#####################################
# Script to perform GLM comparisons #
#####################################

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models"

# species settings
sp_name <- "fagus_sylvatica"
nb_pres <- 1000
nb_background <- 50000
subset <- 1

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

# Lasso GLM with all custom predictors and presence-only data
cov_type <- "custpred_all"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_custpred_all <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with custom predictors except WHC and presence-only data
cov_type <- "custpred_wo_WHC"
bc_covars <- c()
soil_covars <- c()
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_custpred_wo_WHC <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with custom predictors except ndays with Tmin<-5 and presence-only data
cov_type <- "custpred_wo_tmin_5"
bc_covars <- c()
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_custpredwo_tmin_5 <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))

# Lasso GLM with custom predictors + bio6 and presence-only data
cov_type <- "custpred_w_bio6"
bc_covars <- c("bio6")
soil_covars <- c("WHC")
cc_covars <- c("sum_GDD", "nd_5deg", "nd_neg5deg", "w_bal")
source(file.path(wd, "lasso_glm", "lasso_glm_presenceonly.R"))
lasso_custpred_w_bio6 <- readRDS(paste0(wd, "/lasso_glm/fit/", sp_name, "/lasso_glm_", cov_type,"_", Sys.Date(), ".rds"))


