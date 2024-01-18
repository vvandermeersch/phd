
#---------------------------------------#
# Process model outputs, with migration #
#---------------------------------------#

# extent <- c(-10,30,36,66) extent use during migration simulation (to limit simulation runtime)
# locations with lat > 66 will be considered as not colonized (never suitable in any model)


#----------#
# 1. Fagus #
#----------#

fagus_models <- data.frame(
  name = c(
    "BRT",
    "Random Forest",
    "GAM",
    "GLM",
    "PHENOFIT",
    "CASTANEA",
    "PHENOFIT (fitted)",
    "CASTANEA (fitted)"),
  type = c(
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "3Expertprocessbased",
    "3Expertprocessbased",
    "2Fittedprocessbased",
    "2Fittedprocessbased"),
  simfolder = c(
    paste0(wd, "/data/simulations/migration/csdm/brt/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/random_forest/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/gam/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/lasso_glm/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/expert/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/expert/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/fitted/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/fitted/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000")))
fagus_performance <- load_model_performance(
  models = fagus_models, 
  years = c(seq(500,8500, 500)), 
  pollen_folder = "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc")
fagus_performance$species <- "fagus"


#----------#
# 2. Abies #
#----------#

abies_models <- data.frame(
  name = c(
    "BRT",
    "Random Forest",
    "GAM",
    "GLM",
    "PHENOFIT",
    "CASTANEA",
    "PHENOFIT (fitted)",
    "CASTANEA (fitted)"),
  type = c(
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "3Expertprocessbased",
    "3Expertprocessbased",
    "2Fittedprocessbased",
    "2Fittedprocessbased"),
  simfolder = c(
    paste0(wd, "/data/simulations/migration/csdm/brt/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/random_forest/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/gam/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/lasso_glm/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/expert/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/expert/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/fitted/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/fitted/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000")))
abies_performance <- load_model_performance(
  models = abies_models, 
  years = c(seq(500,11000, 500)), 
  pollen_folder = "D:/species/pollen/processed/abies/025deg/001thr_500yrunc")
abies_performance$species <- "abies"


#----------------------#
# 3. Quercus deciduous #
#----------------------#

quercusdeciduous_models <- data.frame(
  name = c(
    "BRT",
    "Random Forest",
    "GAM",
    "GLM",
    "PHENOFIT",
    "CASTANEA",
    "PHENOFIT (fitted)",
    "CASTANEA (fitted)"),
  type = c(
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "3Expertprocessbased",
    "3Expertprocessbased",
    "2Fittedprocessbased",
    "2Fittedprocessbased"),
  simfolder = c(
    paste0(wd, "/data/simulations/migration/csdm/brt/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/random_forest/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/gam/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/lasso_glm/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000")))
quercusdeciduous_performance <- load_model_performance(
  models = quercusdeciduous_models, 
  years = c(seq(500,11500, 500)), 
  pollen_folder = "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc",
  add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc",
  evergreen = FALSE)
quercusdeciduous_performance$species <- "quercusdeciduous"


#----------------------#
# 4. Quercus evergreen #
#----------------------#

quercusevergreen_models <- data.frame(
  name = c(
    "BRT",
    "Random Forest",
    "GAM",
    "GLM",
    "PHENOFIT",
    "CASTANEA",
    "PHENOFIT (fitted)",
    "CASTANEA (fitted)"),
  type = c(
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "3Expertprocessbased",
    "3Expertprocessbased",
    "2Fittedprocessbased",
    "2Fittedprocessbased"),
  simfolder = c(
    paste0(wd, "/data/simulations/migration/csdm/brt/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/random_forest/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/gam/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/lasso_glm/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/expert/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/expert/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/fitted/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/fitted/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000")))
quercusevergreen_performance <- load_model_performance(
  models = quercusevergreen_models, 
  years = c(seq(500,11500, 500)), 
  pollen_folder = "D:/species/pollen/processed/quercus_evergreentype/025deg/0025thr_500yrunc",
  add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc",
  evergreen = TRUE)
quercusevergreen_performance$species <- "quercusevergreen"


#---------#
# Gather! #
#---------#

model_performance_withmig <- rbind(
  fagus_performance, abies_performance, quercusdeciduous_performance, quercusevergreen_performance)

# Join with past climatic dissimiliarity metrics
model_performance_withmig <- left_join(model_performance_withmig, past_climdiss, by = c("year" = "clim_hpv_sorensen.year" ))

