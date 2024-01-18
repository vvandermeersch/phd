#---------------------------------------#
# Process model outputs, with migration #
#---------------------------------------#

# extent <- c(-10,30,36,66) extent use during migration simulation (to limit simulation runtime)
# locations with lat > 66 will be considered as not colonized

#----------#
# 1. Fagus #
#----------#

years <- c(seq(500,12000, 500))
models <- data.frame(name = c("PHENOFIT (fitted)",
                              "PHENOFIT",
                              "Random Forest",
                              "BRT",
                              "GAM",
                              "Lasso GLM",
                              "CASTANEA"),
                     type = c("Fittedprocessbased",
                              "Expertprocessbased",
                              "Correlative",
                              "Correlative",
                              "Correlative",
                              "Correlative",
                              "Expertprocessbased"),
                     type2 = c("Fittedprocessbased",
                               "Expertprocessbased",
                               "Treebased",
                               "Treebased",
                               "Regressionbased",
                               "Regressionbased",
                               "Expertprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_scprb_2km20km"))
source(file.path(wd, "scripts", "data_processing", "load_model_area.R"))
fagus_area <- model_area
fagus_area$species <- "fagus"



#----------#
# 2. Abies #
#----------#

years <- c(seq(500,12000, 500))
models <- data.frame(name = c("PHENOFIT (fitted)",
                              "PHENOFIT",
                              "Random Forest",
                              "GAM",
                              "BRT",
                              "Lasso GLM",
                              "CASTANEA",
                              "CASTANEA (fitted)"),
                     type = c("Fittedprocessbased",
                              "Expertprocessbased",
                              "Correlative",
                              "Correlative",
                              "Correlative",
                              "Correlative",
                              "Expertprocessbased",
                              "Fittedprocessbased"),
                     type2 = c("Fittedprocessbased",
                               "Expertprocessbased",
                               "Regressionbased",
                               "Treebased",
                               "Regressionbased",
                               "Treebased",
                               "Expertprocessbased",
                               "Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/expert/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/fitted/abies_alba_expandLDD_from12k_scprb_2km20km"))
source(file.path(wd, "scripts", "data_processing", "load_model_area.R"))
abies_area <- model_area
abies_area$species <- "abies"



#------------#
# 3. Quercus #
#------------#

years <- c(seq(500,15000, 500))
models <- data.frame(name = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "Random Forest",
                              "BRT",
                              "GAM",
                              "Lasso GLM",
                              "CASTANEA"),
                     type = c("Expertprocessbased",
                              "Fittedprocessbased",
                              "Correlative",
                              "Correlative",
                              "Correlative",
                              "Correlative",
                              "Expertprocessbased"),
                     type2 = c("Expertprocessbased",
                               "Fittedprocessbased",
                               "Treebased",
                               "Treebased",
                               "Regressionbased",
                               "Regressionbased",
                               "Expertprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/expert/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/fitted/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/quercus_expandLDD_from15k_scprb_2km20km"))
source(file.path(wd, "scripts", "data_processing", "load_model_area.R"))
quercus_area <- model_area
quercus_area$species <- "quercus"

model_area <- rbind(fagus_area, abies_area, quercus_area)
model_area <- left_join(model_area, burke_climatenovelty, by = c("year"))
