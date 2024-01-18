#---------------------------------------#
# Process model outputs, with migration #
#---------------------------------------#

# extent <- c(-10,30,36,66) extent use during migration simulation (to limit simulation runtime)
# locations with lat > 66 will be considered as not colonized

#----------#
# 1. Fagus #
#----------#

pollen_folder <- "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc"
years <- c(seq(500,8500, 500))
brk <- 2000
models <- data.frame(name = c("7PHENOFIT(fitted)",
                              "5PHENOFIT",
                              "2RandomForest",
                              "1BRT",
                              "3GAM",
                              "4LassoGLM",
                              "6CASTANEA",
                              "8CASTANEA(fitted)"),
                     type = c("4Fittedprocessbased",
                              "3Expertprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "3Expertprocessbased",
                              "4Fittedprocessbased"),
                     type2 = c("4Fittedprocessbased",
                              "3Expertprocessbased",
                              "1Treebased",
                              "1Treebased",
                              "2Regressionbased",
                              "2Regressionbased",
                              "3Expertprocessbased",
                              "4Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_scprb_2km20km_240ppm",
                                   "D:/simulations/castanea/paleo/migration/fitted/fagus_sylvatica_expandLDD_from12k_scprb_2km20km_240ppm"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
fagus_performance <- model_performance
fagus_performance$species <- "fagus"



#----------#
# 2. Abies #
#----------#

pollen_folder <- "D:/species/pollen/processed/abies/025deg/001thr_500yrunc"
years <- c(seq(500,11000, 500))
brk <- 2000
models <- data.frame(name = c("7PHENOFIT(fitted)",
                              "5PHENOFIT",
                              "2RandomForest",
                              "3GAM",
                              "1BRT",
                              "4LassoGLM",
                              "6CASTANEA",
                              "8CASTANEA(fitted)"),
                     type = c("4Fittedprocessbased",
                              "3Expertprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "3Expertprocessbased",
                              "4Fittedprocessbased"),
                     type2 = c("4Fittedprocessbased",
                               "3Expertprocessbased",
                               "1Treebased",
                               "2Regressionbased",
                               "1Treebased",
                               "2Regressionbased",
                               "3Expertprocessbased",
                               "4Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/expert/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/fitted/abies_alba_expandLDD_from12k_scprb_2km20km_240ppm"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
abies_performance <- model_performance
abies_performance$species <- "abies"

model_performance_withmig <- rbind(fagus_performance, abies_performance)
model_performance_withmig <- left_join(model_performance_withmig, burke_climatenovelty, by = c("year"))



#------------#
# 3. Quercus #
#------------#

pollen_folder <- "D:/species/pollen/processed/quercus/025deg/0025thr_500yrunc"
years <- c(seq(500,11000, 500))
brk <- 2000
models <- data.frame(name = c("5PHENOFIT",
                              "7PHENOFIT(fitted)",
                              "2RandomForest",
                              "1BRT",
                              "3GAM",
                              "4LassoGLM",
                              "6CASTANEA",
                              "8CASTANEA(fitted)"),
                     type = c("3Expertprocessbased",
                              "4Fittedprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "3Expertprocessbased",
                              "4Fittedprocessbased"),
                     type2 = c("3Expertprocessbased",
                               "4Fittedprocessbased",
                               "1Treebased",
                               "1Treebased",
                               "2Regressionbased",
                               "2Regressionbased",
                               "3Expertprocessbased",
                               "4Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/expert/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/fitted/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/quercus_expandLDD_from15k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/quercus_expandLDD_from15k_scprb_2km20km_240ppm",
                                   "D:/simulations/castanea/paleo/migration/fitted/quercus_expandLDD_from15k_scprb_2km20km_240ppm"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
quercus_deciduous_performance <- model_performance
quercus_deciduous_performance$species <- "quercus"

model_performance_withmig <- rbind(fagus_performance, abies_performance, quercus_deciduous_performance)
model_performance_withmig <- left_join(model_performance_withmig, burke_climatenovelty, by = c("year"))

hypervolume_similarity <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/hypervolume_similarity_CRUbaseline.rds")
names(hypervolume_similarity) <- c("year", "hypervolume_sorensen", "hypervolume_jaccard")
model_performance_withmig <- left_join(model_performance_withmig, hypervolume_similarity, by = c("year"))
