#---------------------------------------#
# Process model outputs, with migration #
#---------------------------------------#

# extent <- c(-10,30,36,66) extent use during migration simulation (to limit simulation runtime)
# locations with lat > 66 will be considered as not colonized

#----------#
# 1. Fagus #
#----------#

pollen_folder <- "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc"
add_pollen_folder <- NULL
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
                     type = c("3Fittedprocessbased",
                              "4Expertprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "4Expertprocessbased",
                              "3Fittedprocessbased"),
                     type2 = c("3Fittedprocessbased",
                               "4Expertprocessbased",
                               "1Treebased",
                               "1Treebased",
                               "2Regressionbased",
                               "2Regressionbased",
                               "4Expertprocessbased",
                               "3Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/phenofit/paleo/migration/expert/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from11750",
                                   "D:/simulations/csdm/random_forest/paleo/migration/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/brt/paleo/migration/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/gam/paleo/migration/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from11750",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from11750",
                                   "D:/simulations/castanea/paleo/migration/expert/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/castanea/paleo/migration/fitted/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
fagus_performance <- model_performance
fagus_performance$species <- "fagus"



#----------#
# 2. Abies #
#----------#

pollen_folder <- "D:/species/pollen/processed/abies/025deg/001thr_500yrunc"
add_pollen_folder <- NULL
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
                     type = c("3Fittedprocessbased",
                              "4Expertprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "4Expertprocessbased",
                              "3Fittedprocessbased"),
                     type2 = c("3Fittedprocessbased",
                               "4Expertprocessbased",
                               "1Treebased",
                               "2Regressionbased",
                               "1Treebased",
                               "2Regressionbased",
                               "4Expertprocessbased",
                               "3Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/phenofit/paleo/migration/expert/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/random_forest/paleo/migration/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/gam/paleo/migration/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/brt/paleo/migration/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/castanea/paleo/migration/expert/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/castanea/paleo/migration/fitted/abies_alba_expandLDD_scprb_2km20km_fullmodel_from12000"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
abies_performance <- model_performance
abies_performance$species <- "abies"



#----------------------#
# 3. Quercus deciduous #
#----------------------#

pollen_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc"
add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"
evergreen <- FALSE
years <- c(seq(500,11500, 500))
brk <- 2000
models <- data.frame(name = c("5PHENOFIT",
                              "7PHENOFIT(fitted)",
                              "2RandomForest",
                              "1BRT",
                              "3GAM",
                              "4LassoGLM",
                              "6CASTANEA",
                              "8CASTANEA(fitted)"
                              ),
                     type = c("4Expertprocessbased",
                              "3Fittedprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "4Expertprocessbased",
                              "3Fittedprocessbased"
                              ),
                     type2 = c("4Expertprocessbased",
                               "3Fittedprocessbased",
                               "1Treebased",
                               "1Treebased",
                               "2Regressionbased",
                               "2Regressionbased",
                               "4Expertprocessbased",
                               "3Fittedprocessbased"
                               ),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/phenofit/paleo/migration/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/random_forest/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from11750",
                                   "D:/simulations/csdm/brt/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/gam/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/castanea/paleo/migration/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                                   "D:/simulations/castanea/paleo/migration/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"
                                   ))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
quercus_deciduous_performance <- model_performance
quercus_deciduous_performance$species <- "quercusdeciduous"



#----------------------#
# 4. Quercus evergreen #
#----------------------#

pollen_folder <- "D:/species/pollen/processed/quercus_evergreentype/025deg/0025thr_500yrunc"
add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"
evergreen <- TRUE
years <- c(seq(500,11500, 500))
brk <- 2000
models <- data.frame(name = c("5PHENOFIT",
                              "7PHENOFIT(fitted)",
                              "2RandomForest",
                              "1BRT",
                              "3GAM",
                              "4LassoGLM",
                              "6CASTANEA",
                              "8CASTANEA(fitted)"),
type = c("4Expertprocessbased",
         "3Fittedprocessbased",
         "1Correlative",
         "1Correlative",
         "1Correlative",
         "1Correlative",
         "4Expertprocessbased",
         "3Fittedprocessbased"
),
type2 = c("4Expertprocessbased",
          "3Fittedprocessbased",
          "1Treebased",
          "1Treebased",
          "2Regressionbased",
          "2Regressionbased",
          "4Expertprocessbased",
          "3Fittedprocessbased"
),
simfolder = c("D:/simulations/phenofit/paleo/migration/expert/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000",
              "D:/simulations/phenofit/paleo/migration/fitted/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000",
              "D:/simulations/csdm/random_forest/paleo/migration/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000",
              "D:/simulations/csdm/brt/paleo/migration/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000",
              "D:/simulations/csdm/gam/paleo/migration/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000",
              "D:/simulations/csdm/lasso_glm/paleo/migration/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from11750",
              "D:/simulations/castanea/paleo/migration/expert/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000",
              "D:/simulations/castanea/paleo/migration/fitted/quercus_evergreen_expandLDD_scprb_2km20km_fullmodel_from12000"
))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
quercus_evergreen_performance <- model_performance
quercus_evergreen_performance$species <- "quercusevergreen"


#---------#
# Gather! #
#---------#

model_performance_withmig <- rbind(fagus_performance, abies_performance, quercus_deciduous_performance, quercus_evergreen_performance)
model_performance_withmig <- left_join(model_performance_withmig, burke_climatenovelty, by = c("year"))

model_performance_withmig <- left_join(model_performance_withmig, past_climdiss, by = c("year" = "clim_hpv_sorensen.year" ))

# model_performance_withmig <- model_performance_withmig  %>% dplyr::filter(type != "4Expertprocessbasedincomplete")
