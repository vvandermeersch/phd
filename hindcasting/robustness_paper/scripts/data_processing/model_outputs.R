#-----------------------#
# Process model outputs #
#-----------------------#



#----------#
# 1. Fagus #
#----------#

pollen_folder <- "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc"
years <- c(seq(500,8500, 500))
brk <- 2000
combine_models <- FALSE
models <- data.frame(name = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "Random Forest",
                              "Lasso GLM",
                              "BRT",
                              "GAM",
                              "CASTANEA",
                              "CASTANEA (fitted)",
                              "CASTANEA - CO2 fixed",
                              "CASTANEA (fitted) - CO2 fixed"),
                     type = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "cSDM",
                              "cSDM",
                              "cSDM",
                              "cSDM",
                              "CASTANEA",
                              "CASTANEA (fitted)",
                              "CASTANEA - CO2 fixed",
                              "CASTANEA (fitted) - CO2 fixed"),
                     simfolder = c("D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica",
                                   "D:/simulations/phenofit/paleo/fitted/025deg/fagus_sylvatica",
                                   "D:/simulations/csdm/random_forest/paleo/025deg/fagus_sylvatica",
                                   "D:/simulations/csdm/lasso_glm/paleo/025deg/fagus_sylvatica",
                                   "D:/simulations/csdm/brt/paleo/025deg/fagus_sylvatica",
                                   "D:/simulations/csdm/gam/paleo/025deg/fagus_sylvatica",
                                   "D:/simulations/castanea/paleo/expert/025deg/fagus_sylvatica",
                                   "D:/simulations/castanea/paleo/fitted/025deg/fagus_sylvatica",
                                   "D:/simulations/castanea/paleo/expert/025deg/fagus_sylvatica/CO2_fixed",
                                   "D:/simulations/castanea/paleo/fitted/025deg/fagus_sylvatica/CO2_fixed"),
                     modfolder = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/expert/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/fitted/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/expert/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/fitted/fagus_sylvatica"),
                     mod = c("Fagus_sylvatica_VVanderMeersch.rds",
                             "cmaes_fit_subset4_rep1.rds",
                             "random_forest_finalcov_fullmodel.rds",
                             "lasso_glm_finalcov_fullmodel.rds",
                             "brt_finalcov_fullmodel.rds",
                             "gam_finalcov_fullmodel.rds",
                             "fagus_sylvatica.rds",
                             "cmaes_fit_subset1_rep2.rds",
                             "fagus_sylvatica.rds",
                             "cmaes_fit_subset1_rep2.rds"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance.R"))
fagus_performance <- model_performance
fagus_performance$species <- "fagus"



#----------#
# 2. Abies #
#----------#

pollen_folder <- "D:/species/pollen/processed/abies/025deg/001thr_500yrunc"
years <- c(seq(500, 11000, 500))
brk <- 2000
combine_models <- FALSE
models <- data.frame(name = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "Random Forest",
                              "Lasso GLM",
                              "BRT",
                              "GAM"),
                     type = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "cSDM",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     simfolder = c("D:/simulations/phenofit/paleo/expert/025deg/abies_alba",
                                   "D:/simulations/phenofit/paleo/fitted/025deg/abies_alba",
                                   "D:/simulations/csdm/random_forest/paleo/025deg/abies_alba",
                                   "D:/simulations/csdm/lasso_glm/paleo/025deg/abies_alba",
                                   "D:/simulations/csdm/brt/paleo/025deg/abies_alba",
                                   "D:/simulations/csdm/gam/paleo/025deg/abies_alba"),
                     modfolder = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/abies_alba",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/abies_alba",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/abies_alba",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/abies_alba",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/abies_alba",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/abies_alba"),
                     mod = c("Abies_alba_VVanderMeersch2.rds",
                             "cmaes_fit_subset2_rep5.rds",
                             "random_forest_finalcov_fullmodel.rds",
                             "lasso_glm_finalcov_fullmodel.rds",
                             "brt_finalcov_fullmodel.rds",
                             "gam_finalcov_fullmodel.rds"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance.R"))
abies_performance <- model_performance
abies_performance$species <- "abies"



#------------#
# 2. Quercus #
#------------#

pollen_folder <- "D:/species/pollen/processed/quercus/025deg/0025thr_500yrunc"
years <- c(seq(500,11500,500))
brk <- 2000
combine_models <- TRUE
models <- data.frame(name = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "Random Forest",
                              "Lasso GLM",
                              "BRT",
                              "GAM"),
                     type = c("PHENOFIT",
                              "PHENOFIT (fitted)",
                              "cSDM",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     simfolder1 = c("D:/simulations/phenofit/paleo/expert/025deg/quercus_robur",
                                    "D:/simulations/phenofit/paleo/fitted/025deg/quercus_robur",   
                                    "D:/simulations/csdm/random_forest/paleo/025deg/quercus_robur",
                                    "D:/simulations/csdm/lasso_glm/paleo/025deg/quercus_robur",
                                    "D:/simulations/csdm/brt/paleo/025deg/quercus_robur",
                                    "D:/simulations/csdm/gam/paleo/025deg/quercus_robur"),
                     modfolder1 = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/quercus_robur",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/quercus_robur",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/quercus_robur",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/quercus_robur",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/quercus_robur",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/quercus_robur"),
                     mod1 = c("Quercus_robur_ADuputie.rds",
                              "cmaes_fit_subset1_rep1.rds",
                              "random_forest_finalcov_fullmodel.rds",
                              "lasso_glm_finalcov_fullmodel.rds",
                              "brt_finalcov_fullmodel.rds",
                              "gam_finalcov_fullmodel.rds"),
                     simfolder2 = c("D:/simulations/phenofit/paleo/expert/025deg/quercus_petraea",
                                    "D:/simulations/phenofit/paleo/fitted/025deg/quercus_petraea", 
                                    "D:/simulations/csdm/random_forest/paleo/025deg/quercus_petraea",
                                    "D:/simulations/csdm/lasso_glm/paleo/025deg/quercus_petraea",
                                    "D:/simulations/csdm/brt/paleo/025deg/quercus_petraea",
                                    "D:/simulations/csdm/gam/paleo/025deg/quercus_petraea"),
                     modfolder2 = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/quercus_petraea",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/quercus_petraea",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/quercus_petraea",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/quercus_petraea",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/quercus_petraea",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/quercus_petraea"),
                     mod2 = c("Quercus_petraea_EvolLett2019.rds",
                              "cmaes_fit_subset1_rep1.rds",
                              "random_forest_finalcov_fullmodel.rds",
                              "lasso_glm_finalcov_fullmodel.rds",
                              "brt_finalcov_fullmodel.rds",
                              "gam_finalcov_fullmodel.rds"),
                     simfolder3 = c("D:/simulations/phenofit/paleo/expert/025deg/quercus_pubescens",
                                    "D:/simulations/phenofit/paleo/fitted/025deg/quercus_pubescens", 
                                    "D:/simulations/csdm/random_forest/paleo/025deg/quercus_pubescens",
                                    "D:/simulations/csdm/lasso_glm/paleo/025deg/quercus_pubescens",
                                    "D:/simulations/csdm/brt/paleo/025deg/quercus_pubescens",
                                    "D:/simulations/csdm/gam/paleo/025deg/quercus_pubescens"),
                     modfolder3 = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/quercus_pubescens",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/quercus_pubescens",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/quercus_pubescens",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/quercus_pubescens",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/quercus_pubescens",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/quercus_pubescens"),
                     mod3 = c("Quercus_pubescens_Bleys.rds",
                              "cmaes_fit_subset1_rep3.rds",
                              "random_forest_finalcov_fullmodel.rds",
                              "lasso_glm_finalcov_fullmodel.rds",
                              "brt_finalcov_fullmodel.rds",
                              "gam_finalcov_fullmodel.rds"),
                     simfolder4 = c("D:/simulations/phenofit/paleo/expert/025deg/quercus_ilex",
                                    "D:/simulations/phenofit/paleo/fitted/025deg/quercus_ilex",   
                                    "D:/simulations/csdm/random_forest/paleo/025deg/quercus_ilex",
                                    "D:/simulations/csdm/lasso_glm/paleo/025deg/quercus_ilex",
                                    "D:/simulations/csdm/brt/paleo/025deg/quercus_ilex",
                                    "D:/simulations/csdm/gam/paleo/025deg/quercus_ilex"),
                     modfolder4 = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/quercus_ilex",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/quercus_ilex",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/quercus_ilex",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/quercus_ilex",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/quercus_ilex",
                                    "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/quercus_ilex"),
                     mod4 = c("Quercus_ilex_FTauc_ptype3.rds",
                              "cmaes_fit_subset1_rep1.rds",
                              "random_forest_finalcov_fullmodel.rds",
                              "lasso_glm_finalcov_fullmodel.rds",
                              "brt_finalcov_fullmodel.rds",
                              "gam_finalcov_fullmodel.rds"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance.R"))
quercus_performance <- model_performance
quercus_performance$species <- "quercus"



# Assemble!
model_performance <- rbind(fagus_performance, abies_performance, quercus_performance)
model_performance <- left_join(model_performance, burke_climatenovelty, by = c("year"))
