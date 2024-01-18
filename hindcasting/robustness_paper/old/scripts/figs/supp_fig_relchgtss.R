model_performance_withmig$rel_chg_tss <- NA
model_performance_withmig$test <- "1Observedreference"

csdm_copy <- model_performance_withmig[model_performance_withmig$type == "1Correlative", ]
csdm_copy$test <- "2Crossvalidationreference"
model_performance_relchg <- rbind(model_performance_withmig, csdm_copy)

# Load historical TSS, and compute relative change

## PHENOFIT (expert calibration)
mod <- "5PHENOFIT"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  tss = c(readRDS(file.path(dir, "fagus_sylvatica", "Fagus_sylvatica_VVanderMeersch.rds"))$tss_all,
              readRDS(file.path(dir, "abies_alba", "Abies_alba_VVanderMeersch2.rds"))$tss_all,
              readRDS(file.path(dir, "quercus_robur", "Quercus_robur_ADuputie_Chuine.rds"))$tss_all,
              readRDS(file.path(dir, "quercus_petraea", "Quercus_petraea_VanderMeersch2023_Chuine.rds"))$tss_all,
              readRDS(file.path(dir, "quercus_ilex", "Quercus_ilex_FTauc_ptype3.rds"))$tss_all)
                       
)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss"])/historical[historical$species == "Fagus sylvatica", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss"])/historical[historical$species == "Abies alba", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss"], 
              historical[historical$species == "Quercus petraea", "tss"], 
              historical[historical$species == "Quercus ilex", "tss"])))/
  median(c(historical[historical$species == "Quercus robur", "tss"], 
           historical[historical$species == "Quercus petraea", "tss"], 
           historical[historical$species == "Quercus ilex", "tss"]))

## PHENOFIT (inverse calibration)
mod <- "7PHENOFIT(fitted)"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  tss = c(readRDS(file.path(dir, "fagus_sylvatica", "cmaes_fit_subset4_rep1.rds"))$tss_all,
          readRDS(file.path(dir, "abies_alba", "cmaes_fit_subset2_rep5.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_robur/old", "cmaes_fit_subset1_rep1.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_petraea/old", "cmaes_fit_subset1_rep1.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_ilex", "cmaes_fit_subset1_rep1.rds"))$tss_all)
  
)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss"])/historical[historical$species == "Fagus sylvatica", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss"])/historical[historical$species == "Abies alba", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss"], 
              historical[historical$species == "Quercus petraea", "tss"], 
              historical[historical$species == "Quercus ilex", "tss"])))/
  median(c(historical[historical$species == "Quercus robur", "tss"], 
           historical[historical$species == "Quercus petraea", "tss"], 
           historical[historical$species == "Quercus ilex", "tss"]))

## Lasso GLM
mod <- "4LassoGLM"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  tss_test = c(readRDS(file.path(dir, "fagus_sylvatica", "lasso_glm_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "abies_alba", "lasso_glm_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_robur", "lasso_glm_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_petraea", "lasso_glm_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_ilex", "lasso_glm_finalcov_fullmodel.rds"))$tss_test),
  tss = c(readRDS(file.path(dir, "fagus_sylvatica", "lasso_glm_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "abies_alba", "lasso_glm_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_robur", "lasso_glm_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_petraea", "lasso_glm_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_ilex", "lasso_glm_finalcov_fullmodel.rds"))$tss_all)
  
)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss"])/historical[historical$species == "Fagus sylvatica", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss"])/historical[historical$species == "Abies alba", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss"], 
              historical[historical$species == "Quercus petraea", "tss"], 
              historical[historical$species == "Quercus ilex", "tss"])))/
  median(c(historical[historical$species == "Quercus robur", "tss"], 
           historical[historical$species == "Quercus petraea", "tss"], 
           historical[historical$species == "Quercus ilex", "tss"]))
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss_test"])/historical[historical$species == "Fagus sylvatica", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss_test"])/historical[historical$species == "Abies alba", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss_test"], 
              historical[historical$species == "Quercus petraea", "tss_test"], 
              historical[historical$species == "Quercus ilex", "tss_test"])))/
  median(c(historical[historical$species == "Quercus robur", "tss_test"], 
           historical[historical$species == "Quercus petraea", "tss_test"], 
           historical[historical$species == "Quercus ilex", "tss_test"]))

## GAM
mod <- "3GAM"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  tss_test = c(readRDS(file.path(dir, "fagus_sylvatica", "gam_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "abies_alba", "gam_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_robur", "gam_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_petraea", "gam_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_ilex", "gam_finalcov_fullmodel.rds"))$tss_test),
  tss = c(readRDS(file.path(dir, "fagus_sylvatica", "gam_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "abies_alba", "gam_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_robur", "gam_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_petraea", "gam_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_ilex", "gam_finalcov_fullmodel.rds"))$tss_all)
  
)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss"])/historical[historical$species == "Fagus sylvatica", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss"])/historical[historical$species == "Abies alba", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss"], 
              historical[historical$species == "Quercus petraea", "tss"], 
              historical[historical$species == "Quercus ilex", "tss"])))/
  median(c(historical[historical$species == "Quercus robur", "tss"], 
           historical[historical$species == "Quercus petraea", "tss"], 
           historical[historical$species == "Quercus ilex", "tss"]))
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss_test"])/historical[historical$species == "Fagus sylvatica", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss_test"])/historical[historical$species == "Abies alba", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss_test"], 
              historical[historical$species == "Quercus petraea", "tss_test"], 
              historical[historical$species == "Quercus ilex", "tss_test"])))/
  median(c(historical[historical$species == "Quercus robur", "tss_test"], 
           historical[historical$species == "Quercus petraea", "tss_test"], 
           historical[historical$species == "Quercus ilex", "tss_test"]))

## Random Forest
mod <- "2RandomForest"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  tss_test = c(readRDS(file.path(dir, "fagus_sylvatica", "random_forest_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "abies_alba", "random_forest_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_robur", "random_forest_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_petraea", "random_forest_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_ilex", "random_forest_finalcov_fullmodel.rds"))$tss_test),
  tss = c(readRDS(file.path(dir, "fagus_sylvatica", "random_forest_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "abies_alba", "random_forest_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_robur", "random_forest_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_petraea", "random_forest_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_ilex", "random_forest_finalcov_fullmodel.rds"))$tss_all)
  
)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss"])/historical[historical$species == "Fagus sylvatica", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss"])/historical[historical$species == "Abies alba", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss"], 
              historical[historical$species == "Quercus petraea", "tss"], 
              historical[historical$species == "Quercus ilex", "tss"])))/
  median(c(historical[historical$species == "Quercus robur", "tss"], 
           historical[historical$species == "Quercus petraea", "tss"], 
           historical[historical$species == "Quercus ilex", "tss"]))
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss_test"])/historical[historical$species == "Fagus sylvatica", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss_test"])/historical[historical$species == "Abies alba", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss_test"], 
              historical[historical$species == "Quercus petraea", "tss_test"], 
              historical[historical$species == "Quercus ilex", "tss_test"])))/
  median(c(historical[historical$species == "Quercus robur", "tss_test"], 
           historical[historical$species == "Quercus petraea", "tss_test"], 
           historical[historical$species == "Quercus ilex", "tss_test"]))

## BRT
mod <- "1BRT"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  tss_test = c(readRDS(file.path(dir, "fagus_sylvatica", "brt_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "abies_alba", "brt_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_robur", "brt_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_petraea", "brt_finalcov_fullmodel.rds"))$tss_test,
               readRDS(file.path(dir, "quercus_ilex", "brt_finalcov_fullmodel.rds"))$tss_test),
  tss = c(readRDS(file.path(dir, "fagus_sylvatica", "brt_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "abies_alba", "brt_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_robur", "brt_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_petraea", "brt_finalcov_fullmodel.rds"))$tss_all,
          readRDS(file.path(dir, "quercus_ilex", "brt_finalcov_fullmodel.rds"))$tss_all)
  
)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss"])/historical[historical$species == "Fagus sylvatica", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss"])/historical[historical$species == "Abies alba", "tss"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "1Observedreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss"], 
              historical[historical$species == "Quercus petraea", "tss"], 
              historical[historical$species == "Quercus ilex", "tss"])))/
  median(c(historical[historical$species == "Quercus robur", "tss"], 
           historical[historical$species == "Quercus petraea", "tss"], 
           historical[historical$species == "Quercus ilex", "tss"]))
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" & 
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Fagus sylvatica", "tss_test"])/historical[historical$species == "Fagus sylvatica", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     historical[historical$species == "Abies alba", "tss_test"])/historical[historical$species == "Abies alba", "tss_test"]
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_tss"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_tss"]-
     median(c(historical[historical$species == "Quercus robur", "tss_test"], 
              historical[historical$species == "Quercus petraea", "tss_test"], 
              historical[historical$species == "Quercus ilex", "tss_test"])))/
  median(c(historical[historical$species == "Quercus robur", "tss_test"], 
           historical[historical$species == "Quercus petraea", "tss_test"], 
           historical[historical$species == "Quercus ilex", "tss_test"]))

model_performance_relchg$type2test <- paste0(model_performance_relchg$type2, model_performance_relchg$test)
dunnTest_out <- FSA::dunnTest(rel_chg_tss ~ type2test, method = "bh", data = model_performance_relchg)
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
library(tidyr)
data_letters[, "type2"] <- separate(data.frame(A = data_letters$Group), col = "A" , into = c("X", "Y"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")[,1]
data_letters[, "test"] <- separate(data.frame(A = data_letters$Group), col = "A" , into = c("X", "Y"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")[,2]
data_letters[, "sign"] <- NA
data_letters[data_letters$test == "2Crossvalidationreference", "sign"] <- "CV"

supp_fig_transferability <- ggplot(data = model_performance_relchg) +
  geom_boxplot(aes(x=type2, y=rel_chg_tss*100, col = type2, fill = type2, group = type2test),  position = position_dodge2(preserve = "single"),
               alpha = 0.3, outlier.size = 0.1) +
  geom_text(data = data_letters, aes(x = type2, label = Letter, y = c(12, 51, 29, 61, 78, 28), group = Group), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.5, position=position_dodge(width=0.8)) +
  geom_text(data = data_letters, aes(x = type2, label = sign, y = -30, group = Group), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.3, position=position_dodge(width=0.75)) +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81"),
                     labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81"),
                    labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.position="bottom", legend.title=element_blank(), legend.key.size = unit(0.4, "cm"),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        legend.margin=margin(-5,0,0,0)) +
  labs(y = "TRANSFERABILITY (Relative change in TSS, %)") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), color = guide_legend(nrow=2,byrow=TRUE))
  
