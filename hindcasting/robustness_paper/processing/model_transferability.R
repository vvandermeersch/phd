
#-----------------------------------------------------------------------#
# Compute model transferability metric (relative change of performance) #
#-----------------------------------------------------------------------#

# Prepare dataframe
model_performance_withmig$test <- "1Observedreference"
csdm_copy <- model_performance_withmig[model_performance_withmig$type == "1Correlative", ]
csdm_copy$test <- "2Crossvalidationreference"
model_performance_relchg <- rbind(model_performance_withmig, csdm_copy)
model_performance_relchg$rel_chg_sorensen <- NA

###-----------------------------###
## PHENOFIT (expert calibration) ##
###-----------------------------###
mod <- "PHENOFIT"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "Fagus_sylvatica_VVanderMeersch.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "Abies_alba_VVanderMeersch2.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "Quercus_robur_ADuputie_Chuine.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "Quercus_petraea_VanderMeersch2023_Chuine.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "Quercus_ilex_FTauc_ptype3.rds"))$sorensen_all)
  
)
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus petraea
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]


###------------------------------###
## PHENOFIT (inverse calibration) ##
###------------------------------###
mod <- "PHENOFIT (fitted)"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "cmaes_fit_subset4_rep1.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "cmaes_fit_subset2_rep5.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "cmaes_fit_subset1_rep1.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "cmaes_fit_subset1_rep2.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "cmaes_fit_subset1_rep1.rds"))$sorensen_all)
  
)
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]


###-----------------------------###
## CASTANEA (expert calibration) ##
###-----------------------------###
mod <- "CASTANEA"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/expert"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "fagus_sylvatica_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "abies_alba_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "quercus_robur_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "quercus_petraea_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "quercus_ilex_240ppm_frostVV.rds"))$sorensen_all)
  
)
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]


###------------------------------###
## CASTANEA (inverse calibration) ##
###------------------------------###
mod <- "CASTANEA (fitted)"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/fitted"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "fagus_sylvatica_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "abies_alba_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "quercus_robur_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "quercus_petraea_240ppm.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "quercus_ilex_240ppm.rds"))$sorensen_all)
  
)
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]


###---------###
## Lasso GLM ##
###---------###
mod <- "GLM"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen_test = c(readRDS(file.path(dir, "fagus_sylvatica", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "abies_alba", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_robur", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_petraea", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_ilex", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_test),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "lasso_glm_finalcov_fullmodel.rds"))$sorensen_all)
  
)
### vs. observed sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]
### vs. cross-validation sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen_test"])/historical[historical$species == "Fagus sylvatica", "sorensen_test"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen_test"])/historical[historical$species == "Abies alba", "sorensen_test"]
### quercus deciduous
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
              historical[historical$species == "Quercus petraea", "sorensen_test"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
           historical[historical$species == "Quercus petraea", "sorensen_test"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen_test"])/historical[historical$species == "Quercus ilex", "sorensen_test"]


###---###
## GAM ##
###---###
mod <- "GAM"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen_test = c(readRDS(file.path(dir, "fagus_sylvatica", "gam_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "abies_alba", "gam_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_robur", "gam_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_petraea", "gam_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_ilex", "gam_finalcov_fullmodel.rds"))$sorensen_test),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "gam_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "gam_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "gam_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "gam_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "gam_finalcov_fullmodel.rds"))$sorensen_all)
  
)
### vs. observed sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]
### vs. cross-validation sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen_test"])/historical[historical$species == "Fagus sylvatica", "sorensen_test"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen_test"])/historical[historical$species == "Abies alba", "sorensen_test"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
              historical[historical$species == "Quercus petraea", "sorensen_test"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
           historical[historical$species == "Quercus petraea", "sorensen_test"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen_test"])/historical[historical$species == "Quercus ilex", "sorensen_test"]


###-------------###
## Random Forest ##
###-------------###
mod <- "Random Forest"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen_test = c(readRDS(file.path(dir, "fagus_sylvatica", "random_forest_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "abies_alba", "random_forest_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_robur", "random_forest_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_petraea", "random_forest_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_ilex", "random_forest_finalcov_fullmodel.rds"))$sorensen_test),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "random_forest_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "random_forest_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "random_forest_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "random_forest_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "random_forest_finalcov_fullmodel.rds"))$sorensen_all)
  
)
### vs. observed sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]
### vs. cross-validation sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen_test"])/historical[historical$species == "Fagus sylvatica", "sorensen_test"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen_test"])/historical[historical$species == "Abies alba", "sorensen_test"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
              historical[historical$species == "Quercus petraea", "sorensen_test"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
           historical[historical$species == "Quercus petraea", "sorensen_test"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen_test"])/historical[historical$species == "Quercus ilex", "sorensen_test"]


###---###
## BRT ##
###---###
mod <- "BRT"
dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit"
historical <- data.frame(
  species = c("Fagus sylvatica", 
              "Abies alba", 
              "Quercus robur", 
              "Quercus petraea", 
              "Quercus ilex"),
  sorensen_test = c(readRDS(file.path(dir, "fagus_sylvatica", "brt_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "abies_alba", "brt_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_robur", "brt_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_petraea", "brt_finalcov_fullmodel.rds"))$sorensen_test,
               readRDS(file.path(dir, "quercus_ilex", "brt_finalcov_fullmodel.rds"))$sorensen_test),
  sorensen = c(readRDS(file.path(dir, "fagus_sylvatica", "brt_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "abies_alba", "brt_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_robur", "brt_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_petraea", "brt_finalcov_fullmodel.rds"))$sorensen_all,
          readRDS(file.path(dir, "quercus_ilex", "brt_finalcov_fullmodel.rds"))$sorensen_all)
  
)
### vs. observed sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen"])/historical[historical$species == "Fagus sylvatica", "sorensen"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen"])/historical[historical$species == "Abies alba", "sorensen"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen"], 
              historical[historical$species == "Quercus petraea", "sorensen"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen"], 
           historical[historical$species == "Quercus petraea", "sorensen"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "1Observedreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "1Observedreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen"])/historical[historical$species == "Quercus ilex", "sorensen"]
### vs. cross-validation sorensen
### fagus
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "fagus" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Fagus sylvatica", "sorensen_test"])/historical[historical$species == "Fagus sylvatica", "sorensen_test"]
### abies
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "abies" & 
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Abies alba", "sorensen_test"])/historical[historical$species == "Abies alba", "sorensen_test"]
### quercus deciduous (historical performance = median performance of the 2 species)
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusdeciduous" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
              historical[historical$species == "Quercus petraea", "sorensen_test"])))/
  median(c(historical[historical$species == "Quercus robur", "sorensen_test"], 
           historical[historical$species == "Quercus petraea", "sorensen_test"]))
### quercus evergreen
model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                           model_performance_relchg$test == "2Crossvalidationreference", "rel_chg_sorensen"] <-
  (model_performance_relchg[model_performance_relchg$mod == mod & model_performance_relchg$species == "quercusevergreen" &
                              model_performance_relchg$test == "2Crossvalidationreference", "mig_sorensen"]-
     historical[historical$species == "Quercus ilex", "sorensen_test"])/historical[historical$species == "Quercus ilex", "sorensen_test"]

