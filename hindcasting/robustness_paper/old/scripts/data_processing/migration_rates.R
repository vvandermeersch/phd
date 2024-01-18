#-------------------------#
# Process migration rates #
#-------------------------#



#----------#
# 1. Fagus #
#----------#

models <- data.frame(name = c("PHENOFIT\n(fitted)",
                              "PHENOFIT",
                              "RF",
                              "BRT",
                              "GAM"),
                     type = c("PHENOFIT",
                              "PHENOFIT",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     type2 = c("PHENOFIT",
                              "PHENOFIT\n(fitted)",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD_from12k_lsmask",
                                   "D:/simulations/phenofit/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_lsmask",
                                   "D:/simulations/csdm/random_forest/paleo/migration/fagus_sylvatica_expandLDD_from12k_lsmask",
                                   "D:/simulations/csdm/brt/paleo/migration/fagus_sylvatica_expandLDD_from12k_lsmask",
                                   "D:/simulations/csdm/gam/paleo/migration/fagus_sylvatica_expandLDD_from12k_lsmask"))

fagus_migration_rates <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  migration_rate <- readRDS(file.path(mod$simfolder, "migration_rate.rds"))
  migration_rate$model <- mod$name
  migration_rate$type <- mod$type
  migration_rate$type2 <- mod$type2
  return(migration_rate)
  
})
fagus_migration_rates <- do.call(rbind.data.frame, fagus_migration_rates)
fagus_migration_rates$species <- "Fagus"



#----------#
# 2. Abies #
#----------#

models <- data.frame(name = c("PHENOFIT\n(fitted)",
                              "PHENOFIT",
                              "RF",
                              "GAM",
                              "BRT"),
                     type = c("PHENOFIT",
                              "PHENOFIT",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     type2 = c("PHENOFIT",
                               "PHENOFIT\n(fitted)",
                               "cSDM",
                               "cSDM",
                               "cSDM"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/abies_alba_expandLDD",
                                   "D:/simulations/phenofit/paleo/migration/expert/abies_alba_expandLDD",
                                   "D:/simulations/csdm/random_forest/paleo/migration/abies_alba_expandLDD",
                                   "D:/simulations/csdm/gam/paleo/migration/abies_alba_expandLDD",
                                   "D:/simulations/csdm/brt/paleo/migration/abies_alba_expandLDD"))

abies_migration_rates <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  migration_rate <- readRDS(file.path(mod$simfolder, "migration_rate.rds"))
  migration_rate$model <- mod$name
  migration_rate$type <- mod$type
  migration_rate$type2 <- mod$type2
  return(migration_rate)
  
})
abies_migration_rates <- do.call(rbind.data.frame, abies_migration_rates)
abies_migration_rates$species <- "Abies"
