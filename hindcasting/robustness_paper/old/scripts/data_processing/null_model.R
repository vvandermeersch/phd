#--------------------------------------------#
# Process null model outputs, with migration #
#--------------------------------------------#

# extent <- c(-10,30,36,66) extent use during migration simulation (to limit simulation runtime)
# locations with lat > 66 will be considered as not colonized

#----------#
# 1. Fagus #
#----------#

pollen_folder <- "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc"
years <- c(seq(500,8500, 500))
brk <- 2000
models <- data.frame(name = c("Null model"),
                     type = c("Nullmodel"),
                     type2 = c("Nullmodel"),
                     simfolder = c("D:/simulations/null/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
fagus_performance <- model_performance
fagus_performance$species <- "fagus"

#----------#
# 2. Abies #
#----------#

pollen_folder <- "D:/species/pollen/processed/abies/025deg/001thr_500yrunc"
years <- c(seq(500,11000, 500))
brk <- 2000
models <- data.frame(name = c("Null model"),
                     type = c("Nullmodel"),
                     type2 = c("Nullmodel"),
                     simfolder = c("D:/simulations/null/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
abies_performance <- model_performance
abies_performance$species <- "abies"

model_null_performance <- rbind(fagus_performance, abies_performance)

#------------#
# 3. Quercus #
#------------#

pollen_folder <- "D:/species/pollen/processed/quercus/025deg/0025thr_500yrunc"
years <- c(seq(500,11000, 500))
brk <- 2000
models <- data.frame(name = c("Null model"),
                     type = c("Nullmodel"),
                     type2 = c("Nullmodel"),
                     simfolder = c("D:/simulations/null/paleo/migration/quercus_expandLDD_from12k_scprb_2km20km"))
source(file.path(wd, "scripts", "data_processing", "load_model_performance_withmig.R"))
quercus_performance <- model_performance
quercus_performance$species <- "quercus"