
# Quercus genus historical performance

# Combine presence/absence of the three Quercus species
sp_folder <- "D:/species/processed"
qrob_presabs <- readRDS(file.path(sp_folder, "quercus_robur/quercus_robur_presabs_woUkraine.rds"))
names(qrob_presabs)[ which(names(qrob_presabs)=="pres")] <- "pres_qrob"
qpet_presabs <- readRDS(file.path(sp_folder, "quercus_petraea/quercus_petraea_presabs_woUkraine.rds"))
names(qpet_presabs)[ which(names(qpet_presabs)=="pres")] <- "pres_qpet"
qdec_presabs <- full_join(qrob_presabs, qpet_presabs, by = c("lat", "lon"))
qile_presabs <- readRDS(file.path(sp_folder, "quercus_ilex/quercus_ilex_presabs_woUkraine.rds"))
names(qile_presabs)[ which(names(qile_presabs)=="pres")] <- "pres_qile"
quercus_presabs <- full_join(qdec_presabs, qile_presabs, by = c("lat", "lon"))
quercus_presabs$consensus <- rowSums(quercus_presabs[,c("pres_qrob", "pres_qpet", "pres_qile")], na.rm = TRUE)
quercus_presabs$pres <- ifelse(quercus_presabs$consensus>0,1,0)

# PHENOFIT
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "Quercus_robur_ADuputie_Chuine.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "Quercus_petraea_VanderMeersch2023_Chuine.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "Quercus_ilex_FTauc_ptype3.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "PHENOFIT" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

# PHENOFIT (inverse calibration)
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "cmaes_fit_subset1_rep1.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "cmaes_fit_subset1_rep2.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "cmaes_fit_subset1_rep1.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "PHENOFIT" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

# Lasso GLM
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "lasso_glm_finalcov_fullmodel.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "lasso_glm_finalcov_fullmodel.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "lasso_glm_finalcov_fullmodel.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "Lasso GLM" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

# GAM
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "gam_finalcov_fullmodel.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "gam_finalcov_fullmodel.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "gam_finalcov_fullmodel.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "GAM" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

# Random Forest
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "random_forest_finalcov_fullmodel.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "random_forest_finalcov_fullmodel.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "random_forest_finalcov_fullmodel.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "Random Forest" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

# BRT
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "brt_finalcov_fullmodel.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "brt_finalcov_fullmodel.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "brt_finalcov_fullmodel.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "BRT" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

# CASTANEA
mod_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/CASTANEA/fit/expert"
qrob_mod <- readRDS(file.path(mod_folder, "quercus_robur", "quercus_robur_280ppm.rds"))
qrob_fitness <- qrob_mod$europe_pred
qrob_fitness$pred <- ifelse(qrob_fitness$pred < qrob_mod$best_threshold, 0 ,1)
names(qrob_fitness)[ which(names(qrob_fitness)=="pred")] <- "pred_qrob"
qpet_mod <- readRDS(file.path(mod_folder, "quercus_petraea", "quercus_petraea_280ppm.rds"))
qpet_fitness <- qpet_mod$europe_pred
qpet_fitness$pred <- ifelse(qpet_fitness$pred < qpet_mod$best_threshold, 0 ,1)
names(qpet_fitness)[which(names(qpet_fitness)=="pred")] <- "pred_qpet"
qdec_fitness <- full_join(qrob_fitness, qpet_fitness, by = c("lat", "lon"))
qile_mod <- readRDS(file.path(mod_folder, "quercus_ilex", "quercus_ilex_280ppm.rds"))
qile_fitness <- qile_mod$europe_pred
qile_fitness$pred <- ifelse(qile_fitness$pred < qile_mod$best_threshold, 0 ,1)
names(qile_fitness)[which(names(qile_fitness)=="pred")] <- "pred_qile"
quercus_fitness <- full_join(qdec_fitness, qile_fitness, by = c("lat", "lon"))
quercus_fitness$consensus <- rowSums(quercus_fitness[,c("pred_qrob", "pred_qpet", "pred_qile")], na.rm = TRUE)
quercus_fitness$pred <- ifelse(quercus_fitness$consensus>0,1,0)
quercus_fitness <- inner_join(quercus_presabs, quercus_fitness, by = c("lat", "lon"))
# confusion matrix and TSS
tp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 1,])
fp <- nrow(quercus_fitness[quercus_fitness$pred == 1 & quercus_fitness$pres == 0,])
tn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 0,])
fn <- nrow(quercus_fitness[quercus_fitness$pred == 0 & quercus_fitness$pres == 1,])
sens = tp/(tp+fn)
spec = tn/(tn+fp)
tss = sens + spec - 1
outfile <- list()
outfile$species <- "Quercus genus"
outfile$name <- "CASTANEA" # model name
outfile$tss_all <- tss # tss on every species points
saveRDS(outfile, file = file.path(mod_folder, paste0("quercus_genus", ".rds")))

