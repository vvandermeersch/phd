#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fagus_sylvatica/fagus_sylvatica_presabs_woUkraine.rds"))
sp_name <- "fagus_sylvatica"

out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/mme/fit/fagus_sylvatica"


list_models <- c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/fagus_sylvatica/Fagus_sylvatica_VVanderMeersch.rds",
                 "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/fagus_sylvatica/cmaes_fit_subset4_rep1.rds",
                 "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/fagus_sylvatica/random_forest_finalcov_fullmodel.rds",
                 "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/fagus_sylvatica/lasso_glm_finalcov_fullmodel.rds",
                 "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/fagus_sylvatica/brt_finalcov_fullmodel.rds",
                 "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/fagus_sylvatica/gam_finalcov_fullmodel.rds")

filename <- "mme"

modality <- "Multi-model ensemble"

# Agreement between models
fitness_mme <- c()
init <- TRUE
for(m in list_models){
  mfit <- readRDS(m)
  fitness <- mfit$europe_pred
  
  if(init){fitness_mme <- fitness}else{fitness_mme <- left_join(fitness_mme, fitness, by = c("lon", "lat"))}
  
  init <- FALSE
}
fitness_mme <- cbind(fitness_mme[1:2], rowMeans(fitness_mme[3:8]))
names(fitness_mme)[3] <- "pred"

fitness_presabs <- inner_join(sp_presabs, fitness_mme, by = c("lat", "lon"))
auc_tot <- round(auc(roc(fitness_presabs$pred, as.factor(sp_presabs$pres))),6)
print(auc_tot)

# Best threshold
youden_index <- sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure +
  specificity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]
print(best_threshold)

# Confusion matrix and TSS
fitness_presabs$pred_pres <- ifelse(fitness_presabs$pred < best_threshold, 0 , 1)
tp <- nrow(fitness_presabs[fitness_presabs$pred_pres == 1 & fitness_presabs$pres == 1,])
fp <- nrow(fitness_presabs[fitness_presabs$pred_pres == 1 & fitness_presabs$pres == 0,])
tn <- nrow(fitness_presabs[fitness_presabs$pred_pres == 0 & fitness_presabs$pres == 0,])
fn <- nrow(fitness_presabs[fitness_presabs$pred_pres == 0 & fitness_presabs$pres == 1,])
mig_sens = tp/(tp+fn)
mig_spec = tn/(tn+fp)
tss = mig_sens + mig_spec - 1
print(tss)

# save (as in CSDM)
outfile <- list()
outfile$species <- sp_name
outfile$name <- "MME" # model name
outfile$modality <- modality # modelling modality 
outfile$species_file <- filename
outfile$auc_all <- auc_tot # auc on every species points
outfile$tss_all <- tss # tss on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- fitness # prediction on every Europe cells
saveRDS(outfile, file = file.path(out_folder, paste0(filename, ".rds")))
