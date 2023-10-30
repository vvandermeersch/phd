#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "abies_alba/abies_alba_presabs.rds"))
sp_name <- "abies_alba"

out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/abies_alba"
sim_folder <- "D:/simulations/phenofit/present/expert/abies_alba/VVanderMeersch2"
filename <- "VVanderMeersch2_incomplete"
modality <- "expert calibration - FruitIndex x MaturationIndex"

# Compute AUC on every pres/abs points
survival <- read_mean_outputvalue(sim_folder, output_var = "Survival")
fruitindex <- read_mean_outputvalue(sim_folder, output_var = "FruitIndex")
maturationindex <- read_mean_outputvalue(sim_folder, output_var = "MaturationIndex")

fitness <- cbind(survival[,c(1,2)], fruitindex[,3]*maturationindex[,3])
names(fitness)[3] <- "pred"

fitness_presabs <- inner_join(sp_presabs, fitness, by = c("lat", "lon"))
auc_tot <- round(auc(roc(fitness_presabs$pred, as.factor(sp_presabs$pres))),6)
print(auc_tot)

# Best threshold
youden_index <- sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure +
  specificity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]
print(best_threshold)

# Confusion matrix and TSS
fitness_presabs$pred_pres <- ifelse(fitness_presabs$pred < best_threshold,0 , 1)
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
outfile$name <- "PHENOFIT" # model name
outfile$modality <- modality # modelling modality 
outfile$species_file <- filename
outfile$auc_all <- auc_tot # auc on every species points
outfile$tss_all <- tss # tss on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- fitness # prediction on every Europe cells
saveRDS(outfile, file = file.path(out_folder, paste0(filename, ".rds")))
