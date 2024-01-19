#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "pinus_sylvestris/pinus_sylvestris_presabs.rds"))
sp_name <- "picea_abies"

out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/pinus_sylvestris"
sim_folder <- "D:/simulations/phenofit/present/fitted/pinus_sylvestris/subset1_rep1"
filename <- "cmaes_fit_subset2_rep5"
# filename <- "Chuine2"
modality <- "inverse calibration"

# Compute AUC on every pres/abs points
fitness <- read_mean_outputvalue(sim_folder, output_var = "Fitness")
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
sorensen = 2*tp/(fn + 2*tp + fp)
print(sorensen)

# save (as in CSDM)
outfile <- list()
outfile$species <- sp_name
outfile$name <- "PHENOFIT" # model name
outfile$modality <- modality # modelling modality 
outfile$species_file <- filename
outfile$auc_all <- auc_tot # auc on every species points
outfile$tss_all <- tss # tss on every species points
outfile$sorensen_all <- sorensen # sorensen on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- fitness # prediction on every Europe cells
saveRDS(outfile, file = file.path(out_folder, paste0(filename, ".rds")))
