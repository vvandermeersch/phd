#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fagus_sylvatica/fagus_sylvatica_presabs_woUkraine.rds"))
sp_name <- "fagus_sylvatica"

out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fpe/fagus_sylvatica"
sim_folder <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"
filename <- "cmaes_fpe"
# filename <- "Quercus_ilex_FTauc_ptype3"
modality <- "inverse calibration - fitted parameter ensemble"

# Compute AUC on every pres/abs points
fitness_fpe <- c()
init <- TRUE
for(s in c(1,2)){
  for(r in c(1:5)){
    print(r)
    sim_folder_sr <- paste0(sim_folder, "/", "subset",s,"_rep", r)
    fitness <- read_mean_outputvalue(sim_folder_sr, output_var = "Fitness")
    if(init){
      fitness_fpe <- fitness
    }else{fitness_fpe <- cbind(fitness_fpe, fitness[,3])}
    init <- FALSE
  }
}
fitness_fpe <- cbind(fitness_fpe[1:2], rowMeans(fitness_fpe[3:12]))
names(fitness_fpe)[3] <- "pred"
fitness_presabs <- inner_join(sp_presabs, fitness_fpe, by = c("lat", "lon"))
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
