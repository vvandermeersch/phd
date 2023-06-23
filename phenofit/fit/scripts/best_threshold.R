#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "quercus_pubescens/quercus_pubescens_presabs.rds"))
sp_name <- "quercus_pubescens"



out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/quercus_pubescens"
#sim_folder <- "D:/simulations/phenofit/present/fitted/quercus_pubescens/subset1_rep2"
# sim_folder <- "D:/simulations/phenofit/present/fitted/quercus_ilex/paper_data/CMAES/subset1_rep1"
sim_folder <- "D:/simulations/phenofit/present/expert/quercus_pubescens/BLeys"
# filename <- "cmaes_fit_subset2_rep1"
filename <- "Quercus_pubescens_BLeys"
modality <- "expert calibration"

# Compute AUC on every pres/abs points
fitness <- read_mean_outputvalue(sim_folder, output_var = "Fitness")
names(fitness)[3] <- "pred"
fitness_presabs <- inner_join(sp_presabs, fitness, by = c("lat", "lon")) %>% dplyr::select(-c('pres'))
auc_tot <- round(auc(roc(fitness_presabs$pred, as.factor(sp_presabs$pres))),6)
print(auc_tot)

# Best threshold
youden_index <- sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure +
  specificity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]
print(best_threshold)

# save (as in CSDM)
outfile <- list()
outfile$species <- sp_name
outfile$name <- "PHENOFIT" # model name
outfile$modality <- modality # modelling modality 
outfile$species_file <- filename
outfile$auc_all <- auc_tot # auc on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- fitness # prediction on every Europe cells
saveRDS(outfile, file = file.path(out_folder, paste0(filename, ".rds")))
