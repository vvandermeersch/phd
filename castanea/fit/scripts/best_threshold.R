#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)
library(gtools)
library(vroom)
library(terra)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "quercus_petraea/quercus_petraea_presabs_woUkraine.rds"))
sp_name <- "quercus_petraea"



out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/expert/quercus_petraea"
sim_folder <- "D:/simulations/castanea/backward/quercus_petraea_240ppm"
filename <- "quercus_petraea_240ppm"
modality <- "inverse calibration"

# Compute AUC on every pres/abs points (NPP or Reserves)
biomass <- read_mean_outputvalue(output_folder = sim_folder,
                                 model = "CASTANEA_present", output_var = "BiomassOfReserves",
                                 num_years = 30)
biomass$lat <- round(biomass$lat, 1)
biomass$lon<- round(biomass$lon, 1)
names(biomass)[3] <- "pred"
biomass_presabs <- inner_join(sp_presabs, biomass, by = c("lat", "lon"))
biomass_presabs <- biomass_presabs[biomass_presabs$lat < 66.6, ]
auc_tot <- round(auc(roc(biomass_presabs$pred, as.factor(sp_presabs$pres))),6)
print(auc_tot)


# Best threshold
youden_index <- sensitivity(biomass_presabs$pred, as.factor(biomass_presabs$pres), perc.rank = F)$measure +
  specificity(biomass_presabs$pred, as.factor(biomass_presabs$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(biomass_presabs$pred, as.factor(biomass_presabs$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]
print(best_threshold)

# Confusion matrix and TSS
biomass_presabs$pred_pres <- ifelse(biomass_presabs$pred < best_threshold,0 , 1)
tp <- nrow(biomass_presabs[biomass_presabs$pred_pres == 1 & biomass_presabs$pres == 1,])
fp <- nrow(biomass_presabs[biomass_presabs$pred_pres == 1 & biomass_presabs$pres == 0,])
tn <- nrow(biomass_presabs[biomass_presabs$pred_pres == 0 & biomass_presabs$pres == 0,])
fn <- nrow(biomass_presabs[biomass_presabs$pred_pres == 0 & biomass_presabs$pres == 1,])
mig_sens = tp/(tp+fn)
mig_spec = tn/(tn+fp)
tss = mig_sens + mig_spec - 1
print(tss)

# save (as in CSDM)
outfile <- list()
outfile$species <- sp_name
outfile$name <- "CASTANEA" # model name
outfile$modality <- modality # modelling modality 
outfile$species_file <- filename
outfile$auc_all <- auc_tot # auc on every species points
outfile$tss_all <- tss # tss on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- biomass # prediction on every Europe cells
saveRDS(outfile, file = file.path(out_folder, paste0(filename, ".rds")))
