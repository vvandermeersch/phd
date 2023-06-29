#------------------------------------------#
# Small script to calculate best threshold #
#------------------------------------------#

library(AUC)
library(dplyr)
source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fagus_sylvatica/fagus_sylvatica_presabs_woUkraine.rds"))
sp_name <- "fagus_sylvatica"



out_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/fitted/fagus_sylvatica"
sim_folder <- "D:/simulations/castanea/backward/fagus_sylvatica/paper_data/CMAES/subset1_rep2"
filename <- "cmaes_fit_subset1_rep2"
modality <- "inverse calibration"

# Compute AUC on every pres/abs points
biomass <- read_mean_outputvalue(output_folder = sim_folder,
                                 model = "CASTANEA_present", output_var = "BiomassOfReserves",
                                 num_years = 30)
biomass$lat <- round(biomass$lat, 1)
biomass$lon<- round(biomass$lon, 1)
names(biomass)[3] <- "pred"
biomass_presabs <- inner_join(sp_presabs, biomass, by = c("lat", "lon")) %>% dplyr::select(-c('pres'))
auc_tot <- round(auc(roc(biomass_presabs$pred, as.factor(sp_presabs$pres))),6)
print(auc_tot)

# Best threshold
youden_index <- sensitivity(biomass_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure +
  specificity(biomass_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure - 1
thresholds <- sensitivity(biomass_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]
print(best_threshold)

# save (as in CSDM)
outfile <- list()
outfile$species <- sp_name
outfile$name <- "CASTANEA" # model name
outfile$modality <- modality # modelling modality 
outfile$species_file <- filename
outfile$auc_all <- auc_tot # auc on every species points
outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
outfile$europe_pred <- biomass # prediction on every Europe cells
saveRDS(outfile, file = file.path(out_folder, paste0(filename, ".rds")))
