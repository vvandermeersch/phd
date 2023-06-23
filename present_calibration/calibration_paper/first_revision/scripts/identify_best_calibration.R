# Identify best calibrations
library(AUC)
library(data.table)
library(dplyr)
library(gtools)
source(file.path('C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/calibration_paper/second_revision', "functions", "read_mean_outputvalue_phenofit.R"))

# Setup
sp_folder <- "D:/species/processed"
cal_folder <- 'D:/calibrations/phenofit/larix_decidua/1000pres_1000abs'
sim_folder <- 'D:/simulations/phenofit/present/fitted/larix_decidua'
presabs <- readRDS(file.path(sp_folder, "larix_decidua/larix_decidua_presabs.rds"))

# Way to get lat/lon
climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
alt_file <- paste0(climate_folder, "/ERA5LAND_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)
alt$points <- as.numeric(rownames(alt))


# AUC obtained during calibration
cmaes_files <- mixedsort(list.files(path = cal_folder, pattern = "\\.Rdata$", full.names = T, recursive = T))
auc_data_cal <- data.frame()
for(i in 1:length(cmaes_files)){
  auc_data_cal[i, 'subset'] <- paste0(strsplit(cmaes_files[i], "_")[[1]][6])
  auc_data_cal[i, 'rep'] <- strsplit(strsplit(cmaes_files[i], "_")[[1]][7], "[.]")[[1]][1]
  
  # Load AUC on calibration_points
  load(cmaes_files[i])
  auc_cal <- 1-round(cmaes_fit[[1]][["value"]], 6)
  
  auc_data_cal[i, 'cal'] <- auc_cal
  
}

# Total AUC
simulation_files <- mixedsort(list.files(path = sim_folder, pattern = "Fitness", full.names = T, recursive = T))
auc_data_tot <- data.frame()
for(i in 1:(length(simulation_files))){
  auc_data_tot[i, 'subset'] <- strsplit(strsplit(simulation_files[i], "/")[[1]][7], "_")[[1]][1]
  auc_data_tot[i, 'rep'] <- strsplit(strsplit(simulation_files[i], "/")[[1]][7], "_")[[1]][2]
  
  # Compute AUC on every pres/abs points
  fitness_presabs <- read_mean_outputvalue_phenofit(simulation_files[i], points = presabs)
  auc_tot <- round(auc(roc(fitness_presabs, as.factor(presabs$pres))),6)
  auc_data_tot[i, 'tot'] <- auc_tot
  
  # Best threshold
  youden_index <- sensitivity(fitness_presabs, as.factor(presabs$pres), perc.rank = F)$measure +
    specificity(fitness_presabs, as.factor(presabs$pres), perc.rank = F)$measure - 1
  thresholds <- sensitivity(fitness_presabs, as.factor(presabs$pres), perc.rank = F)$cutoffs
  best_threshold <- thresholds[which(youden_index == max(youden_index))]
  auc_data_tot[i, 'best_threshold'] <- best_threshold
  
}



  