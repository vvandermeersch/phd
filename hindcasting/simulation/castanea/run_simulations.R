
##################################
#                                #
# RUN CASTANEA PALEO SIMULATIONS #
#                                #
##################################

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation"

# Load functions
library(data.table)
library(terra)
library(filesstrings)
library(future.apply)
library(progressr)
library(gtools)
library(vroom)
source(file.path(wd, "functions", "create_inventoryfile.R"))
source(file.path(wd, "functions", "create_runfile.R"))
source(file.path(wd, "functions/read_mean_outputvalue.R"))


# Settings
species=list(name = "Abies alba", structure_file = "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/species/CastaneaSpecies_08_2021.txt")
# species_file <- "CastaneaSpecies_08_2021.txt"
 species_file <- "aalba_subset1_rep1.txt"


inv_options <- readRDS(file.path(wd, "castanea", "inv", "abies_alba_24yr.rds"))

output_folder <- "D:/simulations/castanea/paleo/fitted/025deg/abies_alba/24yr_inventory_modcode"
# output_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/castanea/clim_test/false/output"
climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/castanea_format/025deg"
# climate_folder <-  "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/castanea/clim_test/false"

# CO2 PPM used as baseline conditions in Armstrong et al. (2019)
CO2_data <- data.frame(year = seq(0,21000,1000),
                       conc = c(280, 279, 277, 275, 273,268, 265, 261, 261, 265, 267, 264,
                                245, 238, 237, 224, 210, 194, 189, 188, 188, 186))


ncores <- 10

# Simulation loop
for(year in seq(500, 12000, 500)){
  
  inv_options$general$start_year <- -year-14
  inv_options$model$CO2_mode <- "CO2_FIXED"
  inv_options$model$Ca <-  CO2_data[CO2_data$year == year%/%1000*1000, "conc"]
  # inv_options$model$Ca <- 390
  
  # inv_options[["model"]][["ETR_mode"]] <- "ETR_FAO"
  # inv_options[["model"]][["aero_mode"]] <- "AERO_FAO"

  unlink(paste0("D:/applications/capsis4_castanea/var"), recursive=TRUE)
  source(file.path(wd, "castanea", "init.R"))
  
  with_progress(system.time(run_castanea(runlines, ncores)))
  
  files_to_move <- list.files("D:/applications/capsis4_castanea/var", pattern = "8905_ind1_yearlyResults.log", full.names = TRUE)
  move_files <- lapply(files_to_move, function(i) file.move(i, file.path(output_folder, paste0(year, "BP")), overwrite = T))
  
  biomass <- read_mean_outputvalue(output_folder = file.path(output_folder,  paste0(year, "BP")),
                                   model = "CASTANEA", output_var = "BiomassOfReserves",
                                   year = year, num_years = 30)
  names(biomass)[3] <- "pred"
  saveRDS(biomass, file.path(output_folder, "Reserves", paste0(year, "BP.rds")))
  
  npp <- read_mean_outputvalue(output_folder = file.path(output_folder, paste0(year, "BP")),
                                   model = "CASTANEA", output_var = "NPP",
                                   year = year, num_years = 30)
  names(npp)[3] <- "pred"
  saveRDS(npp, file.path(output_folder, "NPP", paste0(year, "BP.rds")))
  
}


