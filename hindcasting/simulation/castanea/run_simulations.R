
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


# Settings
species=list(name = "Fagus sylvatica", structure_file = "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/species/CastaneaSpecies_08_2021.txt")
species_file <- "CastaneaSpecies_08_2021.txt"
# species_file <- "fagus_cmaes_output_subset1_rep2.txt"


inv_options <- readRDS(file.path(wd, "castanea", "inv", "fagus_sylvatica.rds"))

output_folder <- "D:/simulations/castanea/paleo/expert/025deg/fagus_sylvatica"
climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/castanea_format/025deg"

# CO2 PPM used as baseline conditions in Armstrong et al. (2019)
CO2_data <- data.frame(year = seq(0,21000,1000),
                       conc = c(280, 279, 277, 275, 273,268, 265, 261, 261, 265, 267, 264,
                                245, 238, 237, 224, 210, 194, 189, 188, 188, 186))


ncores <- 20

# Simulation loop
for(year in seq(500, 11500, 500)){
  
  inv_options$general$start_year <- -year-14
  inv_options$model$CO2_mode <- "CO2_FIXED"
  inv_options$model$Ca <-  CO2_data[CO2_data$year == year%/%501*1000, "conc"]
  # inv_options$model$Ca <- 390

  unlink(paste0("D:/applications/capsis4_castanea/var"), recursive=TRUE)
  source(file.path(wd, "castanea", "init.R"))
  
  with_progress(system.time(run_castanea(runlines, ncores)))
  
  dir.create(file.path(output_folder, paste0(year, "BP_15yrinv")), showWarnings = FALSE, recursive = TRUE)
  files_to_move <- list.files("D:/applications/capsis4_castanea/var", pattern = "yearlyResults.log", full.names = TRUE)
  move_files <- lapply(files_to_move, function(i) file.move(i, file.path(output_folder, paste0(year, "BP_30yrinv")), overwrite = T))
  
}

var = "BiomassOfReserves"
files_to_read <- mixedsort(list.files("D:/applications/capsis4_castanea/var", pattern = "yearlyResults.log", full.names = TRUE))
yearly_results <- vroom(files_to_read, col_select = c(var), show_col_types = FALSE, progress=FALSE)
yearly_results[is.na(yearly_results)] <- 0 # NA values mean tree is dead (thus biomass = 0) 

nyears <- nrow(yearly_results)/nrow(grid)
mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), each = nyears, len = nrow(yearly_results))), mean)[-1]

mean_results[mean_results < 0] <- 0 
grid_r[!is.na(grid_r)] <- mean_results
plot(grid_r)


