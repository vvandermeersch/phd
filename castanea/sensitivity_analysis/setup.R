################
# Setup script #
################

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/castanea"

source(file.path(wd, "functions", "run_model.R"))
source(file.path(wd, "functions", "create_speciesfile.R"))
source(file.path(wd, "functions", "create_commandfiles.R"))
source(file.path(wd, "functions", "create_runfile.R"))
source(file.path(wd, "functions", "modify_speciesfile.R"))
source(file.path(wd, "functions", "read_mean_outputvalue.R"))

library(future)
library(future.apply)
#library(progressr)
library(vroom)
library(data.table)
library(dplyr)
library(tibble)

# extract the grid points where GSA will run 
climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
alt_file <- paste0(climate_folder, "/ERA5LAND_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)

alt_subset <- alt %>%
  rownames_to_column('cell') %>%
  subset(lat<46 & lat>42 & lon<8 & lon>4)
points_to_keep <- seq(1, nrow(alt_subset), 2)
alt_subset <- alt_subset[points_to_keep,]

# model settings
capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4 && setmem 2000",
                     castanea_run = "capsis -p script castaneaonly.myscripts.Simulation_EU_Victor", nb_lines_per_file = 495
)

sim_options <- list(command_file_suffix = "",
                    #species_file = "CastaneaSpecies_01_2021.txt",
                    inventory_file = file.path(wd, "calibration", "process_files", "inventory.txt"),
                    climate_name = "D:/climate/ERA5-Land/castanea_format/",
                    climate_ext = ".txt",
                    sim_name = "Test_V_",
                    nb_years = 1
)

inv_options <- list(inventory_file_suffix = "",
                    general = list(cell_size = 20, start_year = 1970,
                                   lat = 44, lon = 5),
                    model = list(CO2_mode = "CO2_PAST_EVOLUTION", elevation_mode = "ELEVATION_EFFECT_FIXED",
                                 predawn_mode = "PREDAWN_CAMP", LAI_mode = "LAI_STAND", mortality_mode = "MORTALITY_RDI",
                                 phenology_mode = "PHENO_CASTANEA", fit2018_file = "fit2018/UniChillRenecofor_2021.fit2018",
                                 drought_on_respiration = "FALSE", soil_init = "SOIL_INIT_EQ", vegetation_type = "TYPE_VEG_STAND",
                                 temperature_on_photosynthesis = "TEMPERATURE_EFFECT_BERNACCHI",
                                 ETR_mode = "ETR_FAO", aero_mode = "AERO_FAO", i_frost = 3,
                                 simulate_reproduction = "FALSE", allocation_schema = "ALLOC_SCHEMA_DAVI2009",
                                 allocation_remain = "ALLOC_REMAIN_RESERVES", allocation_repro = "REPRO_OLD",
                                 potential_from_soil_texture = "true", opt_cols = "-"),
                    output_type = 1,
                    stand = list(species_code = 3, 
                                 macropor_prop = 0, fineroot_prop = 0.5,
                                 dbh = 5, n_trees_ha = 5000, volume_ha = 12, 
                                 age = 8, clumping = 0.61, LAI = 5, opt_vars = "-")
)

grid_points <- alt_subset$cell

load("D:/soil/processed/data_soil.Rdata")

soil_prop <- data.frame(clay_top = data_soil[grid_points, "cly_top"], clay_all = data_soil[grid_points, "cly_all"], 
                        fin_top = data_soil[grid_points, "fin_top"], fin_all = data_soil[grid_points, "fin_all"], 
                        sand_top = data_soil[grid_points, "sand_top"], sand_all = data_soil[grid_points, "sand_all"])
soil_prop <- round(soil_prop, 3)

data <- list(grid = grid_points, lat = data_soil[grid_points, "lat"], lon = data_soil[grid_points, "lon"], 
             depth = round(data_soil[grid_points, "depth"],3), wfc = round(data_soil[grid_points, "FC"],3), 
             wilt = round(data_soil[grid_points, "WP"],3), stone =  round(data_soil[grid_points, "crf_all"],3), 
             bulk = round(data_soil[grid_points, "bld"],3), soil_prop = soil_prop)

species=list(name = "Fagus sylvatica", structure_file = "C:/Users/vandermeersch/Documents/CEFE/thesis/castanea/species/CastaneaSpecies_08_2021.txt")

