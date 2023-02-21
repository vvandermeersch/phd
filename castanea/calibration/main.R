wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/castanea"

source(file.path(wd, "functions", "cmaes_calibration.R"))
source(file.path(wd, "functions", "load_parameter_values.R"))
library(dplyr)
library(progressr)
library(future)
library(future.apply)
library(data.table)


load("D:/species/processed/fagus_sylvatica/1000pres_1000abs/occurrence_subset_1.Rdata")


#-------------------#
# Castanea settings #
#-------------------#
sim_options <- list(command_file_suffix = "",
                    #species_file = "CastaneaSpecies_01_2021.txt",
                    inventory_file = file.path(wd, "calibration", "process_files", "inventory.txt"),
                    climate_name = "D:/climate/ERA5-Land/castanea_format/",
                    climate_ext = ".txt",
                    #sim_name = "Test_V_",
                    nb_years = 30
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
                                 dbh = 9.5, n_trees_ha = 2312, volume_ha = 77, 
                                 age = 30, clumping = 0.61, LAI = 6, opt_vars = "-")
                    )

species=list(name = "Abies alba", structure_file = "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/species/CastaneaSpecies_08_2021.txt")


#-------------------#
# Simulation points #
#-------------------#
load("D:/soil/processed/data_soil.Rdata")

# extract the grid points where GSA will run 
alt_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
alt_file <- paste0(alt_folder, "/ERA5LAND_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)
alt$points <- as.numeric(rownames(alt))
grid_points <- inner_join(alt, species_occurrence, by = c('lat', 'lon'))
grid_points <- grid_points$points

soil_prop <- data.frame(clay_top = data_soil[grid_points, "cly_top"], clay_all = data_soil[grid_points, "cly_all"], 
                        fin_top = data_soil[grid_points, "fin_top"], fin_all = data_soil[grid_points, "fin_all"], 
                        sand_top = data_soil[grid_points, "sand_top"], sand_all = data_soil[grid_points, "sand_all"])
soil_prop <- round(soil_prop, 3)

data <- list(grid = grid_points, lat = data_soil[grid_points, "lat"], lon = data_soil[grid_points, "lon"], 
             depth = round(data_soil[grid_points, "depth"],3), wfc = round(data_soil[grid_points, "FC"],3), 
             wilt = round(data_soil[grid_points, "WP"],3), stone = round(data_soil[grid_points, "crf_all"],3), 
             bulk = round(data_soil[grid_points, "bld"],3), soil_prop = soil_prop)


#-----------------#
# Capsis settings #
#-----------------#
capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4 && setmem 300",
                     castanea_run = "capsis -clfd -p script castaneaonly.myscripts.Simulation_EU_Victor", nb_lines_per_file = 199)


#-----------------#
# CMA-ES settings #
#-----------------#
parallel=list(ncores_runs=1, ncores_eval=2)
controls=list(sigma=2, mu=2, lambda=4, maxit=1)

parameters <- list(init = load_parameter_values("initial_values", species$name, wd), 
                   lb = load_parameter_values("lower_bounds", species$name, wd), 
                   ub = load_parameter_values("upper_bounds", species$name, wd), 
                   fixed = c("Lignroots", "LIGNrl", "LIGNll", "LIGNfb", "LIGNcb", "LIGNcr", "NSS", "potToWood", "ratioG",
                             "CoefLAI3", "RauwPIR", "RauwPAR", "RaufPIR", "TaufPIR", "RaufPAR", "TaufPAR",
                             "emsf", "propec", "CapSoilleaf", "EaVJ", "ETT", "JMT", "teta", "TBASEC", 
                             "NSTART3", "T1rec", "decidue", "cohorLeaves", "BSSminCrit", "rateOfSeed", "rateOfSeedJourne", "seedMass_mg",
                             "seedMass_mu"), 
                   scale_factor=10)



cmaes_calibration(nruns = 1, auc_of, Yobs = species_occurrence, 
                  data = data,
                  parameters = parameters,
                  is_feasible = function(x){TRUE},
                  controls = controls,
                  parallel = parallel,
                  cmaes_settings = cmaes_settings,
                  species = species,
                  capsis_settings = capsis_settings,
                  sim_options = sim_options, inv_options = inv_options)











