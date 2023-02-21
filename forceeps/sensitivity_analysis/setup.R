################
# Setup script #
################

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps"

library(dplyr)
library(tibble)
library(progressr)
library(data.table)
library(future.apply)
library(vroom)
library(raster)

source(file.path(wd, "functions", "run_model.R"))
source(file.path(wd, "functions", "forceeps_model_gsa.R"))
source(file.path(wd, "functions", "create_speciesfile.R"))
source(file.path(wd, "functions", "create_commandfile.R"))
source(file.path(wd, "functions", "modify_speciesfile.R"))


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


# model settings
capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4 && setmem 2000",
                     forceeps_run = "capsis -p script forceps.myscripts.victor.SimulationLauncherVictor")


sim_options <- list(siteFileName = "unused", speciesFileName = "unused", defaultClimateFileName = "unused",
                    bareGround = "false", bareGroundPatchN = 0, bareGroundPatchArea = 0,
                    inventory = "true", inventoryFileName = "test.inv",
                    adaptative_options = list(Mode = "false", InputCompleteExport = "unused", VarianceLevel = "ADAPTIVE_VARIANCE_LEVEL_LOW",
                                              TradeOffOption = "ADAPTIVE_TRADEOFF_G_OPTION", InventoryPatchN = 0, InventoryPatchArea = 800, 
                                              Initial_laSd = 0, Initial_drtolSd = 0, Initial_gSd = 0, ExtraVariance_drtol = 0,
                                              ExtraVariance_g = 0, TreeReproductionWeight = "true"),
                    initDate = 1970, randomSeed = 98,
                    distProba = 0, deathProbaCoef = 4.605, slowGrowthProba = 0.368, slowGrowthTime = 2,
                    potentialSpeciesList = 17, crownLengthOption = "true", crownLengthImpactOnGrowth = "true",
                    regeneration_options = list(MaxTreesPerPatchOption = "false", MaxTreesPerPatchNumber = 0,
                                                MaxTreesPerPatchWithNTries = "false", MaxTreesPerPatchNTries = 0,
                                                SeedlingsPerPatchOption = "false", SeedlingsPerPatchNumber = 0,
                                                ForclimLike = "true", ActualSpeciesOnly = "true", ActualSpeciesOnlyAtPatchLevel = "false",
                                                Constraint = "true", ActualSpeciesOnlyAbundance = "false"),
                    establishmentProbabilityCoef = 0.1,
                    ppMoth_options = list(Activated = "false", RegionalPercentageOfInfestedTreesOption = "false",
                                          RegionalPercentageOfInfestedTrees = 0.12),
                    variability = "false", variabilityFileName = "unused",
                    climaticFitness = "false", fitnessImpactsGrowth = "false", 
                    management = "false", managementFileName = "unused",
                    droughtStressOption = "FORCLIM_LIKE_DROUGHT_STRESS",
                    setup_file = "C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps/calibration/test.setup"
)

species <- list(structure_file = file.path(wd, "first_tests", "forceps2.New_species"),
                name = "Fagus sylvatica")

grid_points <- alt_subset$cell

commandfile_options <- list(setupFileName = "C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps/calibration/test.setup",
                            climateFolder = "D:/climate/ERA5-Land/forceeps_format",
                            numberOfYearsToBeJumped = 1,
                            exportTimeStep = 1,
                            numberOfYears = 30,
                            speciesFileName = NULL,
                            grid = grid_points)