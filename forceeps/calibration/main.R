wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps"

source(file.path(wd, "functions", "cmaes_calibration.R"))
source(file.path(wd, "functions", "load_parameter_values.R"))


load("D:/species/processed/fagus_sylvatica/1000pres_1000abs/occurrence_subset_1.Rdata")


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



commandfile_options <- list(setupFileName = sim_options$setup_file,
                            climateFolder = "D:/climate/ERA5-Land/forceeps_format",
                            numberOfYearsToBeJumped = 1,
                            exportTimeStep = 1,
                            numberOfYears = 5,
                            speciesFileName = NULL,
                            grid = grid_points)

#-----------------#
# CMA-ES settings #
#-----------------#
parallel=list(ncores_runs=1, ncores_eval=1)
controls=list(sigma=2, mu=10, lambda=20, maxit=100)


#parameters_fit <- c("kS", "kHMax", "kAMax", "kG", "kDDMin", "kWiTN", "kWiTX", "kTHot", "kTCo2Hot", "kDrTol")
parameters <- list(init = load_parameter_values("inital_values", "Fagus sylvatica", wd), 
                   lb = load_parameter_values("lower_bounds", "Fagus sylvatica", wd), 
                   ub = load_parameter_values("upper_bounds", "Fagus sylvatica", wd), 
                   fixed = c("kBrow", "kLQ", "kImmT", "ageMaturityForReproduction"), 
                   scale_factor=10)



cmaes_calibration(nruns=1, obj_function = auc_of, Yobs = species_occurrence, 
                  parameters = parameters,
                  is_feasible = function(x){TRUE},
                  controls= controls,
                  parallel= parallel,
                  cmaes_settings=list(rand_init=FALSE, ipop=FALSE, ipop_factor=NULL),
                  capsis_settings = capsis_settings,
                  species = species,
                  sim_options = sim_options, commandfile_options = commandfile_options)

