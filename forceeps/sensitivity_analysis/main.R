wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/forceeps"

library(dplyr)
library(tibble)
library(sensobol)
library(progressr)
library(data.table)
library(future.apply)

source(file.path(wd, "functions", "create_speciesfile.R"))
source(file.path(wd, "functions", "create_commandfile.R"))
source(file.path(wd, "functions", "modify_speciesfile.R"))
source(file.path(wd, "functions", "run_model.R"))

capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4 && setmem 2000",
                     forceeps_run = "capsis -p script forceps.myscripts.victor.SimulationLauncherVictor")

# extract the zone  
climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
alt_file <- paste0(climate_folder, "/ERA5LAND_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)

alt_subset <- alt %>%
  rownames_to_column('cell') %>%
  subset(lat<46 & lat>42 & lon<8 & lon>4)

ggplot(data=alt_subset, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = alt)) +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=FALSE, mid=700)

# parameters 
n_parameters <- 10
parameters <- c("kS", "kHMax", "kAMax", "kG", "kDDMin", "kWiTN", "kWiTX", "kTHot", "kTCo2Hot", "kDrTol")
parameters_lb <- c(20, 10, 50, 10, 100, -50, -20, 10, 40, 0)
parameters_ub <- c(200, 100, 3000, 1000, 2000, 10, 30, 40, 50, 1)
parameters_fixed <- c("kType", "kNTol", "kBrow", "kLy", "kLa", "kLQ", "kImmT", "ageMaturityForReproduction")

# sobol matrices
#N <- 2 ^ 10
N <- 2 ^ 10
mat <- sobol_matrices(N = N, params = parameters) # 0-1
colnames_save <- colnames(mat)
mat <- mat %*% diag(parameters_ub - parameters_lb) + matrix(parameters_lb, nrow=nrow(mat), ncol=length(parameters_lb), byrow=TRUE) # real ranges
colnames(mat) <- colnames_save


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
                    setup_file = "C:/Users/vandermeersch/Documents/CEFE/phd/forceeps/calibration/test.setup"
)

species <- list(structure_file = file.path(wd, "first_tests", "forceps2.New_species"),
                name = "Fagus sylvatica")



# create species_file
grid_points <- alt_subset$cell
commandfile_options <- list(setupFileName = "C:/Users/vandermeersch/Documents/CEFE/phd/forceeps/calibration/test.setup",
                            climateFolder = "D:/climate/ERA5-Land/forceeps_format",
                            numberOfYearsToBeJumped = 1,
                            exportTimeStep = 1,
                            numberOfYears = 10,
                            speciesFileName = NULL,
                            grid = grid_points)





structure_file <- file.path(wd, "first_tests", "forceps2.New_species")
runlines <- c()

for(i in 1:nrow(mat)){
  ind_folder <- file.path(wd, "sensitivity_analysis", "process_files2", paste0("ind",i))
  dir.create(ind_folder, showWarnings = FALSE)
  
  # create species_file
  species_file <- file.path(ind_folder, paste0("ind", i, ".species"))
  create_speciesfile(file = species_file, structure_file, species_name = "Fagus sylvatica")
  modify_speciesfile(new_params = mat[i,], param_fixed=parameters_fixed, file = species_file)
  
  # create command_file
  command_file <- file.path(ind_folder, "commandfile")
  create_commandfile(command_file, commandfile_options, species_file = paste0("ind", i, ".species"))
  
  # save runline
  if(!is.null(capsis_settings$cd_java8)){
    run <- paste(capsis_settings$cd_java8, capsis_settings$java8, 
                 capsis_settings$cd, paste(capsis_settings$forceeps_run, command_file), sep=' && ')
  }else{
    run <- paste(capsis_settings$cd, paste(capsis_settings$forceeps_run, command_file), sep=' && ')
  }
  runlines <- c(runlines, run)
  
}


with_progress(system.time(run_model(runlines, ncores = 20)))
# 6490s for 1200 sim on 20 cores
# 35394s for 12288 sim on 20 cores

output_path <- "C:/Users/vandermeersch/Documents/CEFE/phd/forceeps/sensitivity_analysis/process_files3"
files <- list.files(path = output_path, pattern = "\\.siteproductivity.txt$", recursive = T)


# for one site
site <- "75425" 
site_outputfiles <- grep(site, files, value = TRUE)
cnames <- c("date", "patchId", "speciesId", "speciesShortName", "adultProdBasalArea", "adultProdBiomass", "adultTreeBasalArea", "adultTreeBiomass", 
            "deadBasalArea", "deadBiomass", "saplingBasalArea", "saplingBiomass", "adultTreeNumber", "deadNumber", "saplingNumber", "droughtIndexAnnual",
            "droughtIndexSeasonal")


output_list <- lapply(site_outputfiles, function(i){
  fread(file.path(output_path, i), skip=9, col.names=cnames) %>%
    dplyr::select("date", "patchId", "adultTreeBasalArea") %>% 
    group_by(patchId) %>%
    summarise(meanBasalArea = mean(adultTreeBasalArea))
})

output <- rbindlist(output_list)
output <- output[output$patchId==1,]

# error diagnostic
#some points are missing (36), which ones ?
inds <- str_match(site_outputfiles, "ind\\s*(.*?)\\s*/output") 
inds <- as.numeric(inds[,2])
range <- 1:12288
range[!(range %in% inds)] # for example, ind510 or ind 2558 are missing
shell(runlines[2558], intern=F) # error, "could not find any algf value (intercepted no layer ?)" 


ind <- sobol_indices(Y = as.numeric(output$meanBasalArea), N = N, params = parameters)
