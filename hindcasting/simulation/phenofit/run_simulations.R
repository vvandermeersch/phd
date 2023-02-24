
##################################
#                                #
# RUN PHENOFIT PALEO SIMULATIONS #
#                                #
##################################

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation"

source(file.path(wd, "functions/command_file_setup.R"))
source(file.path(wd, "functions/read_mean_outputvalue.R"))


# configuration

species_file <- "C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/forward/Fagus_sylvatica_VVanderMeersch.species"
# species_file <- "D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/paper_data/CMAES/subset_4/cmaes_fit_subset4_rep1.species"
# species_file <- "C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/forward/Abies_alba_VVanderMeersch2.species"
output_folder <- "D:/simulations/phenofit/paleo/test"
climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg"
command_file <- file.path(wd, "phenofit","command_file.txt")


# capsis settings

capsis_settings=list(java8="java8.cmd", cd_java8="cd C:/Program Files/Java/scripts", cd_capsis="cd/d D:/applications/capsis4 && setmem 30000")
run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptPaleo", command_file)
run <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')


# simulation loop

for(year in seq(11000, 10000, -200)){
  yr_b <- as.character(- year - 14)
  yr_e <- as.character(- year + 15)
  command_file_setup(command_file, species_file, output_folder, climate_folder,
                     climate_scenario="HadCM3B", starting_year=yr_b, ending_year=yr_e, quiet_mode="true")
  system.time(shell(run, intern=F))
  
  fitness <- read_mean_outputvalue(file.path(output_folder, paste0(year, "BP")), output_var = "Fitness")
  saveRDS(fitness, file.path(output_folder, paste0(year, "BP"), "fitness.rds"))
}






