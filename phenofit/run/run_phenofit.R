wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/phenofit/run/"
source(paste0(wd, "/functions/command_file_setup.R"))


command_file <- paste0(wd, "Command_file.txt")

#species_file <- "D:/calibrations/phenofit/abies_alba/1000pres_1000abs/07-05/cmaes_fit_2022-05-07.species"
species_file <- "C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/forward/Fagus_sylvatica_VVanderMeersch.species"
#species_file <- "C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/backward/abies_alba/Abies_alba_init.species"
#species_file <- "C:/Users/vandermeersch/Documents/temp/abies_alba_2.species"
#species_file <- "D:/calibrations/phenofit/fagus_sylvatica/ABC/11-06/abc_fit_2022-06-11.species"
#species_file <- "C:/Users/vandermeersch/Documents/temp/error_q_ilex/cmaes/constrait_corrected/quercus_ilex_17_m.species"
  
#output_folder <- paste0(wd, "test_debug")
output_folder <- "D:/simulations/phenofit/paleo/test"
#output_folder <- "D:/simulations/phenofit/backward/fagus_sylvatica/ABC/11-06"
#output_folder <- "C:/Users/vandermeersch/Documents/temp/error_q_ilex/cmaes/constrait_corrected/out"

# climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
#climate_folder <-  "D:/climate/ERA5-Land/phenofit_format/fagus_sylvatica_extraction/2000pres_2000abs/subset_5"
#climate_folder <-  "D:/climate/ERA5-Land/phenofit_format/abies_alba_extraction/1000pres_1000abs/subset_1"
#climate_folder <-  "D:/climate/ERA5-Land/phenofit_format/quercus_ilex_extraction/1000pres_1000abs/subset_1"
climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg/5000BP"

command_file_setup(command_file, species_file, output_folder, climate_folder,
                   climate_scenario="HadCM3B", starting_year="-5014", ending_year="-4985", quiet_mode="true")


capsis_settings=list(java8="java8.cmd", cd_java8="cd C:/Program Files/Java/scripts", cd_capsis="cd/d D:/applications/capsis4 && setmem 30000")
run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptPaleo", command_file)
run <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')

system.time(shell(run, intern=F))


