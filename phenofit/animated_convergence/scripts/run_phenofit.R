# Script to run Phenofit


source(paste0(wd, "/functions/command_file_setup.R"))


command_file <- paste0(wd, "Command_file.txt")

climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"

command_file_setup(command_file, species_file, output_folder, climate_folder,
                   climate_scenario="ERA5LAND", starting_year="1970", ending_year="2000", quiet_mode="true")

capsis_settings=list(java8="java8.cmd", cd_java8="cd C:/Program Files/Java/scripts", cd_capsis="cd/d D:/applications/capsis4 && setmem 30000")
run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptVictor", command_file)
run <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')

system.time(shell(run, intern=F))


