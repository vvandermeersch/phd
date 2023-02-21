

# nb_files : number of distinct command files to generate

# sim_options <- list(command_file_suffix = "",
#                     
#                     inventory_file = "inventory_Fagus.txt",
#                     climate_name = "data/castaneaonly/climate/WATCH/",
#                     climate_ext = ".txt",
#                     opt_cols = "-",
#                     sim_name = "WATCH_Fagus_",
#                     nb_years = 30,
#                     scenario_file = "UniChillRenecofor_2021.fit2018"
#                     )
# 
# output_dir <- "C:/Users/vandermeersch/Documents/CEFE/thesis/calibration/castanea/command_files"
# 
# data <- list(lat = latitude, lon = longitude, alt = altitude,
#              wfc = wfc, wilt = wilt,
#              stone = stone, bulk = bulk, soil_prop = soil_prop,
#              mesh_corres = "NULL",
#              opt_vars = opt_vars)


create_commandfiles <- function(nb_lines_per_file, output_dir, sim_options, sim_name, species_file, data){
  
  header <- c("# Command file for Castanea", paste0('# Generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date()), "")
  
  filenames <- c("# The filenames below are relative to the folder containing this command file",
                 paste("castaneaFileName","=", species_file, sep="\t"),
                 paste("inventoryFileName","=", sim_options$inventory_file, sep="\t"),
                 "",
                 paste("climateBaseName","=", sim_options$climate_name, sep="\t"),
                 paste("climateExtension","=", sim_options$climate_ext, sep="\t"),
                 paste("simulationName","=", sim_name , sep="\t"),
                 "", "")
  
  colnames <- paste("#	siteId", "gridName", "latitude", "longitude", "soilHeight", "stone", "wfc", "wilt", "bulk", 
                    "SOLCLAYtop", "SOLCLAYsol", "SOLFINtop", "SOLFINsol", "SOLSANDtop", "SOLSANDsol", 
                    "numberOfYears", 
                    sep="\t")
  
  # number of lines per file
  nb_lines <- length(data$lat)
  if(nb_lines < nb_lines_per_file){
    stop("Total number of lines lower than number of lines per file you asked for !")
  }
  nb_files <- ceiling(nb_lines/nb_lines_per_file)
  
  if(nb_files == 1){
    list_lines <- list(1:nb_lines)
  }else{
    list_lines <- split(1:nb_lines, cut(seq_along(1:nb_lines), nb_files, labels = FALSE))
  }
  
  # loop on command files needed  
  for(i in 1:nb_files){
    
    site_id <- 1
    
    command_file <- paste0(output_dir, "/commandfile",  sim_options$command_file_suffix, "_", i, ".txt")
    
    write(header, command_file)
    write(filenames, command_file, append=TRUE)
    write(colnames, command_file, append=TRUE)
    
    # loop on lines to add to the files
    for(j in unlist(list_lines[i])){
      
      vals <- paste(site_id, format(data$grid[j], scientific = FALSE), data$lat[j], data$lon[j], data$depth[j], data$stone[j], data$wfc[j], data$wilt[j], data$bulk[j],
                    data$soil_prop$clay_top[j], data$soil_prop$clay_all[j], data$soil_prop$fin_top[j],
                    data$soil_prop$fin_all[j], data$soil_prop$sand_top[j], data$soil_prop$sand_all[j],
                    sim_options$nb_years,
                    sep="\t")

      write(vals, command_file,append=TRUE)
      
      site_id <- site_id + 1
    }
    
  }
  
}
