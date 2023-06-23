
# Castanea

create_inventoryfile <- function(output_dir, inv_options, data){
  
  inventory_file <- paste0(output_dir, "/inventory",  inv_options$inventory_file_suffix, ".txt")
  
  header <- c("# Inventory file for Castanea", paste0('# Generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date()), "")
  write(header, inventory_file)
  
  general_options <- c("# General",
                       paste("fmCellWidth","=", inv_options$general$cell_size, sep="\t"),
                       paste("year","=", inv_options$general$start_year, sep="\t"),
                       paste("latitude","=", inv_options$general$lat, sep="\t"),
                       paste("longitude","=", inv_options$general$lon, sep="\t"),
                       "")
  write(general_options, inventory_file, append = TRUE)
  
  castanea_options <- c("# CASTANEA parameters",
                        paste("CO2mode","=", inv_options$model$CO2_mode, sep="\t"),
                        paste("Ca","=", inv_options$model$Ca, sep="\t"),
                        paste("elevationEffect","=", inv_options$model$elevation_mode, sep="\t"),
                        paste("predawnModel","=", inv_options$model$predawn_mode, sep="\t"),
                        paste("LAImode","=", inv_options$model$LAI_mode, sep="\t"),
                        paste("mortalityModel","=", inv_options$model$mortality_mode, sep="\t"),
                        paste("typeOfVegetation","=", inv_options$model$vegetation_type, sep="\t"),
                        paste("phenoMode","=", inv_options$model$phenology_mode, sep="\t"),
                        paste("fit2018FileName","=", inv_options$model$fit2018_file, sep="\t"),
                        paste("droughtOnRespiration","=", inv_options$model$drought_on_respiration, sep="\t"),
                        paste("soilInit","=", inv_options$model$soil_init, sep="\t"),
                        paste("temperatureEffectOnPhotosynthesis","=", inv_options$model$temperature_on_photosynthesis, sep="\t"),
                        paste("allocRemain","=", inv_options$model$allocation_remain, sep="\t"),
                        paste("ETRMode","=", inv_options$model$ETR_mode, sep="\t"),
                        paste("aeroMode","=", inv_options$model$aero_mode, sep="\t"),
                        paste("iFROST","=", inv_options$model$i_frost, sep="\t"),
                        paste("simulationReproduction","=", inv_options$model$simulate_reproduction, sep="\t"),
                        paste("allocRepro","=", inv_options$model$allocation_repro, sep="\t"),
                        paste("allocSchema","=", inv_options$model$allocation_schema, sep="\t"),
                        paste("parametPotFromSoil","=", inv_options$model$potential_from_soil_texture, sep="\t"),
                        paste("initWoodByVolume","=", inv_options$init_wood_by_volume, sep="\t"),
                        paste("stomataStress","=", inv_options$model$stomata_stress, sep="\t"),
                        "",
                        paste("output","=", inv_options$output_type, sep="\t"),
                        "",
                        paste("optionalVariables","=", inv_options$model$opt_cols, sep="\t"),
                        "")
  write(castanea_options, inventory_file, append = TRUE)
  
  cell_header <- paste("# Type", "Id",	"Species", "xc", "yc", "zc", "soilHeight", "stone", "wfc", "wilt", "propMacro", "bulk", 
                 "SOLCLAYtop", "SOLCLAYsol", "SOLFINtop", "SOLFINsol", "SOLSANDtop", "SOLSANDsol", 
                 "prac", "pracDeep", "deepSoilDep", "stoneDeep", "MacroDeep", 
                 "dbh", "Nha", "Vha", "age", "clumping", "LAI", "optional variable", sep="\t")
  write(cell_header, inventory_file, append = TRUE)
  
  cells <- paste("cell", 1, inv_options$stand$species_code, 200, 200, 0, 0, 0, 0, 0, inv_options$stand$macropor_prop, 0, 0, 0, 0, 0, 0, 0, 
                 inv_options$stand$fineroot_prop, 0, 0, 0, 0, 
                 inv_options$stand$dbh, inv_options$stand$n_trees_ha,
                 inv_options$stand$volume_ha, inv_options$stand$age, inv_options$stand$clumping, inv_options$stand$LAI, inv_options$stand$opt_vars, sep="\t")
  write(cells, inventory_file, append = TRUE)
  
  
  
  
}