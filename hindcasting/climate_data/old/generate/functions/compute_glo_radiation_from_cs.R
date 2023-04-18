# function to compute daily global radiation 
# from interpolated clear-sky values and cloudiness (generated with GWGEN)

# author : V. Van der Meersch - 17/10/2022

compute_glo_radiation_from_cs <- function(rad_file, gwgen_file, output_dir){
  
  output_file <- file.path(output_dir,
                           paste0(strsplit(basename(rad_file), "_")[[1]][1], 
                                  "_", strsplit(basename(rad_file), "_")[[1]][2], "_glorad.csv"))
  
  # read data
  cloud_data <- fread(gwgen_file)
  cs_rad_data <- fread(rad_file)
  
  # compute global radiation
  glo_rad <- kasten_clearness(cloud_data$mean_cloud)*cs_rad_data$cs_radiation
  
  glo_rad_data <- data.frame(cloud_data$id, cloud_data$year, cloud_data$month, cloud_data$day, round(glo_rad,3))
  names(glo_rad_data) <- c("id", "year", "month", "day", "glo_radiation")
  
  write.table(glo_rad_data, file = output_file, col.names = T, row.names = FALSE, sep=",")
  
  message("Daily radiation computed !")
  
  return(output_file)
  
}



# functions to compute the clear-sky index for a given cloudiness 
# N.B. : clear-sky index (or solar clearness index) is the ratio global radiation/clear-sky radiation

# Kasten & Czeplak, 1980 - "developed for northern Germany"
kasten_clearness <- function(cloudiness){
  return((1-0.75*cloudiness^3.4))
}

# Antoine et al, 1996 - "developed for a global application"
antoine_clearness <- function(cloudiness){
  return(1-0.29*(cloudiness + (cloudiness)^2))
}

# Luo et al., 2010 - "developed for New Zealand"
luo_clearness <- function(cloudiness){
  return(1-1.9441*(cloudiness)^3+2.8777*(cloudiness)^2-2.2023*cloudiness)
}
