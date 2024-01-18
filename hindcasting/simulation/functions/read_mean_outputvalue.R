library(data.table) #fastest
setDTthreads(1) #only one thread (in case of parallel cmaes)

read_mean_outputvalue <- function(output_folder, model = "PHENOFIT", output_var = "Fitness",
                                  year = NULL, num_years = NULL){
  
  if(model == "PHENOFIT"){
    output <- fread(paste0(output_folder,"/", output_var, ".txt"), header=T, sep="\t", fill=T)
    output_mean <- apply(output[c(-1,-2),-1], 2, mean)
    output <- data.frame(lat = as.numeric(output[1,-1]), lon = as.numeric(output[2,-1]), value = output_mean)
  }else if(model == "CASTANEA"){
    
    # load simulation grid
    grid <- fread(file.path("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min", paste0(year, "BP"), "HadCM3B_Altitude.fit"))
    grid_r <- rast(grid[,c(2,1,3)])
    load("D:/soil/processed/data_soil.Rdata")
    soil_r <- rast(lapply(names(data_soil)[3:17], function(i) rast(data_soil[,c("lon", "lat", i)])))
    soil_r <- resample(soil_r, grid_r)
    soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
    soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
    soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
    soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
    sim_grid <- subset(soil_r, 1)
    if(length(grid_r[!is.na(grid_r)]) != length(sim_grid[!is.na(sim_grid)])){ stop("Error")}
    
    # load data
    files_to_read <- mixedsort(list.files(output_folder, 
                                          pattern = "yearlyResults.log", full.names = TRUE))
    yearly_results <- vroom(files_to_read, col_select = all_of(output_var), show_col_types = FALSE, progress=FALSE)
    yearly_results[is.na(yearly_results)] <- 0 # NA values mean tree is dead (thus biomass = 0) 
    mean_results <- aggregate(yearly_results, 
                              list(rep(1:(nrow(yearly_results) %/% num_years + 1), 
                                       each = num_years, len = nrow(yearly_results))), mean)[-1]
    mean_results[mean_results <0 ] <- 0 # negative mean values mean tree is dead (thus biomass = 0)
    sim_grid[!is.na(sim_grid)] <- mean_results
    output <- as.data.frame(sim_grid, xy = T)[,c(2,1,3)]
    names(output) <- c("lat", "lon", "value")
    
  }else if(model == "CASTANEA_present"){
    
    # load simulation grid
    ERA5_points <- readRDS("D:/climate/ERA5-Land/ERA5_Europe_points.rds")
    # ERA5_points <- fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_Altitude.fit")
    sim_grid <- rast(ERA5_points[,c(2,1,3)])
    
    # load data
    files_to_read <- mixedsort(list.files(output_folder, 
                                          pattern = "yearlyResults.log", full.names = TRUE))
    yearly_results <- vroom(files_to_read, col_select = all_of(output_var), show_col_types = FALSE, progress=FALSE, num_threads = 40)
    yearly_results[is.na(yearly_results)] <- 0 # NA values mean tree is dead (thus biomass = 0) 
    mean_results <- aggregate(yearly_results, 
                              list(rep(1:(nrow(yearly_results) %/% num_years + 1), 
                                       each = num_years, len = nrow(yearly_results))), mean)[-1]
    mean_results[mean_results <0 ] <- 0 # negative mean values mean tree is dead (thus biomass = 0)
    sim_grid[!is.na(sim_grid)] <- mean_results
    output <- as.data.frame(sim_grid, xy = T)[,c(2,1,3)]
    names(output) <- c("lat", "lon", "value")
    
  }
  
  
  
  return(output)
}
