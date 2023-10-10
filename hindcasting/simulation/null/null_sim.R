library(terra)

sp_folder <- "D:/species/processed"

output_folder <- "D:/simulations/null/paleo/025deg"


current_distribution <- readRDS(file.path(sp_folder, "quercus_ilex/quercus_ilex_presabs_woUkraine.rds"))
current_distribution <- rast(current_distribution)
plot(current_distribution)
current_distribution[is.na(current_distribution)] <- 0

for(yr in seq(250,15000,250)){
  past_grid <- readRDS(paste0("D:/simulations/phenofit/paleo/expert/025deg/quercus_robur/", yr, "BP.rds"))
  past_grid <- rast(past_grid[c(2,1,3)])
  current_distribution <- extend(current_distribution, past_grid)
  
  null_distribution <- mask(resample(current_distribution, past_grid, method = "average"), past_grid)
  null_distribution <- as.data.frame(null_distribution, xy = TRUE)[c(2,1,3)]
  names(null_distribution) <- c("lat", "lon", "pred")
  saveRDS(null_distribution, file.path(output_folder, "quercus_ilex", paste0(yr, "BP.rds")))
  
}
