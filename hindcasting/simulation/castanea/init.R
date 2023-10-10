# Initialization script for CASTANEA in paleo

cat(paste0("Doing year ", year, "\n"))

# function to run model in Capsis
run_castanea <- function(runlines, ncores){
  plan(multisession, workers = ncores)
  list_lines <- split(1:nrow(runlines), cut(seq_along(1:nrow(runlines)), ncores, labels = FALSE))
  prog <- progressor(nrow(runlines))
  out <- future_lapply(1:ncores, function(i){
    for(j in unlist(list_lines[i])){
      shell(runlines[j,], intern=T)
      prog()
    }
  })
  plan(sequential)
  gc()
}

# general settings
capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4_castanea && setmem 2000",
                     castanea_run = "capsis -clfd -p script castaneaonly.myscripts.Simulation_EU_Victor", nb_lines_per_file = NULL
)

sim_options <- list(command_file_suffix = "",
                    #species_file = "CastaneaSpecies_01_2021.txt",
                    inventory_file = file.path(wd, "castanea", "run", "inventory.txt"),
                    climate_name = paste0(climate_folder,"/", year, "BP/"),
                    climate_ext = ".txt",
                    sim_name = "",
                    nb_years = 30
)



# load simulation grid
grid <- fread(file.path("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min", paste0(year, "BP"), "HadCM3B_Altitude.fit"))
grid_r <- rast(grid[,c(2,1,3)])

# soil site data
load("D:/soil/processed/data_soil.Rdata")
soil_r <- rast(lapply(names(data_soil)[3:17], function(i) rast(data_soil[,c("lon", "lat", i)])))
soil_r <- resample(soil_r, grid_r)
soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)
soil_r <- mask(focal(soil_r, w = 9, fun = "mean", na.policy ="only"), grid_r)


test <- subset(soil_r, 1)
if(length(grid_r[!is.na(grid_r)]) != length(test[!is.na(test)])){ stop("Error")}

soil_prop <- data.frame(clay_top = as.numeric(values(soil_r$cly_top, na.rm = TRUE)), clay_all = as.numeric(values(soil_r$cly_all, na.rm = TRUE)), 
                        fin_top = as.numeric(values(soil_r$fin_top, na.rm = TRUE)), fin_all = as.numeric(values(soil_r$fin_all, na.rm = TRUE)), 
                        sand_top = as.numeric(values(soil_r$sand_top, na.rm = TRUE)), sand_all = as.numeric(values(soil_r$sand_all, na.rm = TRUE)))
soil_prop <- round(soil_prop, 3)

lonlat <- as.data.frame(soil_r$WP, xy = TRUE)[1:2]
data <- list(grid = 1:nrow(grid), lat = lonlat$y, lon = lonlat$x, 
             depth = round(values(soil_r$depth, na.rm = TRUE), 3), wfc = round(values(soil_r$FC, na.rm = TRUE), 3), 
             wilt = round(values(soil_r$WP, na.rm = TRUE), 3), stone = round(values(soil_r$crf_all, na.rm = TRUE), 3), 
             bulk = round(values(soil_r$bld, na.rm = TRUE), 3), soil_prop = soil_prop)
plot(rast(data.frame(cbind(data$lon, data$lat, data$stone))))


# create needed files
capsis_settings$nb_lines_per_file <- ceiling(length(data$grid)/ncores)
create_inventoryfile(output_dir = file.path(wd, "castanea", "run"), inv_options, data)
create_runfile(capsis_settings$nb_lines_per_file, output_dir = file.path(wd, "castanea", "run"), species_file = species_file, 
               sim_options = sim_options, data = data , capsis_settings = capsis_settings, start = 1)
runlines <- read.table(file.path(wd, "castanea", "run","runfile.txt"), sep='\t')
cat(paste0("Found ", nrow(runlines), " simulation lines!\n"))
