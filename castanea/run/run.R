wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/castanea"

source(file.path(wd, "functions", "cmaes_calibration.R"))
source(file.path(wd, "functions", "read_mean_outputvalue.R"))

library(future)
library(future.apply)
library(progressr)
library(vroom)

## Test data ----
capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4_castanea && setmem 2000",
                     castanea_run = "capsis -clfd -p script castaneaonly.myscripts.Simulation_EU_Victor", nb_lines_per_file = 5171
                     )

sim_options <- list(command_file_suffix = "",
                    #species_file = "CastaneaSpecies_01_2021.txt",
                    inventory_file = file.path(wd, "run", "inventory.txt"),
                    climate_name = "D:/climate/ERA5-Land/castanea_format/",
                    climate_ext = ".txt",
                    sim_name = "Quercus_",
                    nb_years = 30
                    )

# inv_options <- list(inventory_file_suffix = "",
#                     general = list(cell_size = 20, start_year = 1970,
#                                    lat = 44, lon = 5),
#                     model = list(CO2_mode = "CO2_PAST_EVOLUTION", elevation_mode = "ELEVATION_EFFECT_FIXED",
#                                  predawn_mode = "PREDAWN_CAMP", LAI_mode = "LAI_STAND", mortality_mode = "MORTALITY_RDI",
#                                  phenology_mode = "PHENO_CASTANEA", fit2018_file = "fit2018/UniChillRenecofor_2021.fit2018",
#                                  drought_on_respiration = "FALSE", soil_init = "SOIL_INIT_EQ", vegetation_type = "TYPE_VEG_STAND",
#                                  temperature_on_photosynthesis = "TEMPERATURE_EFFECT_BERNACCHI",
#                                  ETR_mode = "ETR_FAO", aero_mode = "AERO_FAO", i_frost = 3,
#                                  simulate_reproduction = "FALSE", allocation_schema = "ALLOC_SCHEMA_DAVI2009",
#                                  allocation_remain = "ALLOC_REMAIN_RESERVES", allocation_repro = "REPRO_OLD",
#                                  potential_from_soil_texture = "true", stomata_stress = "STOMATA_STRESS_GRANIER",
#                                  opt_cols = "-"),
#                     init_wood_by_volume = "true",
#                     output_type = 1,
#                     stand = list(species_code = 3,
#                                  macropor_prop = 0, fineroot_prop = 0.5,
#                                  dbh = 9.5, n_trees_ha = 2312, volume_ha = 77,
#                                  age = 30, clumping = 0.61, LAI = 6, opt_vars = "-")
#                     )

# inv_options <- list(inventory_file_suffix = "",
#                     general = list(cell_size = 20, start_year = 1970,
#                                    lat = 44, lon = 5),
#                     model = list(CO2_mode = "CO2_PAST_EVOLUTION", elevation_mode = "ELEVATION_EFFECT_FIXED",
#                                  predawn_mode = "PREDAWN_CAMP", LAI_mode = "LAI_STAND", mortality_mode = "MORTALITY_RDI",
#                                  phenology_mode = "PHENO_CASTANEA", fit2018_file = "fit2018/UniChillRenecofor_2021.fit2018",
#                                  drought_on_respiration = "FALSE", soil_init = "SOIL_INIT_EQ", vegetation_type = "TYPE_VEG_STAND",
#                                  temperature_on_photosynthesis = "TEMPERATURE_EFFECT_BERNACCHI",
#                                  ETR_mode = "ETR_FAO", aero_mode = "AERO_FAO", i_frost = 3,
#                                  simulate_reproduction = "FALSE", allocation_schema = "ALLOC_SCHEMA_DAVI2009",
#                                  allocation_remain = "ALLOC_REMAIN_RESERVES", allocation_repro = "REPRO_OLD",
#                                  potential_from_soil_texture = "true", stomata_stress = "STOMATA_STRESS_RAMBAL",
#                                  opt_cols = "-"),
#                     init_wood_by_volume = "true",
#                     output_type = 1,
#                     stand = list(species_code = 11,
#                                  macropor_prop = 0, fineroot_prop = 0.5,
#                                  dbh = 7, n_trees_ha = 6500, volume_ha = 70,
#                                  age = 70, clumping = 0.7, LAI = 1.7, opt_vars = "-")
# )
# 7	6500	70	70 puechabon Hendrik

# inv_options <- list(inventory_file_suffix = "",
#                     general = list(cell_size = 20, start_year = 1970,
#                                    lat = 44, lon = 5),
#                     model = list(CO2_mode = "CO2_PAST_EVOLUTION", elevation_mode = "ELEVATION_EFFECT_FIXED",
#                                  predawn_mode = "PREDAWN_CAMP", LAI_mode = "LAI_STAND", mortality_mode = "MORTALITY_RDI",
#                                  phenology_mode = "PHENO_CASTANEA", fit2018_file = "fit2018/UniChillRenecofor_2021.fit2018",
#                                  drought_on_respiration = "FALSE", soil_init = "SOIL_INIT_EQ", vegetation_type = "TYPE_VEG_STAND",
#                                  temperature_on_photosynthesis = "TEMPERATURE_EFFECT_BERNACCHI",
#                                  ETR_mode = "ETR_FAO", aero_mode = "AERO_FAO", i_frost = 3,
#                                  simulate_reproduction = "FALSE", allocation_schema = "ALLOC_SCHEMA_DAVI2009",
#                                  allocation_remain = "ALLOC_REMAIN_RESERVES", allocation_repro = "REPRO_OLD",
#                                  potential_from_soil_texture = "true", stomata_stress = "STOMATA_STRESS_GRANIER",
#                                  opt_cols = "-"),
#                     init_wood_by_volume = "true",
#                     output_type = 1,
# stand = list(species_code = 1,
#              macropor_prop = 0, fineroot_prop = 0.5,
#              dbh = 10, n_trees_ha = 350, volume_ha = 180,
#              age = 24, clumping = 0.56, LAI = 5.41, opt_vars = "-")
# )
# ventoux Hendrik : 10	350	180	24	0.56	5.41
# Jagodzinski et al : dbh = 15.9, n_trees_ha = 1852, volume_ha = 283.4, age = 40, clumping = 0.5, LAI = 7


# inv_options <- list(inventory_file_suffix = "",
#                     general = list(cell_size = 20, start_year = 1970,
#                                    lat = 44, lon = 5),
#                     model = list(CO2_mode = "CO2_PAST_EVOLUTION", elevation_mode = "ELEVATION_EFFECT_FIXED",
#                                  predawn_mode = "PREDAWN_CAMP", LAI_mode = "LAI_STAND", mortality_mode = "MORTALITY_RDI",
#                                  phenology_mode = "PHENO_CASTANEA", fit2018_file = "fit2018/UniChillRenecofor_2021.fit2018",
#                                  drought_on_respiration = "FALSE", soil_init = "SOIL_INIT_EQ", vegetation_type = "TYPE_VEG_STAND",
#                                  temperature_on_photosynthesis = "TEMPERATURE_EFFECT_BERNACCHI",
#                                  ETR_mode = "ETR_FAO", aero_mode = "AERO_FAO", i_frost = 3,
#                                  simulate_reproduction = "FALSE", allocation_schema = "ALLOC_SCHEMA_DAVI2009",
#                                  allocation_remain = "ALLOC_REMAIN_RESERVES", allocation_repro = "REPRO_OLD",
#                                  potential_from_soil_texture = "true", stomata_stress = "STOMATA_STRESS_GRANIER",
#                                  opt_cols = "-"),
#                     init_wood_by_volume = "true",
#                     output_type = 1,
#                     stand = list(species_code = 12,
#                                  macropor_prop = 0, fineroot_prop = 0.5,
#                                  dbh = 15, n_trees_ha = 1200, volume_ha = 126,
#                                  age = 42, clumping = 0.7, LAI = 6, opt_vars = "-")
# )

inv_options <- list(inventory_file_suffix = "",
                    general = list(cell_size = 20, start_year = 1970,
                                   lat = 44, lon = 5),
                    model = list(CO2_mode = "CO2_PAST_EVOLUTION", elevation_mode = "ELEVATION_EFFECT_FIXED",
                                 predawn_mode = "PREDAWN_CAMP", LAI_mode = "LAI_STAND", mortality_mode = "MORTALITY_RDI",
                                 phenology_mode = "PHENO_CASTANEA", fit2018_file = "fit2018/UniChillRenecofor_2021.fit2018",
                                 drought_on_respiration = "FALSE", soil_init = "SOIL_INIT_EQ", vegetation_type = "TYPE_VEG_STAND",
                                 temperature_on_photosynthesis = "TEMPERATURE_EFFECT_BERNACCHI",
                                 ETR_mode = "ETR_FAO", aero_mode = "AERO_FAO", i_frost = 3,
                                 simulate_reproduction = "FALSE", allocation_schema = "ALLOC_SCHEMA_DAVI2009",
                                 allocation_remain = "ALLOC_REMAIN_RESERVES", allocation_repro = "REPRO_OLD",
                                 potential_from_soil_texture = "true", stomata_stress = "STOMATA_STRESS_GRANIER",
                                 opt_cols = "-"),
                    init_wood_by_volume = "true",
                    output_type = 1,
                    stand = list(species_code = 14,
                                 macropor_prop = 0, fineroot_prop = 0.5,
                                 dbh = 14.66, n_trees_ha = 1286, volume_ha = 62.56,
                                 age = 32, clumping = 0.61, LAI = 8, opt_vars = "-")
)
# h. davi renecofor site CHP_70




load("D:/soil/processed/data_soil.Rdata")

#grid_points <- sample(1:101510, 400, replace=FALSE)
ERA5_points <- readRDS("D:/climate/ERA5-Land/ERA5_Europe_points.rds")
grid_points <- ERA5_points$points



# extract the grid points 
# library(data.table)
# species_occurrence <- readRDS("C:/Users/vandermeersch/Documents/temp/tgcc_quercus/occurrence_subset_1.rds")
# alt_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
# alt_file <- paste0(alt_folder, "/ERA5LAND_", "Altitude.fit")
# alt <- fread(alt_file, showProgress=F)
# colnames(alt) <- c("lat", "lon", "alt")
# alt$lat <- round(alt$lat, 1)
# alt$lon <- round(alt$lon, 1)
# alt$points <- as.numeric(rownames(alt))
# grid_points <- inner_join(species_occurrence, alt, by = c('lat', 'lon'))
# grid_points <- grid_points$points







soil_prop <- data.frame(clay_top = data_soil[grid_points, "cly_top"], clay_all = data_soil[grid_points, "cly_all"], 
                        fin_top = data_soil[grid_points, "fin_top"], fin_all = data_soil[grid_points, "fin_all"], 
                        sand_top = data_soil[grid_points, "sand_top"], sand_all = data_soil[grid_points, "sand_all"])
soil_prop <- round(soil_prop, 3)

data <- list(grid = grid_points, lat = data_soil[grid_points, "lat"], lon = data_soil[grid_points, "lon"], 
             depth = data_soil[grid_points, "depth"], wfc = round(data_soil[grid_points, "FC"],3), 
             wilt = round(data_soil[grid_points, "WP"],3), stone = round(data_soil[grid_points, "crf_all"],3), 
             bulk = round(data_soil[grid_points, "bld"],3), soil_prop = soil_prop)

species=list(name = "Quercus robur", structure_file = "C:/Users/vandermeersch/Documents/CEFE/phd/castanea/species/CastaneaSpecies_08_2021.txt")


## Create needed files ----

create_inventoryfile(output_dir = file.path(wd, "run"), inv_options, data)
create_runfile(capsis_settings$nb_lines_per_file, output_dir = file.path(wd, "run"), species_file = "CastaneaSpecies_08_2021.txt", 
               sim_options = sim_options, data = data , capsis_settings = capsis_settings, start = 1)

runlines <- read.table(file.path(wd, "run","runfile.txt"), sep='\t')


## Run the model ----

ncores <- 15
list_lines <- split(1:nrow(runlines), cut(seq_along(1:nrow(runlines)), ncores, labels = FALSE))

plan(multisession, workers = ncores)
handlers("progress")

run <- function(){
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

unlink(paste0("D:/applications/capsis4_castanea/var"), recursive=TRUE)
with_progress(system.time(run()))



# Read output variables D:/simulations/castanea/forward/quercus_petraea_capsisrevision18131


reserve_biomass <- read_mean_outputvalue(grid_points, lambda = 1, output_folder = 'D:/simulations/castanea/forward/quercus_petraea_capsisrevision18131', var = "BiomassOfReserves")
npp <- read_mean_outputvalue(grid_points, lambda = 1, output_folder = 'D:/applications/capsis4_save/var', var = "NPP")
gpp <- read_mean_outputvalue(grid_points, lambda = 1, output_folder = 'D:/applications/capsis4_save/var', var = "GPP")
reserve_biomass <- t(reserve_biomass)
plan(sequential)

plan(multisession, workers = 20)
npp <- read_mean_outputvalue_2(grid_points, output_folder = 'D:/simulations/castanea/first_tests/01-04', var = "NPP")
npp <- t(npp)

reserve_biomass_m <- reserve_biomass
# reserve_biomass_m[which(is.na(reserve_biomass_m))] <- 0
reserve_biomass_m[reserve_biomass_m<0] <- 0
reserve_biomass_m[is.na(reserve_biomass_m)] <- 0
reserve_biomass_m[reserve_biomass_m > quantile(reserve_biomass_m, 0.99)] <- quantile(reserve_biomass_m, 0.99) #test // cmaes output

npp_m <- npp
# reserve_biomass_m[which(is.na(reserve_biomass_m))] <- 0
npp_m[npp_m<0] <- 0


tmoy <- read_mean_outputvalue(grid_points, lambda = 1, output_folder = 'D:/simulations/castanea/first_tests/31-03', var = "Tmoy")
data_climate <- data.frame(data$lat, data$lon, t(tmoy), t(glorad), t(prec), t(rh))
names(data_climate) <- c("lat", "lon", "tmoy")


# Plot

library(ggplot2)
library(colorspace)

data_plot <- data.frame(data$lat, data$lon, reserve_biomass_m)
names(data_plot) <- c("lat", "lon", "reserve_biomass")

presabs_fit <- left_join(species_occurrence, data_plot)
auc(roc(presabs_fit$reserve_biomass, as.factor(species_occurrence$pres)))


ggplot(data=data_plot, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = reserve_biomass)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0) 

ggplot(data=data_soil[grid_points,], aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = WP)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0)


# Diagnostic input files
# source(file.path(wd, "functions", "diag_climate.R"))
# tmin <- diag_climate(1:20000, climate_folder = 'D:/climate/ERA5-Land/castanea_format2', var = "tmin")
# tmax <- diag_climate(1:20000, climate_folder = 'D:/climate/ERA5-Land/castanea_format2', var = "tmax")
# tmean <- diag_climate(1:20000, climate_folder = 'D:/climate/ERA5-Land/castanea_format2', var = "tj")
# data_climate <- data.frame(data$lat[1:20000], data$lon[1:20000], tmin, tmean, tmax)
# names(data_climate) <- c("lat", "lon", "tmin", "tmean", "tmax")
# 
# tmin_map <- ggplot(data=data_climate, aes(x = lon, y = lat)) + 
#   geom_tile(aes(fill = tmin)) +
#   theme_void() +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   ylab("") +
#   xlab("") +
#   scale_fill_continuous_divergingx(palette = 'RdBu', rev=TRUE, mid = 0)
# 
# 
# tmean_map <- ggplot(data=data_climate, aes(x = lon, y = lat)) + 
#   geom_tile(aes(fill = tmean)) +
#   theme_void() +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   ylab("") +
#   xlab("") +
#   scale_fill_continuous_divergingx(palette = 'RdBu', rev=TRUE, mid = 0)
# 
# tmax_map <- ggplot(data=data_climate, aes(x = lon, y = lat)) + 
#   geom_tile(aes(fill = tmax)) +
#   theme_void() +
#   theme(legend.position="bottom") +
#   theme(legend.title=element_blank()) +
#   ylab("") +
#   xlab("") +
#   scale_fill_continuous_divergingx(palette = 'RdBu', rev=TRUE, mid = 0)



forward_sim_folder <- "D:/simulations/castanea/forward"

frostd <- read_mean_outputvalue(1:101510, lambda =1, file.path(forward_sim_folder, "quercus_ilex"), var = "GPP")


species_occurrence <- readRDS("C:/Users/vandermeersch/Documents/temp/q/quercus tgcc/occurrence_subset_1.rds")

presabs_fit <- left_join(species_occurrence, data_plot)
auc(roc(data_plot$reserve_biomass, as.factor(species_occurrence$pres)))



