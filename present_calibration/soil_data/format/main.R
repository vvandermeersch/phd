library(raster)
library(dplyr)
library(tidyr)
library(data.table)

rawdata_folder <- "D:/soil"
out_folder <- "D:/soil/castanea_format"

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/soil_data/format"
source(file.path(wd, "functions", "phenofit_get_WHC.R"))
source(file.path(wd, "functions", "get_comments.R"))

# get ERA5_Land grid (0.1°)
ERA5land_grid <- raster(paste0("D:/climate/ERA5-Land/raw/","2m_dewpoint_temperature_1969_01.nc"))

# 7 soil depths : 0, 5, 15, 30, 60, 100, and 200 cm depth
dep <- c(0, 5, 15, 30, 60, 100, 200)
# 6 layers ?
thick <- c(5, 10, 15, 30, 40, 100)

# Folders
EU_SHD_folder <- paste0(rawdata_folder, "/EU_SoilHydroGrids_1km/raw")
SoilGrids_folder <- paste0(rawdata_folder, "/SoilGrids250m")
SoilGrids_v2_folder <- paste0(rawdata_folder, "/SoilGrids250m_v2")

# Load field capacity data
filenames <- list.files(EU_SHD_folder, pattern="^FC", full.names=TRUE)
fc_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load wilting point data
filenames <- list.files(EU_SHD_folder, pattern="^WP", full.names=TRUE)
wp_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load depth to bedrock (R horizon, cm)
depth <- raster(paste0(SoilGrids_folder, "/BDRICM_M_250m_ll.tif")) %>% crop(ERA5land_grid)

# Load weight percentage of the silt particles
filenames <- list.files(SoilGrids_folder, pattern="^SLTPPT", full.names=TRUE)
slt_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load weight percentage of the clay particles
filenames <- list.files(SoilGrids_folder, pattern="^CLYPPT", full.names=TRUE)
cly_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load weight percentage of the sand particles
filenames <- list.files(SoilGrids_folder, pattern="^SNDPPT", full.names=TRUE)
snd_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load volumetric percentage of coarse fragments (>2 mm)
filenames <- list.files(SoilGrids_folder, pattern="^CRFVOL", full.names=TRUE)
crf_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load bulk density (fine earth)
filenames <- list.files(SoilGrids_folder, pattern="^BLDFIE", full.names=TRUE)
bld_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load nitrogen (SoilGrids v2 : already in depth intervals)
filenames <- list.files(SoilGrids_v2_folder, pattern="nitrogen", full.names=TRUE)
nitr_stack <- raster::stack(filenames) %>% crop(ERA5land_grid) # crop not needed ?

# Load pH (SoilGrids v2 : already in depth intervals)
filenames <- list.files(SoilGrids_v2_folder, pattern="ph", full.names=TRUE)
ph_stack <- raster::stack(filenames) %>% crop(ERA5land_grid) # crop not needed ?

# Load organic carbon stocks (SoilGrids v2 : already in depth intervals)
filenames <- list.files(SoilGrids_v2_folder, pattern="ocs", full.names=TRUE)
ocs_stack <- raster::stack(filenames) %>% crop(ERA5land_grid) # crop not needed ?

# Change resolution, from 250m/1km to 0.1deg
fc_stack <- resample(fc_stack, ERA5land_grid, method="bilinear")
wp_stack <- resample(wp_stack, ERA5land_grid, method="bilinear")
depth <- resample(depth, ERA5land_grid, method="bilinear")
slt_stack <- resample(slt_stack, ERA5land_grid, method="bilinear")
cly_stack <- resample(cly_stack, ERA5land_grid, method="bilinear")
snd_stack <- resample(snd_stack, ERA5land_grid, method="bilinear")
crf_stack <- resample(crf_stack, ERA5land_grid, method="bilinear")
bld_stack <- resample(bld_stack, ERA5land_grid, method="bilinear")
nitr_stack <- resample(nitr_stack, ERA5land_grid, method="bilinear")
ph_stack <- resample(ph_stack, ERA5land_grid, method="bilinear")
ocs_stack <- resample(ocs_stack, ERA5land_grid, method="bilinear")

# from one depth layer to layered weights
depth_weights <- depth
depth_weights[!is.na(depth_weights)] <- thick[1]
names(depth_weights) <- "layer1"
for (i in 2:6){
  r <- depth
  r[] <- NA
  r[depth>=dep[(i+1)]] <- thick[i]
  r[depth<dep[(i+1)]] <- depth[depth<dep[(i+1)]] - dep[i]
  r[r<0] <- 0
  names(r) <- paste0("layer", i)
  depth_weights <- stack(depth_weights, r)
}

# 7 depths => 6 layer values (mean two by two), only for SoilGrids v1 (2017)
fc_layer <- mean(fc_stack[[1]],fc_stack[[2]], na.rm=T)
names(fc_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(fc_stack[[i-1]],fc_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  fc_layer <-stack(fc_layer,r)
}
wp_layer <- mean(wp_stack[[1]],wp_stack[[2]], na.rm=T)
names(wp_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(wp_stack[[i-1]],wp_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  wp_layer <-stack(wp_layer,r)
}
bld_layer <- mean(bld_stack[[1]],bld_stack[[2]], na.rm=T)
names(bld_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(bld_stack[[i-1]],bld_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  bld_layer <-stack(bld_layer,r)
}
crf_layer <- mean(crf_stack[[1]],crf_stack[[2]], na.rm=T)
names(crf_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(crf_stack[[i-1]], crf_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  crf_layer <-stack(crf_layer,r)
}
cly_layer <- mean(cly_stack[[1]],cly_stack[[2]], na.rm=T)
names(cly_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(cly_stack[[i-1]], cly_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  cly_layer <-stack(cly_layer,r)
}
snd_layer <- mean(snd_stack[[1]],snd_stack[[2]], na.rm=T)
names(snd_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(snd_stack[[i-1]], snd_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  snd_layer <-stack(snd_layer,r)
}
fin_stack <- slt_stack+cly_stack # fine particles
fin_layer <- mean(fin_stack[[1]],fin_stack[[2]], na.rm=T)
names(fin_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(fin_stack[[i-1]], fin_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  fin_layer <-stack(fin_layer,r)
}


# Castanea need only mean value (weighted by each layer thickness), divide by 100 (percentage to proportion)
fc_mean <- weighted.mean(fc_layer, w = depth_weights)/100
wp_mean <- weighted.mean(wp_layer, w = depth_weights)/100
crf_mean <- weighted.mean(crf_layer, w = depth_weights)/100
cly_top <- weighted.mean(subset(cly_layer, 1:3), w = subset(depth_weights, 1:3))/100 # first 30cm (top soil)
cly_sol <- weighted.mean(cly_layer, w = depth_weights)/100
snd_top <- weighted.mean(subset(snd_layer, 1:3), w = subset(depth_weights, 1:3))/100 # first 30cm (top soil)
snd_sol <- weighted.mean(snd_layer, w = depth_weights)/100
fin_top <- weighted.mean(subset(fin_layer, 1:3), w = subset(depth_weights, 1:3))/100 # first 30cm (top soil)
fin_sol <- weighted.mean(fin_layer, w = depth_weights)/100

bld_mean <- weighted.mean(bld_layer, w = depth_weights)/1000 #divide by 1000, kg/m3 to g/cm3
nitr_mean <-  weighted.mean(nitr_stack, w = depth_weights) / 100 #cg/kg to g/kg (conventional unit) (warning : depth from SoilGrids v1, nitr from v2...)
ph_mean <- weighted.mean(ph_stack, w = depth_weights) / 10 #ph*10 to ph (conventional unit) (warning : depth from SoilGrids v1, pH from v2...)
ocs_mean <- ocs_stack / 10 # t/ha to kg/m2 (conventional unit)

# Phenofit need a Water Holding Capacity (mm)
dif <- (fc_layer - wp_layer)/100 
whc <- sum(dif*depth_weights)*10

# Get non NA values from ERA5
ERA5land_df <- as.data.frame(ERA5land_grid, xy=T) %>% na.omit
names(ERA5land_df)[3] <- "value"
ERA5land_df$x <- round(ERA5land_df$x,2)
ERA5land_df$y <- round(ERA5land_df$y,2)




# Keep only soil values also in ERA5
fc_mean_df <- as.data.frame(fc_mean, xy=T)
names(fc_mean_df) <- c("x", "y", "FC")
fc_mean_df$x <- round(fc_mean_df$x,2)
fc_mean_df$y <- round(fc_mean_df$y,2)
wp_mean_df <- as.data.frame(wp_mean, xy=F)
names(wp_mean_df) <- c("WP")
depth_df <- as.data.frame(depth, xy=F)
names(depth_df) <- c("depth")
bld_mean_df <- as.data.frame(bld_mean, xy=F)
names(bld_mean_df) <- c("bld")
nitr_mean_df <- as.data.frame(nitr_mean, xy=F)
names(nitr_mean_df) <- c("nitrogen")
ph_mean_df <- as.data.frame(ph_mean, xy=F)
names(ph_mean_df) <- c("pH")
ocs_mean_df <- as.data.frame(ocs_mean, xy=F)
names(ocs_mean_df) <- c("carbon")
whc_df <- as.data.frame(whc, xy=F)
names(whc_df) <- c("WHC")
snd_top_df <- as.data.frame(snd_top, xy=F)
names(snd_top_df) <- c("sand_top")
snd_sol_df <- as.data.frame(snd_sol, xy=F)
names(snd_sol_df) <- c("sand_all")
cly_top_df <- as.data.frame(cly_top, xy=F)
names(cly_top_df) <- c("cly_top")
cly_sol_df <- as.data.frame(cly_sol, xy=F)
names(cly_sol_df) <- c("cly_all")
fin_top_df <- as.data.frame(fin_top, xy=F)
names(fin_top_df) <- c("fin_top")
fin_sol_df <- as.data.frame(fin_sol, xy=F)
names(fin_sol_df) <- c("fin_all")
crf_sol_df <- as.data.frame(crf_mean, xy=F)
names(crf_sol_df) <- c("crf_all")
data_soil <- cbind(fc_mean_df, wp_mean_df, depth_df, whc_df, bld_mean_df, nitr_mean_df, ocs_mean_df, ph_mean_df,
                   crf_sol_df, cly_top_df, cly_sol_df, fin_top_df, fin_sol_df, snd_top_df, snd_sol_df) %>% 
  left_join(ERA5land_df) %>% 
  drop_na(value) %>% 
  dplyr::select(-c("value"))
names(data_soil)[1:2] <- c("lon", "lat")

data_soil <- data_soil %>% drop_na(c("FC","WP","depth","WHC","bld","crf_all","cly_top","cly_all",
                                     "fin_top","fin_all","sand_top","sand_all"))
# some point are missing for pH and nitrogen (a few dozen) // ERA5Land points
# we need to interpolate them
library(zoo)
data_soil <- as.data.frame(na.approx(data_soil)) # approximation based on before/after values
save(data_soil, file="D:/soil/processed/data_soil.Rdata")

# Create WHC file for Phenofit
# phenofit_get_WHC(data_soil, "D:/climate/ERA5-Land/phenofit_format/transformed/")




