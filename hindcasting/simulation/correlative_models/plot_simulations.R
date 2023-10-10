
###################################
#                                 #
# PLOT PHENOFIT PALEO SIMULATIONS #
#                                 #
###################################

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/correlative_models/random_forest"
library(ggplot2)
library(ggnewscale)
library(terra)
library(raster)

hadcm3b_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
pollen_folder <- "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc"
ext <- ext(c(-14,40,34,72))


# setup
sim_dir <- "D:/simulations/csdm/lasso_glm/paleo/025deg"
species <- "fagus_sylvatica"
mod <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/fagus_sylvatica/lasso_glm_finalcov_fullmodel.rds")



# loop on years

for(year in seq(500,16500,500)){
  
  fitness <- readRDS(file.path(sim_dir, species, paste0(year, "BP.rds")))
  
  pollen <- data.frame(lon = 0, lat= 0, pres = NA)
  if(year%%500 ==0){pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))}
  
  # ice sheet
  yr_ICE6G <- year/1000
  extent <- ext(c(-10,35,36,71))
  ice_sheet <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_sheet[ice_sheet == 0] <- NA
  ice_sheet <- resample(ice_sheet, rast(fitness[c(2,1,3)]))
  ice_sheet <- as.data.frame(ice_sheet, xy=T)
  
  plot <- ggplot() +
    geom_raster(data = fitness, aes(fill = pred, x = lon, y = lat)) +
    geom_point(data = pollen[pollen$pres == 1,], aes(x = lon, y = lat), col = "white", fill = "#009B72", shape = 21, size = 1) +
    geom_point(data = pollen[pollen$pres == 0,], aes(x = lon, y = lat), col = "white", shape = 4, size = 0.5) +
    theme_void() +
    ylab("") +
    xlab("") +
    scale_fill_gradientn(colours = c("#ff9ea0", "#f8961e", "#f9c74f", "#43aa8b", "#577590"), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1)) +
    guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                                 ticks = FALSE)) +
    theme(legend.title=element_blank(), legend.position = "none", 
          legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.5, 'cm')) +
    new_scale_fill() + 
    geom_raster(data = ice_sheet, aes(fill = sftgif, x = x, y = y), alpha = 0.7) +
    scale_fill_gradient2(low = "#e9e8ef", mid = "#c9ccde", high = "#a6abd0", limits = c(0,100), breaks = c(0,50,100), midpoint = 50) +
    geom_rect(aes(xmin = -13.3, xmax = -6.8, ymin = 71, ymax = 72.9), 
              fill = "grey", alpha = 0.4, color = "black") +
    annotate("text", x = -10, y = 72, label = paste0(year, " BP")) 
    
  
  ggsave(plot = plot, 
         filename = file.path(sim_dir, paste0("fitnessplot_", year, "BP.png")), 
         device = "png", width = 7.5, height = 7, bg = "white")
  
  # best threshold
  ths <- mod$best_threshold
  
  fitness$bin_pred <- 0
  fitness[fitness$pred >= ths, "bin_pred"] <- 1
  plot <- ggplot() +
    geom_raster(data = fitness, aes(fill = bin_pred, x = lon, y = lat)) +
    geom_point(data = pollen[pollen$pres == 1,], aes(x = lon, y = lat), col = "white", fill = "#009B72", shape = 21, size = 1) +
    geom_point(data = pollen[pollen$pres == 0,], aes(x = lon, y = lat), col = "white", shape = 4, size = 0.5) +
    theme_void() +
    ylab("") +
    xlab("") +
    scale_fill_gradientn(colours = c("#ff9ea0", "#f8961e", "#f9c74f", "#43aa8b", "#577590"), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1)) +
    guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                                 ticks = FALSE)) +
    theme(legend.title=element_blank(), legend.position = "none", 
          legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.5, 'cm')) +
    new_scale_fill() + 
    geom_raster(data = ice_sheet, aes(fill = sftgif, x = x, y = y), alpha = 0.7) +
    scale_fill_gradient2(low = "#e9e8ef", mid = "#c9ccde", high = "#a6abd0", limits = c(0,100), breaks = c(0,50,100), midpoint = 50) +
    geom_rect(aes(xmin = -13.3, xmax = -6.8, ymin = 71, ymax = 72.9), 
              fill = "grey", alpha = 0.4, color = "black") +
    annotate("text", x = -10, y = 72, label = paste0(year, " BP")) 
  
  
  ggsave(plot = plot, 
         filename = file.path(sim_dir, paste0("presplot_", year, "BP.png")), 
         device = "png", width = 7.5, height = 7, bg = "white")
}



extent <- ext(c(-10,20,36,71))
for(year in c(10500, 12000, 13000, 14500, 15000)){
  
  fitness <- readRDS(file.path(sim_dir, species, paste0(year, "BP.rds")))
  fitness <- crop(rast(fitness[c(2,1,3)]), extent)
  
  # ice sheet
  yr_ICE6G <- year/1000
  ice_sheet <- rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif"))
  ice_sheet[ice_sheet == 0] <- NA
  ice_sheet <- crop(resample(ice_sheet, fitness), extent)
  
  ice_sheet <- as.data.frame(ice_sheet, xy=T)
  fitness <- as.data.frame(fitness, xy=T)
  
  # altitude <- load_altitude(year, folder = hadcm3b_folder, extent = ext, dscale = T)
  
  plot <- ggplot() +
    geom_raster(data = fitness, aes(fill = pred, x = x, y = y)) +
    theme_void() +
    ylab("") +
    xlab("") +
    scale_fill_gradientn(colours = c("#ff9ea0", "#f8961e", "#f9c74f", "#43aa8b", "#577590"), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1)) +
    guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                                 ticks = FALSE)) +
    theme(legend.title=element_blank(), legend.position = "none", 
          legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.5, 'cm')) +
    new_scale_fill() + 
    geom_raster(data = ice_sheet, aes(fill = sftgif, x = x, y = y), alpha = 0.7) +
    scale_fill_gradient2(low = "#e9e8ef", mid = "#c9ccde", high = "#a6abd0", limits = c(0,100), breaks = c(0,50,100), midpoint = 50)
  
  ggsave(plot = plot, 
         filename = file.path(wd, paste0("fitnessplot_zoom_", year, "BP.png")), 
         device = "png", width = 6, height = 7, bg = "white")
  
}
