
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
ext <- ext(c(-14,40,34,72))


# setup
output_folder <- "D:/simulations/csdm/random_forest/paleo"
species <- "fagus_sylvatica"


# loop on years

for(year in c(seq(21000,1000, -2000), 15)){
  
  fitness <- readRDS(file.path(sim_dir, species, paste0(year, "BP.rds")))
  
  ice_sheet <- load_icesheet(year, folder = hadcm3b_folder, sea_ice = F, extent = ext)
  ice_sheet[ice_sheet$ice > 1, "ice"] <- 1
  ice_sheet[ice_sheet$ice < 0.05, ] <- NA
  
  # altitude <- load_altitude(year, folder = hadcm3b_folder, extent = ext, dscale = T)
  
  plot <- ggplot() +
    geom_raster(data = fitness, aes(fill = pred, x = lon, y = lat)) +
    theme_void() +
    ylab("") +
    xlab("") +
    scale_fill_gradientn(colours = c("#ff9ea0", "#f8961e", "#f9c74f", "#43aa8b", "#577590"), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1)) +
    guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                                 ticks = FALSE)) +
    theme(legend.title=element_blank(), legend.position = "none", 
          legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.5, 'cm')) +
    new_scale_fill() + 
    geom_raster(data = ice_sheet, aes(fill = ice, x = lon, y = lat), alpha = 1) +
    scale_fill_gradient2(low = "#e9e8ef", mid = "#c9ccde", high = "#a6abd0", limits = c(0,1.1), breaks = c(0,0.5,1), midpoint = 0.5) +
    geom_rect(aes(xmin = -13.3, xmax = -6.8, ymin = 71, ymax = 72.9), 
              fill = "grey", alpha = 0.4, color = "black") +
    annotate("text", x = -10, y = 72, label = paste0(year, " BP")) 
    
  
  ggsave(plot = plot, 
         filename = file.path(wd, paste0("fitnessplot_", year, "BP.png")), 
         device = "png", width = 7.5, height = 7, bg = "white")
}


