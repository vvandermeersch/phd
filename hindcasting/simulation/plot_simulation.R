library(tidyterra)
library(ggplot2)
library(cowplot)
library(extrafont)
library(terra)

extent <- c(-10,30,36,66) # extent use during migration simulation (to limit simulation runtime)

sim_dir <- "D:/simulations/castanea/paleo/expert/025deg/quercus_robur/32yr_inventory_modcode/NPP"

# best_threshold <- 0.036 #quercus_robur PHENOFIT
# best_threshold <- 0.016 #quercus petraea PHENOFIT
# best_threshold <- 0.002 # quercus pubescens PHENOFIT


# best_threshold <- 0.518 # quercus robur RF
# best_threshold <- 0.612 # quercus petraea RF

# best_threshold <- 0.604 # quercus ilex (evergreen) PHENOFIT fitted
#best_threshold <- cmaes_fit_subset1_rep1[["best_threshold"]] # q robur subset1_rep1 
# best_threshold <- 0.728

best_threshold <- 528.2 # castanea, q. robur 280 ppm

plots <- lapply(seq(12000,1000, -1000), function(yr){
  dist <- readRDS(file.path(sim_dir, paste0(yr, "BP.rds")))
  dist <- rast(dist[c(2,1,3)])
  dist <- crop(dist, extent)
  
  dist <- ifel(dist < best_threshold, 0, 1)
  
  values(dist) <- as.factor(values(dist))
  plot <- ggplot() +
    geom_spatraster(data = dist) +
    theme_void() +
    scale_fill_manual(values = c("0" = "#FFF4EC", "1" = "#F2B880", "2" = "#C98686"), na.value = "white") +
    annotate("text", x = -5, y = 64, label = paste0(yr, "BP"), color = "#b15151",
             family= "Noto Sans Cond", size = 3) +
    theme(legend.position = "none")
  
  return(plot)
  
})

plots <- lapply(seq(16000,1000, -1000), function(yr){
  dist <- readRDS(file.path(sim_dir, paste0(yr, "BP.rds")))
  dist <- rast(dist[c(2,1,3)])
  dist <- crop(dist, extent)
  
  plot <- ggplot() +
    geom_spatraster(data = dist) +
    theme_void() +
    scale_fill_gradientn(colours = c("#ff9ea0", "#f8961e", "#f9c74f", "#43aa8b", "#577590"), breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1),
                         na.value = "white") +
    guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                                 ticks = FALSE)) +
    annotate("text", x = -5, y = 64, label = paste0(yr, "BP"), color = "#b15151",
             family= "Noto Sans Cond", size = 3) +
    theme(legend.position = "none")
  
  return(plot)
  
})

plots <- plot_grid(plotlist = plots, ncol = 4)

ggsave(filename= file.path(sim_dir, "simulation_overview.pdf"), 
       plot=plots , height=9, width=12)
