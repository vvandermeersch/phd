
library(tidyterra)
library(ggplot2)
library(cowplot)
library(extrafont)
library(terra)

extent <- c(-10,30,36,66) # extent use during migration simulation (to limit simulation runtime)
pollen_dir <- "D:/species/pollen/processed/quercus/025deg/0025thr_500yrunc"

sim_dir <- "D:/simulations/castanea/paleo/migration/fitted/quercus_deciduous_expandLDD_from15k_scprb_2km20km_190ppm"
plots <- lapply(seq(15000,500, -1000), function(yr){
  dist <- readRDS(file.path(sim_dir, paste0(yr, "BP.rds")))
  dist <- unwrap(dist)
  dist <- crop(dist, extent)
  dist <- as.data.frame(dist, xy = T)
  dist[,1:2] <- round(dist[,1:2], 2)
  
  pollen <- readRDS(file.path(pollen_dir, paste0("pres_", yr, "BP.rds")))
  pollen <- pollen[pollen$lon <= extent[2] & pollen$lat <= extent[4],]
  
  dist$output_raster <- as.factor(dist$output_raster)
  plot <- ggplot() +
    geom_raster(data = dist, aes(x = x, y = y, fill = output_raster)) +
    geom_point(data = pollen[pollen$pres == 1,], aes(x = lon, y = lat), size = 0.2) +
    theme_void() +
    scale_fill_manual(values = c("0" = "#FFF4EC", "1" = "#F2B880", "2" = "#C98686"), na.value = "white") +
    annotate("text", x = -5, y = 64, label = paste0(yr, "BP"), color = "#b15151",
             family= "Noto Sans Cond", size = 3) +
    theme(legend.position = "none")
  
  return(plot)
  
})
  
plots <- cowplot::plot_grid(plotlist = plots, ncol = 5)

ggsave(filename= file.path(sim_dir, "migration_overview.pdf"), 
       plot=plots , height=9.6, width=16)
