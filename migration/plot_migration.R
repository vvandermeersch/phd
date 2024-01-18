
library(tidyterra)
library(ggplot2)
library(cowplot)
library(extrafont)
library(terra)

extent <- c(-10,30,36,66) # extent use during migration simulation (to limit simulation runtime)
pollen_dir <- "D:/species/pollen/processed/fagus/025deg/001thr_500yrunc"
# add_pollen_dir <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"

# sim_dir <- "D:/simulations/castanea/paleo/migration/fitted/quercus_evergreen_expandLDD_from12k_scprb_2km20km_240ppm_fullmodel"
sim_dir <- "D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD_scprb_2km20km_fullmodel_from12000"
# sim_dir <- "D:/simulations/csdm/random_forest/paleo/migration/quercus_evergreen_expandLDD_from12k_scprb_2km20km_fullmodel"

plots <- lapply(seq(11500,500, -1000), function(yr){
  dist <- readRDS(file.path(sim_dir, paste0(yr, "BP.rds")))
  dist <- unwrap(dist)
  dist <- crop(dist, extent)
  dist <- as.data.frame(dist, xy = T)
  dist[,1:2] <- round(dist[,1:2], 2)
  
  pollen <- readRDS(file.path(pollen_dir, paste0("pres_", yr, "BP.rds")))
  
  if(!is.null(add_pollen_dir)){
    add_pollen <- readRDS(file.path(add_pollen_dir, paste0("pres_", yr, "BP.rds")))
    add_pollen  <- add_pollen %>% dplyr::filter(likely_only_deciduous == 0) # evergreen only
    pollen <- dplyr::full_join(pollen, add_pollen, by = c("lat", "lon"))
    pollen$pres <- rowSums(pollen[, c("pres.x", "pres.y")], na.rm = T)
    pollen$pres <- ifelse(pollen$pres > 0, 1, 0)
  }
  
  pollen <- pollen[pollen$lon <= extent[2] & pollen$lat <= extent[4],]
  
  names(dist) <- c("x", "y", "output")
  dist$output <- as.factor(dist$output)
  plot <- ggplot() +
    geom_raster(data = dist, aes(x = x, y = y, fill = output)) +
    geom_point(data = pollen[pollen$pres == 1,], aes(x = lon, y = lat), size = 0.2) +
    theme_void() +
    scale_fill_manual(values = c("0" = "#ededed", "1" = "#F2B880", "2" = "#C98686"), na.value = "white") +
    annotate("text", x = -5, y = 64, label = paste0(yr, "BP"), color = "#b15151",
             family= "Helvetica", size = 3) +
    theme(legend.position = "none")
  
  return(plot)
  
})
  
plots <- cowplot::plot_grid(plotlist = plots, ncol = 4)

ggsave(filename= file.path(sim_dir, "migration_overview.pdf"), 
       plot=plots , height=10, width=14)
