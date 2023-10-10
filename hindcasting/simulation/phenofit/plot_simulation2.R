library(tidyterra)
library(ggplot2)
library(cowplot)
library(extrafont)

extent <- c(-10,30,36,66) # extent use during migration simulation (to limit simulation runtime)

sim_dir <- "D:/simulations/phenofit/paleo/expert/025deg/quercus_ilex"

# best_threshold <- 0.036 #quercus_robur
# best_threshold <- 0.016 #quercus petraea
best_threshold <- 0.172 #quercus ilex

plots <- lapply(seq(16000,500, -1000), function(yr){
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

plots <- plot_grid(plotlist = plots, ncol = 4)

ggsave(filename= file.path(sim_dir, "simulation_overview.pdf"), 
       plot=plots , height=8, width=12)
