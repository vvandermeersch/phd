
# categorical
m <- c(0, 1.6, 1,
       1.6, 1.8, 2,
       1.8, 100, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)


plots <- lapply(seq(2040, 2090,10), function(yr){
  climdist <- readRDS(file.path(future_novelty_dir, paste0("climdist_", yr, "BP.rds")))
  climdist_cat <- as.data.frame(classify(climdist, rclmat, right=NA), xy = T)
  climdist_cat$mahalanobis_distance <- as.factor(climdist_cat$mahalanobis_distance)
  levels(climdist_cat$mahalanobis_distance) <- c("Correlative", "Process", "Unknown")
  
  ggplot(data = climdist_cat, aes(x = x, y = y, fill = mahalanobis_distance)) +
    geom_raster(alpha = 0.9) +
    scale_fill_manual(values = c("#e86117", "#457b9d", "#D5DCE7")) +
    theme_void() +
    theme(legend.position="bottom", legend.title=element_blank(),
          legend.text = element_text(colour = "black", family= "Noto Sans", size = 7)) +
    annotate("text", x = -5, y = 71.1, label = paste0(yr, " BP"), family = "Noto Sans") 
  
})
future_figure <- plot_grid(plotlist = lapply(plots, function(x) x + theme(legend.position = 'none')))
future_figure_cat <- plot_grid(future_figure, 
                           get_legend( plots[[1]]),
                           ncol = 1,
                           rel_heights = c(1,0.1))



# continuous
plots <- lapply(c(2030, 2050, 2070, 2090), function(yr){
  climdist <- as.data.frame(readRDS(file.path(future_novelty_dir, paste0("climdist_", yr, "BP.rds"))), xy = T)
  climdist[climdist$mahalanobis_distance >2.3, "mahalanobis_distance"] <- 2.3
  
  ggplot(data = climdist, aes(x = x, y = y, fill = mahalanobis_distance)) +
    geom_raster(alpha = 0.9) +
    scale_fill_gradientn(
      name = "Model confidence zone",
      limits = c(0.9,2.3),
      colours = c("#e86117", "#457b9d", "grey"), 
      values = scales::rescale(c(0.9, 1.65, 2.35)),
      breaks = c(1, 1.65, 2.3),
      labels = c("Correlative", "Process", "Unknown")) +
    theme_void() +
    theme(legend.position="bottom",
          legend.text = element_text(colour = "black", family= "Noto Sans Cond", size = 8),
          legend.title = element_text(colour = "black", family= "Noto Sans Cond", size = 8),
          legend.title.align = 0.5, 
          legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(1, 'cm'),
          plot.margin = unit(c(0,0.3,0,0.3), "cm")) +
    annotate("text", x = 12, y = 71.1, label = paste0(yr, " BP"), family = "Noto Sans Cond", size = 4) +
    guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                                 ticks = FALSE))
  
})
future_figure <- plot_grid(plotlist = lapply(plots, function(x) x + theme(legend.position = 'none')), nrow = 1)
future_figure_cont <- plot_grid(future_figure, 
                               get_legend( plots[[1]]),
                               ncol = 1,
                               rel_heights = c(1,0.18))





