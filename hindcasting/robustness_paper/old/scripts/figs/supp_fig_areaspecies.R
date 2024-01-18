years <- c(500, 2000, 5000, 8000, 11000, 15000)


# Quercus
qboxplot_area <- ggplot(model_area[model_area$year %in% years & model_area$species == "quercus",], 
                         aes(y = area_occupied_km, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  facet_wrap(vars(year), nrow = 1)
  # scale_y_continuous(expand = expansion(mult = c(0, 0)),
  #                    breaks = seq(-0,0.8,0.2),
  #                    name = "TSS") +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank())




species_plot <- plot_grid(aboxplot_under, aboxplot_over,
                          fboxplot_under, fboxplot_over,
                          qboxplot_under, qboxplot_over,
                          ncol = 2,
                          align = "hv",
                          labels = c('A)', '', 'B)', '', 'C)'),
                          label_fontfamily = "Helvetica Narrow",
                          label_fontface = "plain",
                          label_size = 8.5)
supp_fig_species <- species_plot + annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dotted")), 
                                                     xmin = 0.53, xmax = 0.53, 
                                                     ymin = 0.03, ymax = 0.97)



