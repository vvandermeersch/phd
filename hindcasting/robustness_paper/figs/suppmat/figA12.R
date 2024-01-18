
genus_names <- c(
  `abies` = "Abies",
  `fagus` = "Fagus",
  `quercusdeciduous` = "Quercus deciduous",
  `quercusevergreen` = "Quercus evergreen"
)

# Summarize model performances
model_performance_mean_perspeciespermodel <- model_performance_withmig %>%
  group_by(type, species, earlyholoc) %>%
  dplyr::summarise(mean_migsorensen=mean(mig_sorensen), sd_migsorensen=sd(mig_sorensen), 
                   mean_sorensen=mean(sorensen), sd_sorensen=sd(sorensen)) %>%
  as.data.frame()

xy_plot <- ggplot(data = model_performance_mean_perspeciespermodel, aes(x=mean_sorensen, y=mean_migsorensen,
                                                             shape = earlyholoc, linetype = earlyholoc, col = type)) + 
  facet_wrap(~ species, scales = "free", labeller = as_labeller(genus_names)) +
  geom_abline(intercept=0, slope=1, linetype = "dashed", color = "grey", linewidth = 0.3) +
  geom_pointrange(aes(ymin = mean_migsorensen - sd_migsorensen, ymax = mean_migsorensen + sd_migsorensen),
                  linewidth = 0.4, size = 0.5, stroke = 0.4) +
  geom_errorbarh(aes(xmax = mean_sorensen + sd_sorensen, xmin = mean_sorensen - sd_sorensen, height = 0),
                 linewidth = 0.4) + 
  coord_cartesian(ylim=c(0, 1), xlim = c(0, 1), clip = "off") +
  scale_x_continuous(breaks = c(0,0.5,1),
                     expand = c(0,0),
                     name = "Without migration") +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,0.5,1),
                     name = "With migration") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        legend.position="bottom", legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.y = unit(0.7, "cm"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  scale_shape_manual(breaks= c("FALSE", "TRUE"),
                     values= c(0, 16),
                     labels = c("Late-Middle Holocene", "Early Holocene")) +
  scale_linetype_manual(breaks= c("FALSE", "TRUE"),
                     values= c("solid", "dashed"),
                     labels = c("Late-Middle Holocene", "Early Holocene")) +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  guides(colour = "none")

# Summarize model performances
model_performance_mean_perspeciespermodel <- model_performance_withmig %>%
  group_by(type, species, clim_hpv_sorensen.mean = round(clim_hpv_sorensen.mean, 2)) %>%
  dplyr::summarise(mean_migsorensen=mean(mig_sorensen), sd_migsorensen=sd(mig_sorensen), 
                   mean_sorensen=mean(sorensen), sd_sorensen=sd(sorensen)) %>%
  as.data.frame()

delta_plot <- ggplot(data = model_performance_mean_perspeciespermodel, aes(x = clim_hpv_sorensen.mean, y = mean_sorensen, col = type)) + 
  facet_wrap(~ species, scales = "free", labeller = as_labeller(genus_names)) +
  geom_pointrange(aes(ymin = mean_sorensen - sd_sorensen, ymax = mean_sorensen + sd_sorensen),
                  linewidth = 0.3, size = 0.2, stroke = 0.4) +
  geom_line() + 
  geom_ribbon(aes(ymin = mean_sorensen, ymax = mean_migsorensen, fill = type), 
              alpha = 0.3, colour = NA) +
  coord_cartesian(xlim=c(0.085, 0.285),ylim=c(0, 1), clip = "off") +
  labs(x = "Climatic dissimilarity", y = "Performance") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        legend.position="bottom", legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0.7, "cm"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based"))


figA12_main <- plot_grid(delta_plot, xy_plot,
                         rel_widths = c(0.6,0.4),
                         labels = c("a", "b"), 
                         label_fontfamily = "Helvetica Narrow", label_size = 11)

