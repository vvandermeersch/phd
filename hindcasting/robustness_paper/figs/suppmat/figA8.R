
genus_names <- c(
  `abies` = "Abies",
  `fagus` = "Fagus",
  `quercusdeciduous` = "Quercus deciduous",
  `quercusevergreen` = "Quercus evergreen"
)

# Summarize model performances (for the seek of clarity in the figure A8)
model_performance_mean_perspeciespermodel <- model_performance_withmig %>%
  group_by(type, species, clim_hpv_sorensen.mean = round(clim_hpv_sorensen.mean, 2)) %>%
  dplyr::summarise(mean_migsorensen=mean(mig_sorensen), sd_migsorensen=sd(mig_sorensen)) %>%
  as.data.frame()

figA8_main <- ggplot(data = model_performance_withmig, aes(x = clim_hpv_sorensen.mean, y = mig_sorensen , col = factor(type))) +
  facet_wrap(~ species, scales = "free", labeller = as_labeller(genus_names)) +
  geom_pointrange(data = model_performance_mean_perspeciespermodel, aes(y = mean_migsorensen, 
                                                     ymin=mean_migsorensen-sd_migsorensen, 
                                                     ymax=mean_migsorensen+sd_migsorensen), 
                  position = position_dodge(width = 0.005),
                  linewidth = 0.3, size = 0.2, stroke = 0.4) +
  geom_smooth(method = "gam", se = FALSE) +
  scale_y_continuous(expand =c(0,0)) +
  scale_x_continuous(expand =c(0,0)) +
  labs(x = "Climatic dissimilarity", y = "Model performance") +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#e86117"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="bottom", legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9)) +
  coord_cartesian(ylim=c(-0.03, 1), xlim = c(0.085, 0.285), clip = "on")
