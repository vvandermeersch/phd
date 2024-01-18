
figA9_main <- ggplot(data = model_performance_withmig, aes(x = clim_hpv_sorensen.mean, y = mig_sorensen , col = factor(mod))) +
  facet_wrap(~ species, scales = "free", labeller = as_labeller(genus_names)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "gam", se = FALSE) +
  scale_y_continuous(expand =c(0,0)) +
  scale_x_continuous(expand =c(0,0)) +
  labs(x = "Climatic dissimilarity", y = "Model performance") +
  scale_color_manual(breaks=c('BRT', 'Random Forest', 'GAM', 'GLM',
                              'PHENOFIT (fitted)', 'CASTANEA (fitted)', 'PHENOFIT', 'CASTANEA'),
                     values = c("#2e5168", "#3d6d8b", "#4d89af", "#6fa0c0",
                                "#794a66", "#ae7b9a", "#ea702e", "#f4b08a")) +
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
