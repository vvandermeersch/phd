
#-----------------------------------------------------------------#
# Sorensen evolution across years, with linear-plateau regression #
#-----------------------------------------------------------------#

model_performance_median <- model_performance_withmig %>%
  group_by(type, year) %>%
  dplyr::summarise(median_migtss=median(mig_sorensen), sd_migtss=sd(mig_sorensen)) %>%
  as.data.frame()

figA13_main <- ggplot(model_performance_withmig, aes(x = year, y = mig_sorensen, color = type, fill = type)) +
  # geom_segment(aes(x = 12000-critpoint_csdm, y = -Inf,
  #                  xend = 12000-critpoint_csdm,
  #                  yend = critpoint_csdm_y ),
  #              size = 0.3, col = "black") +
  # geom_segment(aes(x = 12000-critpoint_csdm_confint[1], y = -Inf,
  #                  xend = 12000-critpoint_csdm_confint[1],
  #                  yend = 0.51),
  #              size = 0.1, linetype = "dashed", col = "darkgrey") +
  # geom_segment(aes(x = 12000-critpoint_csdm_confint[2], y = -Inf,
  #                xend = 12000-critpoint_csdm_confint[2],
  #                yend = critpoint_csdm_y),
  #            size = 0.1, linetype = "dashed", col = "darkgrey") +
  scale_x_continuous(breaks = seq(0,12000,2000),
                   expand = c(0.001,0.001),
                   name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Performance") +
  geom_pointrange(data = model_performance_median, aes(y = median_migtss, 
                                                       ymin=median_migtss-sd_migtss, 
                                                       ymax=median_migtss+sd_migtss), 
                  position = position_dodge(width = 150),
                  linewidth = 0.3, size = 0.2, stroke = 0.4) +
  geom_line(data=conf_intervals, aes(y = intconf.Sim.Median)) +
  geom_ribbon(data=conf_intervals, aes(ymin = intconf.Sim.2.5., ymax = intconf.Sim.97.5.), 
              alpha = 0.2, linetype = "dashed", size = 0.4)  + 
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE), color = guide_legend(nrow=1,byrow=TRUE)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9), 
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5),
        legend.position="none", legend.title=element_blank(),
        legend.key.size = unit(0.4, "cm"), legend.justification = "center",
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(0, 1), xlim = c(0, 12000), clip = "on")
