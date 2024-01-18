
#-------------------------#
# Ordered Beta regression #
#-------------------------#

ordbetareg_plot <- ggplot(data = expected_predictions, aes(y=Estimate.migsorensen,x=clim_hpv_sorensen.mean, col = type, fill = type)) +
  geom_pointrange(data = model_performance_mean, aes(y = mean_migsorensen, 
                                                       ymin=mean_migsorensen-sd_migsorensen, 
                                                       ymax=mean_migsorensen+sd_migsorensen), 
                  position = position_dodge(width = 0.005),
                  linewidth = 0.3, size = 0.2, stroke = 0.4) +
  geom_ribbon(aes(ymin=Q2.5.migsorensen,
                  ymax=Q97.5.migsorensen),
              alpha=0.3,
              linetype = "dashed", size = 0.4) +
  geom_line() +
  scale_x_continuous(breaks = seq(0.05,0.3,0.05),
                     expand = c(0,0),
                     name = "Climatic dissimilarity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,1,0.2),
                     name = "Performance") +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE), color = guide_legend(nrow=3,byrow=TRUE)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="bottom", legend.title=element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.y = unit(0.7, "cm"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(-0.0, 1), xlim = c(0.085, 0.285), clip = "on") +
  annotate("text", label=paste0('R^2*" = CI 95% ["*',betareg_r2$Q2.5,'*", "*',betareg_r2$Q97.5,'*"]"'), 
           parse=TRUE, x=0.12, y=0.95, family= "Helvetica Narrow", size = 2.7)


