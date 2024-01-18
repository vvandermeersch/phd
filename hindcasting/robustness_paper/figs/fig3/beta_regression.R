
#----------------------------------#
# Fig.2-a: Ordered Beta regression #
#----------------------------------#

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
                     name = "Model predictive performance") +
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
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.75), "cm")) +
  coord_cartesian(ylim=c(-0.0, 1), xlim = c(0.07, 0.34), clip = "off") +
  annotate("text", label=paste0('R^2*" = CI 95% ["*',betareg_r2$Q2.5,'*", "*',betareg_r2$Q97.5,'*"]"'), 
           parse=TRUE, x=0.13, y=0.95, family= "Helvetica Narrow", size = 2.7) +
  
  annotate("rect", xmin = 0.297, xmax = 0.34, ymin = 0.01, ymax = 1, alpha = .7, fill = "white") + # white shade
  
  annotate("rect", xmin = 0.27, xmax = 0.3, ymin = -0, ymax = -0.15, fill = "white") +
  
  annotate("point", x=0.13, y = -0.032, shape = 24, size = 2, col = "black", fill = "#6867ac") + # LateMiddle - Early Holocene
  
  # trick for ssp2 and ssp5 - 2030 (full ggplot solution!...)
  # annotate("point", x=0.24, y = 0.032, shape = 25, size = 2.4, col = "black", fill = "#f69320") +  # ssp2 - 2030
  # annotate("point", x=0.24345, y = 0.021, shape = 22, size = 3.3, col = "#bf1d1e", fill = "#bf1d1e") + # ssp5 - 2030
  # annotate("point", x=0.246, y = 0.027, shape = 24, size = 3.3, col = "white", fill = "white") + # white triangle to hide
  # annotate("point", x=0.24, y = 0.032, shape = 25, size = 2.4, col = "black", fill = NA) + # black frame
  
  annotate("point", x=0.279, y = -0.032, shape = 24, size = 2, col = "black", fill = "#f69320") +  # ssp2 - 2060
  annotate("text", label="SSP2", x=0.279, y = -0.087, size = 2.4, col = "#f69320", family= "Helvetica Narrow") +
  
  annotate("point", x=0.332, y = -0.032, shape = 24, size = 2, col = "black", fill = "#bf1d1e") + # ssp5 - 2060
  annotate("text", label="SSP5", x=0.332, y = -0.087, size = 2.3, col = "#bf1d1e", family= "Helvetica Narrow") 


