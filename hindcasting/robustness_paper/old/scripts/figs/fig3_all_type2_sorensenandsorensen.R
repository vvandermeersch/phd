#----------------------------------------------#
# Figure 3: TSS with linear-plateau regression #
#----------------------------------------------#

model_performance_median <- model_performance_withmig %>%
  group_by(type2, hypervolume_sorensen) %>%
  dplyr::summarise(median_migtss=median(mig_sorensen), sd_migtss=sd(mig_sorensen)) %>%
  as.data.frame()

tss_performance <- ggplot(model_performance_withmig, aes(x = 1-hypervolume_sorensen, y = mig_sorensen, color = type2, fill = type2)) +
  scale_x_continuous(breaks = seq(0,0.3,0.05),
                     expand = c(0.001,0.001),
                     name = "CLIMATE NOVELTY") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,1,0.2),
                     name = "PREDICTIVE PERFORMANCE") +
  geom_pointrange(data = model_performance_median, aes(y = median_migtss, 
                                                       ymin=median_migtss-sd_migtss, 
                                                       ymax=median_migtss+sd_migtss), 
                  position = position_dodge(width = 0.015),
                  linewidth = 0.3, size = 0.1, stroke = 0.4) +
  geom_line(data=conf_intervals[conf_intervals$type2 != "1Correlative",], aes(y = intconf.Sim.Median)) +
  geom_ribbon(data=conf_intervals[conf_intervals$type2 != "1Correlative",], aes(ymin = intconf.Sim.2.5., ymax = intconf.Sim.97.5.), 
              alpha = 0.2, linetype = "dashed", size = 0.4)  + 
  # geom_segment(aes(x = trunc((2-critpoint_phenofit)*100)/100, y = -Inf, 
  #                  xend = trunc((2-critpoint_phenofit)*100)/100, 
  #                  yend = critpoint_phenofit_y+0.04), 
  #              size = 0.8, linetype = "solid", col = "#457b9d") +
  # geom_segment(aes(x = trunc((2-critpoint_csdm)*100)/100, y = -Inf, 
  #                  xend = trunc((2-critpoint_csdm)*100)/100, 
  #                  yend = critpoint_csdm_y ), 
  #              size = 0.8, linetype = "dashed", col = "#e86117") +
  
  # geom_segment(aes(x = 2-inter_coords[1], y = -Inf, 
  #                  xend = 2-inter_coords[1], 
  #                  yend = inter_coords[2]), 
  #              size = 0.8, linetype = "dashed", col = "#457b9d") +
  annotate("rect", xmin=round(max(model_performance_withmig$median),1),xmax=round(max(model_performance_withmig$median),0), 
         ymin=-Inf, ymax=Inf, alpha=0.7, fill="white") +
  geom_segment(aes(x = round(max(model_performance_withmig$median),1), y = -Inf, 
                   xend = round(max(model_performance_withmig$median),1), 
                   yend = 0.44), 
               size = 0.5, linetype = "dashed", col = "darkgrey") +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81"),
                     labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81"),
                    labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  theme_bw() + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE), color = guide_legend(nrow=2,byrow=TRUE)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 8, hjust = 0.1, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5, margin = margin(r = -10)),
        legend.position="bottom", legend.title=element_blank(),
        legend.key.size = unit(0.4, "cm"), legend.justification = "right",
        legend.spacing.x = unit(0.05, "cm"),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(0, 1.65), xlim = c(0.05, 0.3), clip = "on")



model_performance_withmig$crit <- model_performance_withmig$median<(2-critpoint_rbcsdm)

model_performance_withmig[model_performance_withmig$type2=="Treebased", "type2"] <- "1Treebased"
model_performance_withmig[model_performance_withmig$type2=="Regressionbased", "type2"] <- "2Regressionbased"
model_performance_withmig[model_performance_withmig$type2=="Expertprocessbased", "type2"] <- "3Expertprocessbased"
model_performance_withmig[model_performance_withmig$type2=="Fittedprocessbased", "type2"] <- "4Fittedprocessbased"

# lower than critical point
conoverTest_out <- conover.test::conover.test(x=model_performance_withmig[model_performance_withmig$crit == TRUE,]$sorensen, 
                                              g=model_performance_withmig[model_performance_withmig$crit == TRUE,]$type2, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters <- data.frame(letters)
# data_letters[data_letters$Group == "PHENOFIT(fitted)", "Group"] <- "PHENOFIT (fitted)"

boxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE,], aes(x = type2, y = sorensen, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,1,0.2),
                     name = "Sørensen index") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.97), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.5) +
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
        axis.ticks.x = element_blank(), axis.line.x = element_blank()) +
  coord_cartesian(ylim=c(0, 1), clip = "off")

# higher than critical point
conoverTest_out <- conover.test::conover.test(x=model_performance_withmig[model_performance_withmig$crit == FALSE,]$sorensen, 
                                              g=model_performance_withmig[model_performance_withmig$crit == FALSE,]$type2, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters <- data.frame(letters)
# data_letters[data_letters$Group == "PHENOFIT(fitted)", "Group"] <- "PHENOFIT (fitted)"

boxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE,], aes(x = type2, y = sorensen, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,1,0.2),
                     name = "Sørensen index ") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.97), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.5) +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(),
        plot.background = element_blank()) +
  coord_cartesian(ylim=c(0, 1), clip = "off")

library(grid)


tss_performance2 <- tss_performance +
  annotation_custom(ggplotGrob(boxplot_over), xmin = trunc((2-critpoint_rbcsdm)*100)/100, xmax = 1.66, 
                    ymin = 0.99, ymax = 1.6) +
  annotation_custom(ggplotGrob(boxplot_under), xmin = 0.98, xmax = trunc((2-critpoint_rbcsdm)*100)/100+0.005, 
                    ymin = 0.99, ymax = 1.6) +
  
  annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dashed")), 
                    xmin = (2-critpoint_rbcsdm), xmax = (2-critpoint_rbcsdm), 
                    ymin = 1.05, ymax = 1.5) +
  
  
  annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dashed")), 
                    xmin = (2-critpoint_rbcsdm), xmax = (2-critpoint_rbcsdm), 
                    ymin = -0.2, ymax = 0.52) +
  annotation_custom(grob = linesGrob(gp = gpar(col = "darkgrey", lty = "dotted")), 
                    xmin = (2-critpoint_rbcsdm_confint[1]), xmax = (2-critpoint_rbcsdm_confint[1]), 
                    ymin = -0.2, ymax = 0.5) +
  annotation_custom(grob = linesGrob(gp = gpar(col = "darkgrey", lty = "dotted")), 
                    xmin = (2-critpoint_rbcsdm_confint[2]), xmax = (2-critpoint_rbcsdm_confint[2]), 
                    ymin = -0.2, ymax = 0.52)


rect <- rectGrob(
  x = unit(0.12, "npc"),
  y = unit(0.66, "npc"),
  width = unit(0.05, "npc"),
  height = unit(0.25, "npc"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = "white", alpha = 1, col = "white")
)

tss_performance3 <- ggdraw(tss_performance2) +
  draw_grob(rect) +
  draw_text("Correlative:", x = 0.364, y = 0.1325, size = 8, family = "Helvetica Narrow", fontface="bold") +
  draw_text("Process-based:", x = 0.341, y = 0.081, size = 8, family = "Helvetica Narrow", fontface="bold")
