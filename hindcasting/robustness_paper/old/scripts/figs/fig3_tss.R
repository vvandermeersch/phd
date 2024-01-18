#----------------------------------------------#
# Figure 3: TSS with linear-plateau regression #
#----------------------------------------------#


tss_performance <- ggplot(model_performance_lp, aes(x = median, y = median_var, color = type, fill = type)) +
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "CLIMATE NOVELTY") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0.3,0.8,0.2),
                     name = "TSS") +
  geom_pointrange(aes(ymin=median_var-sd_var, ymax=median_var+sd_var), position = position_dodge(width = 0.015),
                  linewidth = 0.3, size = 0.1, stroke = 0.4) +
  geom_line(data=conf_intervals, aes(y = intconf.Sim.Median)) +
  geom_ribbon(data=conf_intervals, aes(ymin = intconf.Sim.2.5., ymax = intconf.Sim.97.5.), 
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
  annotate("rect", xmin=round(max(model_performance_lp$median),1),xmax=round(max(model_performance_lp$median),0), 
           ymin=-Inf, ymax=Inf, alpha=0.7, fill="white") +
  geom_segment(aes(x = round(max(model_performance_lp$median),1), y = -Inf, 
                   xend = round(max(model_performance_lp$median),1), 
                   yend = 0.44), 
               size = 0.5, linetype = "dashed", col = "darkgrey") +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title.y = element_text(colour = "black", family= "Noto Sans", size = 9, hjust = 0.2),
        axis.title.x = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank(),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(-0.35, 1.35), xlim = c(0.82, 1.94), clip = "on")


model_performance_withmig$crit <- model_performance_withmig$median<trunc((2-critpoint_csdm)*100)/100

# lower than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type, method = "bh", data = model_performance_withmig[model_performance_withmig$crit == TRUE,])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
data_letters[data_letters$Group == "PHENOFIT(fitted)", "Group"] <- "PHENOFIT (fitted)"
boxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE,], aes(x = type, y = mig_tss, color = type, fill = type)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
            family = "Noto Sans", size = 3) +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 7),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 7),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank()) +
  coord_cartesian(ylim=c(-0.2, 1))

# higher than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type, method = "bh", data = model_performance_withmig[model_performance_withmig$crit == FALSE,])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
data_letters[data_letters$Group == "PHENOFIT(fitted)", "Group"] <- "PHENOFIT (fitted)"
boxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE,], aes(x = type, y = mig_tss, color = type, fill = type)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = toupper(var)) +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
            family = "Noto Sans", size = 3) +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(),
        plot.background = element_blank()) +
  coord_cartesian(ylim=c(-0.2, 1))

library(grid)


tss_performance2 <- tss_performance +
  annotation_custom(ggplotGrob(boxplot_over), xmin = trunc((2-critpoint_csdm)*100)/100, xmax = 1.76, 
                    ymin = 0.79, ymax = 1.4) +
  annotation_custom(ggplotGrob(boxplot_under), xmin = 1.07, xmax = trunc((2-critpoint_csdm)*100)/100, 
                    ymin = 0.79, ymax = 1.4) +
  annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dotted")), 
                    xmin = trunc((2-critpoint_csdm)*100)/100, xmax = trunc((2-critpoint_csdm)*100)/100, 
                    ymin = 0.85, ymax = 1.3) +
  annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dotted")), 
                    xmin = trunc((2-critpoint_csdm)*100)/100, xmax = trunc((2-critpoint_csdm)*100)/100, 
                    ymin = -0.3, ymax = 0.38)
 
rect <- rectGrob(
  x = unit(0.1, "npc"),
  y = unit(0.7, "npc"),
  width = unit(0.05, "npc"),
  height = unit(0.22, "npc"),
  hjust = 0, vjust = 0,
  gp = gpar(fill = "white", alpha = 1, col = "white")
)

tss_performance3 <- ggdraw(tss_performance2) +
  draw_grob(rect)
