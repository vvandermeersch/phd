model_performance_lp[model_performance_lp$type2=="Treebased", "type2"] <- "1Treebased"
model_performance_lp[model_performance_lp$type2=="Regressionbased", "type2"] <- "2Regressionbased"
model_performance_lp[model_performance_lp$type2=="Expertprocessbased", "type2"] <- "3Expertprocessbased"
model_performance_lp[model_performance_lp$type2=="Fittedprocessbased", "type2"] <- "4Fittedprocessbased"

evolution_var <- ggplot(model_performance_lp, aes(x = median, y = sd_var^2, color = type2, fill = type2)) +
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "CLIMATE NOVELTY") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125),
                     name = "SKILL VARIANCE") +
  geom_point(size = 0.5, stroke = 0.4) +
  geom_smooth(alpha = 0.3, size = 0.5) + 
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81"),
                     labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81"),
                    labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5),
        legend.position="bottom", legend.title=element_blank(),
        legend.key.size = unit(0.4, "cm")) +
  coord_cartesian(ylim=c(0, 0.135), clip = "on")


dunnTest_out <- FSA::dunnTest(sd_var^2 ~ type2, method = "bh", data = model_performance_lp)
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)

boxplot_var <- ggplot(model_performance_lp, aes(x = type2, y =  sd_var^2, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.13), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.5) +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = c(0, 0.05, 0.1),
                     name = "SKILL VARIANCE") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian(ylim=c(0, 0.135), clip = "on")

assemble_plots <- plot_grid(evolution_var + theme(legend.position="none"), boxplot_var, ncol = 2, rel_widths = c(0.7,0.2),  align = "h")

supp_fig_var <- plot_grid(assemble_plots, get_legend(evolution_var), ncol = 1, rel_heights = c(0.9, 0.1))
