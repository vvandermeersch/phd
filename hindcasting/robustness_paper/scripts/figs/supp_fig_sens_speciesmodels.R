
# Quercus
# lower than critical point
dunnTest_out <- FSA::dunnTest(sens ~ mod, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == TRUE & model_performance_withmig$species == "quercus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
data_letters <- rbind(data_letters, c("8CASTANEA(fitted)", NA, NA))
qboxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE & model_performance_withmig$species == "quercus",], 
                         aes(x = mod, y = sens, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Sensitivity") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 1.05), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.5) +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text.x = element_text(colour = "black", family= "Helvetica Narrow", size = 6.5, angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title.x = element_blank(),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        plot.margin = margin(t = 12, b = 5.5, r = 5.5, l = 5.5)) +
  coord_cartesian(ylim=c(0, 1), clip = "off") +
  scale_x_discrete(labels=c("1BRT" = "BRT", "2RandomForest" = "Random\nForest", "3GAM" = "GAM", "4LassoGLM" = "Lasso GLM",
                            "5PHENOFIT" = "PHENOFIT", "6CASTANEA" = "CASTANEA", "7PHENOFIT(fitted)" = "PHENOFIT\nfitted",
                            "8CASTANEA(fitted)" = "CASTANEA\nfitted"))

# higher than critical point
dunnTest_out <- FSA::dunnTest(sens ~ mod, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == FALSE & model_performance_withmig$species == "quercus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
data_letters <- rbind(data_letters, c("8CASTANEA(fitted)", NA, NA))
qboxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE & model_performance_withmig$species == "quercus",], 
                        aes(x = mod, y = sens, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Sensitivity") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 1.05), vjust = 0, inherit.aes = F,
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
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 6.5, angle = 45, hjust = 1),
        axis.text.y = element_blank(), axis.title = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(t = 12, b = 5.5, r = 5.5, l = 5.5)) +
  coord_cartesian(ylim=c(0, 1), clip = "off") +
  scale_x_discrete(labels=c("1BRT" = "BRT", "2RandomForest" = "Random\nForest", "3GAM" = "GAM", "4LassoGLM" = "Lasso GLM",
                            "5PHENOFIT" = "PHENOFIT", "6CASTANEA" = "CASTANEA", "7PHENOFIT(fitted)" = "PHENOFIT\nfitted", 
                            "8CASTANEA(fitted)" = "CASTANEA\nfitted"))

# Abies
# lower than critical point
dunnTest_out <- FSA::dunnTest(sens ~ mod, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == TRUE & model_performance_withmig$species == "abies",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
aboxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE & model_performance_withmig$species == "abies",], 
                         aes(x = mod, y = sens, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Sensitivity") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 1.05), vjust = 0, inherit.aes = F,
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
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        plot.margin = margin(t = 12, b = 5.5, r = 5.5, l = 5.5)) +
  coord_cartesian(ylim=c(0, 1), clip = "off")

# higher than critical point
dunnTest_out <- FSA::dunnTest(sens ~ mod, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == FALSE & model_performance_withmig$species == "abies",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
aboxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE & model_performance_withmig$species == "abies",], 
                        aes(x = mod, y = sens, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Sensitivity") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 1.05), vjust = 0, inherit.aes = F,
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
        plot.background = element_blank(),
        plot.margin = margin(t = 12, b = 5.5, r = 5.5, l = 5.5)) +
  coord_cartesian(ylim=c(0, 1), clip = "off")

# Fagus
# lower than critical point
dunnTest_out <- FSA::dunnTest(sens ~ mod, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == TRUE & model_performance_withmig$species == "fagus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
data_letters <- rbind(data_letters, c("8CASTANEA(fitted)", NA, NA))
fboxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE & model_performance_withmig$species == "fagus",], 
                         aes(x = mod, y = sens, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Sensitivity") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 1.05), vjust = 0, inherit.aes = F,
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
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        plot.margin = margin(t = 12, b = 5.5, r = 5.5, l = 5.5)) +
  coord_cartesian(ylim=c(0, 1), clip = "off")

# higher than critical point
dunnTest_out <- FSA::dunnTest(sens ~ mod, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == FALSE & model_performance_withmig$species == "fagus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
data_letters <- rbind(data_letters, c("8CASTANEA(fitted)", NA, NA))
fboxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE & model_performance_withmig$species == "fagus",], 
                        aes(x = mod, y = sens, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Sensitivity") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 1.05), vjust = 0, inherit.aes = F,
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
        plot.background = element_blank(),
        plot.margin = margin(t = 12, b = 5.5, r = 5.5, l = 5.5)) +
  coord_cartesian(ylim=c(0, 1), clip = "off")


species_plot <- plot_grid(aboxplot_under, aboxplot_over,
                          fboxplot_under, fboxplot_over,
                          qboxplot_under, qboxplot_over,
                          ncol = 2,
                          align = "v",
                          axis = "ltrb",
                          labels = c('A)', '', 'B)', '', 'C)', ''),
                          label_fontfamily = "Helvetica Narrow",
                          label_fontface = "plain",
                          label_size = 8.5, rel_heights = c(2, 2, 2.8))
supp_fig_speciesmodels <- species_plot + annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dotted")), 
                                                           xmin = 0.53, xmax = 0.53, 
                                                           ymin = 0.13, ymax = 0.97)



