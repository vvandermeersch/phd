
# Quercus
# lower than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type2, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == TRUE & model_performance_withmig$species == "quercus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
qboxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE & model_performance_withmig$species == "quercus",], 
                         aes(x = type2, y = mig_tss, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
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
  coord_cartesian(ylim=c(-0.2, 1))

# higher than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type2, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == FALSE & model_performance_withmig$species == "quercus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
qboxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE & model_performance_withmig$species == "quercus",], 
                        aes(x = type2, y = mig_tss, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
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
  coord_cartesian(ylim=c(-0.2, 1))

# Abies
# lower than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type2, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == TRUE & model_performance_withmig$species == "abies",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
aboxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE & model_performance_withmig$species == "abies",], 
                         aes(x = type2, y = mig_tss, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
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
  coord_cartesian(ylim=c(-0.2, 1))

# higher than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type2, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == FALSE & model_performance_withmig$species == "abies",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
aboxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE & model_performance_withmig$species == "abies",], 
                        aes(x = type2, y = mig_tss, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
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
  coord_cartesian(ylim=c(-0.2, 1))

# Fagus
# lower than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type2, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == TRUE & model_performance_withmig$species == "fagus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
fboxplot_under <- ggplot(model_performance_withmig[model_performance_withmig$crit ==TRUE & model_performance_withmig$species == "fagus",], 
                         aes(x = type2, y = mig_tss, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
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
  coord_cartesian(ylim=c(-0.2, 1))

# higher than critical point
dunnTest_out <- FSA::dunnTest(mig_tss ~ type2, method = "bh", data = model_performance_withmig[
  model_performance_withmig$crit == FALSE & model_performance_withmig$species == "fagus",])
letters <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest_out$res, threshold = 0.05)
data_letters <- data.frame(letters)
fboxplot_over <- ggplot(model_performance_withmig[model_performance_withmig$crit == FALSE & model_performance_withmig$species == "fagus",], 
                        aes(x = type2, y = mig_tss, color = type2, fill = type2)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,0.8,0.2),
                     name = "TSS") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.87), vjust = 0, inherit.aes = F,
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
  coord_cartesian(ylim=c(-0.2, 1))


species_plot <- plot_grid(aboxplot_under, aboxplot_over,
                          fboxplot_under, fboxplot_over,
                          qboxplot_under, qboxplot_over,
                          ncol = 2,
                          align = "hv",
                          labels = c('A)', '', 'B)', '', 'C)', ''),
                          label_fontfamily = "Helvetica Narrow",
                          label_fontface = "plain",
                          label_size = 8.5)
supp_fig_species <- species_plot + annotation_custom(grob = linesGrob(gp = gpar(col = "black", lty = "dotted")), 
                                 xmin = 0.53, xmax = 0.53, 
                                 ymin = 0.03, ymax = 0.97)



