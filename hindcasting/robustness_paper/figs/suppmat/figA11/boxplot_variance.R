
#------------------------------------------#
# Boxplot of model performance (std. dev.) #
#------------------------------------------#

# Model performance variance
model_performance_var <- model_performance_withmig %>%
  group_by(type, year) %>%
  dplyr::summarise(sd_migsorensen=sd(mig_sorensen)) %>%
  as.data.frame()

conoverTest_out <- conover.test::conover.test(x=model_performance_var$sd_migsorensen, 
                                              g=model_performance_var$type, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters <- data.frame(letters)

boxplot_sdperformance <- ggplot(model_performance_var, aes(x = type, y = sd_migsorensen, color = type, fill = type)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1, width = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,0.35,0.05),
                     name = "Performance (std. dev.)") +
  geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = c(rep(0.31,3))), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 3) +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 6.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        plot.margin = unit(c(0, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(0.1, 0.35), clip = "on") 

