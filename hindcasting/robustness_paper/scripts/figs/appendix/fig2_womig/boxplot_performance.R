
#---------------------------------------#
# Fig.2-b: Boxplot of model performance #
#---------------------------------------#

model_performance_withmig$earlyholoc <- model_performance_withmig$year>=8500

period_names <- c(
  `TRUE` = "Early Holocene",
  `FALSE` = "Late-Middle Holocene"
)


# Middle-Late Holocene
conoverTest_out <- conover.test::conover.test(x=model_performance_withmig[model_performance_withmig$earlyholoc == FALSE,]$mig_sorensen, 
                                              g=model_performance_withmig[model_performance_withmig$earlyholoc == FALSE,]$type, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters_late <- data.frame(letters)
data_letters_late$earlyholoc <- FALSE


# Early Holocene
conoverTest_out <- conover.test::conover.test(x=model_performance_withmig[model_performance_withmig$earlyholoc == TRUE,]$mig_sorensen, 
                                              g=model_performance_withmig[model_performance_withmig$earlyholoc == TRUE,]$type, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters_early <- data.frame(letters)
data_letters_early$earlyholoc <- TRUE

data_letters <- rbind(data_letters_late, data_letters_early)

boxplot_performance <- ggplot(model_performance_withmig, aes(x = type, y = mig_sorensen, color = type, fill = type)) +
  facet_wrap(~earlyholoc, labeller = as_labeller(period_names)) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.1, width = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,1,0.2),
                     name = "Performance") +
  # geom_text(data = data_letters, aes(x = as.character(Group), label = Letter, y = 0.93), vjust = 0, inherit.aes = F,
  #           family = "Helvetica Narrow", size = 3) +
  scale_color_manual(breaks= c("1Correlative", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Correlative", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#e86117", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9)) +
  coord_cartesian(ylim=c(0, 1), clip = "off") 
  # geom_segment(data = data_frame(earlyholoc = FALSE), aes(x = 4, xend = 4, 
  #                                                         y = 0, yend = 1), 
  #              size = 0.5, linetype = "dashed", col = "darkgrey", inherit.aes = FALSE)

