
#----------------------------------#
# Boxplot of model transferability #
#----------------------------------#

model_performance_relchg$earlyholoc <- model_performance_relchg$year>=8500
model_performance_relchg$type_test <- paste0(model_performance_relchg$type, model_performance_relchg$test)

# Middle-Late Holocene
conoverTest_out <- conover.test::conover.test(x=model_performance_relchg[model_performance_relchg$earlyholoc == FALSE,]$rel_chg_sorensen, 
                                              g=model_performance_relchg[model_performance_relchg$earlyholoc == FALSE,]$type_test, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters_late <- data.frame(letters)
data_letters_late[, "type"] <- tidyr::separate(data.frame(A = data_letters_late$Group), col = "A" , into = c("X", "Y"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")[,1]
data_letters_late[, "test"] <- tidyr::separate(data.frame(A = data_letters_late$Group), col = "A" , into = c("X", "Y"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")[,2]
data_letters_late[, "sign"] <- NA
data_letters_late[, "sign_position"] <- NA
data_letters_late[data_letters_late$test == "2Crossvalidationreference", "sign"] <- "CV"
data_letters_late[data_letters_late$test == "2Crossvalidationreference", "sign_position"] <- -5
data_letters_late$earlyholoc <- FALSE


# Early Holocene
conoverTest_out <- conover.test::conover.test(x=model_performance_relchg[model_performance_relchg$earlyholoc == TRUE,]$rel_chg_sorensen, 
                                              g=model_performance_relchg[model_performance_relchg$earlyholoc == TRUE,]$type_test, 
                                              kw=FALSE,
                                              method="by", table = FALSE)
letters <- rcompanion::cldList(P.adjusted ~ comparisons, data = conoverTest_out, threshold = 0.05)
data_letters_early <- data.frame(letters)
data_letters_early[, "type"] <- tidyr::separate(data.frame(A = data_letters_early$Group), col = "A" , into = c("X", "Y"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")[,1]
data_letters_early[, "test"] <- tidyr::separate(data.frame(A = data_letters_early$Group), col = "A" , into = c("X", "Y"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")[,2]
data_letters_early[, "sign"] <- NA
data_letters_early[, "sign_position"] <- NA
data_letters_early[data_letters_early$test == "2Crossvalidationreference", "sign"] <- "CV"
data_letters_early[data_letters_early$test == "2Crossvalidationreference", "sign_position"] <- -40
data_letters_early$earlyholoc <- TRUE

data_letters <- rbind(data_letters_late, data_letters_early)

boxplot_transferability <- ggplot(model_performance_relchg) +
  facet_wrap(~earlyholoc, labeller = as_labeller(period_names)) +
  geom_boxplot(aes(x=type, y=rel_chg_sorensen*100, col = type, fill = type, group = type_test),  position = position_dodge2(preserve = "single"),
               alpha = 0.3, outlier.size = 0.1) +
  geom_text(data = data_letters, aes(x = type, label = Letter, y = 135, group = Group), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 3, position=position_dodge(width=0.8)) +
  geom_text(data = data_letters, aes(x = type, label = sign, y = sign_position, group = Group), vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.3, position=position_dodge(width=0.75)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-100,150,50),
                     name = "Transferability") +
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
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9)) +
  coord_cartesian(ylim=c(-100, 150), clip = "off") +
  geom_segment(data = data_frame(earlyholoc = FALSE), aes(x = 3.6, xend = 3.6,
                   y = -100, yend = 150),
               size = 0.5, linetype = "dashed", col = "darkgrey")


