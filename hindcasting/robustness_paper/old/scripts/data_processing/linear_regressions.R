library(nlme)
test_df <- model_performance_lp
test_df$median_var <- log(1-test_df$median_var)
test_df$median <- log(2.1-test_df$median)

test <- lmList(median_var ~ median| type, data=test_df)

newdata <- data.frame(median=rep(seq(log(2.1-0.8),log(2.1-2), length.out=100),3),
                      type = c(rep("cSDM", 100), rep("PHENOFIT", 100), rep("PHENOFIT (fitted)", 100)))
conf_int <- do.call(rbind, lapply(c("cSDM", "PHENOFIT", "PHENOFIT (fitted)"), 
                                  function(x) predict(test[[x]], newdata[newdata$type ==x,], interval="confidence")))

newdata <- cbind(newdata, conf_int)

newdata$median <- 2.1 -exp(newdata$median)
newdata$fit <- 1 - exp(newdata$fit)
newdata$lwr <- 1 - exp(newdata$lwr)
newdata$upr <- 1 - exp(newdata$upr)



tss_performance_lm <- ggplot(model_performance_lp, aes(x = median, y = median_var, color = type, fill = type)) +
  geom_pointrange(aes(ymin=median_var-sd_var, ymax=median_var+sd_var), position = position_dodge(width = 0.015), size = 0.3)+
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0.2,0.8,0.2),
                     name = toupper(var)) +
  geom_line(data = newdata, aes(y= fit)) +
  geom_ribbon(data=newdata, aes(y = fit, ymin = lwr, ymax = upr), 
              alpha = 0.2, linetype = "dashed") + 
  annotate("rect", xmin=round(max(model_performance_lp$median),1),xmax=round(max(model_performance_lp$median),0), 
           ymin=-Inf, ymax=Inf, alpha=0.7, fill="white") +
  geom_segment(aes(x = round(max(model_performance_lp$median),1), y = -Inf, 
                   xend = round(max(model_performance_lp$median),1), 
                   yend = Inf), 
               size = 0.8, linetype = "dashed", col = "darkgrey") +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank()) +
  coord_cartesian(ylim=c(-0.3, 1))

