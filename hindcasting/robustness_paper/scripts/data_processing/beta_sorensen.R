library(glmmTMB)
library("DHARMa")
library(lme4)

hist(model_performance_withmig$sorensen)
range(model_performance_withmig$sorensen)

beta.fit <- glmmTMB(formula = sorensen ~ hypervolume_sorensen*type2, family = ordbeta(link = "logit"), 
                    data = model_performance_withmig)
summary(beta.fit)


check_beta_model <- simulateResiduals(fittedModel = beta.fit, n = 5000)
plot(check_beta_model)
testDispersion(check_beta_model)

newdata <- data.frame(hypervolume_sorensen = rep(seq(0.75,1,length.out = 100),4),
                      type2 = rep(unique(model_performance_withmig$type2), each = 100))

beta.pred <- predict(beta.fit, newdata = newdata, type = "response", se.fit = TRUE)
beta.confint <- confint(beta.fit)

test <- bootMer(beta.fit, predict, nsim = 100, re.form = NA)
pred$CI.lower = apply(test$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
predCI.upper = apply(test$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))




pred <- data.frame(hypervolume_sorensen = newdata$hypervolume_sorensen, 
                   type2 = newdata$type2,
                   sorensen = beta.pred$fit,
                   sd = beta.pred$se.fit)


model_performance_median <- model_performance_withmig %>%
  group_by(type2, hypervolume_sorensen = round(hypervolume_sorensen,2)) %>%
  dplyr::summarise(median_migtss=median(sorensen), sd_migtss=sd(sorensen)) %>%
  as.data.frame()


sorensen_performance <- ggplot(data = pred, aes(x=1-hypervolume_sorensen, y = sorensen, col = type2, fill = type2)) +
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
  geom_line() +
  geom_ribbon(aes(ymin = sorensen-2*sd, ymax = sorensen+2*sd), 
              alpha = 0.2, linetype = "dashed", size = 0.4) +
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
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7.5, margin = margin(r = 0)),
        legend.position="bottom", legend.title=element_blank(),
        legend.key.size = unit(0.4, "cm"), legend.justification = "right",
        legend.spacing.x = unit(0.05, "cm"),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(0, 1), xlim = c(0.05, 0.25), clip = "on")




