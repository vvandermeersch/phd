

lin.mod <- glm((1-sorensen) ~ type2*hypervolume_sorensen, 
                   data=model_performance_withmig, 
                   family=gaussian(link="identity"))

plot(lin.mod, 1)
plot(lin.mod, 3)

log.lin.mod <- glm(log(1-sorensen) ~ type2*hypervolume_sorensen, 
                   data=model_performance_withmig, 
                   family=gaussian(link="identity"))
plot(log.lin.mod, 3)
plot(log.lin.mod, 2)

hist(1-model_performance_withmig$sorensen)


glmGamma <- glm((1-mig_sorensen) ~ type2*hypervolume_sorensen, 
                data=model_performance_withmig,
                family = Gamma(link = "log"))
library(MASS)
myshape <- gamma.shape(glmGamma)
gampred <- predict(glmGamma , type = "response", se = TRUE, 
                   dispersion = 1/myshape$alpha) 
summary(glmGamma, dispersion = 1/myshape$alpha)

library("DHARMa")
check_gamma_model <- simulateResiduals(fittedModel = glmGamma, n = 500)
plot(check_gamma_model)

newdata <- data.frame(hypervolume_sorensen = rep(seq(0.75,0.95,length.out = 100),4),
                      type2 = rep(unique(model_performance_withmig$type2), each = 100))
pred <- data.frame(hypervolume_sorensen = newdata$hypervolume_sorensen, 
                   type2 = newdata$type2,
                   sorensen = 1-predict(glmGamma, newdata = newdata, type = "response"))


model_performance_median <- model_performance_withmig %>%
  group_by(type2, hypervolume_sorensen) %>%
  dplyr::summarise(median_migtss=median(sorensen), sd_migtss=sd(sorensen)) %>%
  as.data.frame()

ggplot(data = pred, aes(x=1-hypervolume_sorensen, y = sorensen, col = type2)) +
  geom_pointrange(data = model_performance_median, aes(y = median_migtss, 
                                                       ymin=median_migtss-sd_migtss, 
                                                       ymax=median_migtss+sd_migtss), 
                  position = position_dodge(width = 0.015),
                  linewidth = 0.3, size = 0.1, stroke = 0.4) +
  geom_line() +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81"),
                     labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration"))


