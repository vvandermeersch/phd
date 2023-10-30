# remotes::install_github("stan-dev/cmdstanr", force = TRUE)
# 
# library(cmdstanr)
# cmdstanr::check_cmdstan_toolchain()
# install_cmdstan(overwrite=TRUE)
# 
# remotes::install_github("saudiwin/ordbetareg_pack",build_vignettes=TRUE,dependencies = TRUE)

ggplot(data = model_performance_withmig, aes(x = year, y = mig_sorensen, fill = type2, col = type2)) +
  geom_point() +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81"),
                     labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81"),
                    labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration"))

hist(1-model_performance_withmig$hypervolume_sorensen)
hist(model_performance_withmig$mig_sorensen)
hist(log(model_performance_withmig$mig_sorensen))

library(ordbetareg)
ord_fit_mean <- ordbetareg(formula=mig_sorensen ~ hypervolume_sorensen*mod, 
                           data=model_performance_withmig,
                           backend="cmdstanr",
                           control=list(adapt_delta=0.95),
                           chains = 1,
                           iter = 500,
                           cores =1,
                           true_bounds = c(0,1),
                           silent = 0)

plot(ord_fit_mean)

all_draws <- prepare_predictions(ord_fit_mean)

cutzero <- plogis(all_draws$dpars$cutzero)
cutone <- plogis(all_draws$dpars$cutzero + exp(all_draws$dpars$cutone))

model_performance_withmig %>% 
  ggplot(aes(x=sorensen)) +
  geom_histogram(bins=100) +
  theme_minimal() + 
  theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=c(0,0.25,0.50,0.75,1),
                     labels=c("0","Bad","0.5","Good","1")) +
  geom_vline(xintercept = mean(cutzero),linetype=2) +
  geom_vline(xintercept = mean(cutone),linetype=2) +
  ylab("") +
  xlab("")


ord_pred <- conditional_effects(ord_fit_mean)[[3]]

model_performance_median <- model_performance_withmig %>%
  group_by(mod, hypervolume_sorensen) %>%
  dplyr::summarise(median_migtss=median(mig_sorensen), sd_migtss=sd(mig_sorensen)) %>%
  as.data.frame()

ggplot(data = ord_pred, aes(y=estimate__,x=1-hypervolume_sorensen, col = mod, fill = mod)) +
  geom_ribbon(aes(ymin=lower__,
                  ymax=upper__),
              alpha=0.3,
              linetype = "dashed", size = 0.4) +
  geom_line() +
  geom_pointrange(data = model_performance_median, aes(y = median_migtss, 
                                                       ymin=median_migtss-sd_migtss, 
                                                       ymax=median_migtss+sd_migtss), 
                  position = position_dodge(width = 0.015),
                  linewidth = 0.3, size = 0.2, stroke = 0.4) +
  scale_x_continuous(breaks = seq(0.05,0.3,0.05),
                     expand = c(0,0),
                     name = "Climatic distance") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0,1,0.2),
                     name = "Predictive performance") +
  # scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
  #                    values= c( "#457b9d", "#82BCC4", "#e86117","#995D81"),
  #                    labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  # scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
  #                   values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81"),
  #                   labels = c("Tree-based", "Regression-based", 'Expert calibration', "Inverse calibration")) +
  theme_bw() + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE), color = guide_legend(nrow=1,byrow=TRUE)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 10, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 10, margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 0)),
        legend.position="bottom", legend.title=element_blank(),
        legend.key.size = unit(0.4, "cm"), legend.justification = "center",
        legend.spacing.x = unit(0.05, "cm"),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(ylim=c(0, 1), xlim = c(0.05, 0.22), clip = "on")


pp <- predict(ord_fit_mean, probs = c(0.05, 0.95))

obs_vs_pred <- data.frame(obs = model_performance_withmig$sorensen, pred = pp[,1], qmin = pp[,3], qmax = pp[,4])

ggplot(data = obs_vs_pred, aes(x = obs, y = pred)) +
  geom_pointrange(aes(ymin=qmin, ymax=qmax), 
                  position = position_dodge(width = 0.015),
                  linewidth = 0.3, size = 0.2, stroke = 0.4) +
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(ylim=c(0, 1), xlim = c(0, 1), clip = "on")

