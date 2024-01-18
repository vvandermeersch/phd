library(nls2)

test <- model_performance[model_performance$mod != "CASTANEA" & model_performance$mod != "CASTANEA (fitted)" &
                            model_performance$mod != "CASTANEA (fitted) - CO2 fixed" &
                            model_performance$mod != "CASTANEA - CO2 fixed",]
test <- test %>%
  group_by(type, median) %>%
  dplyr::summarise(median_var=median(!!sym(var)), sd_var=sd(!!sym(var)))

# find starting values 
p_csdm <- nls2(median_var~SSlogis(median,Asym,xmid,scal),data=test,
              subset=(test$type=="cSDM"))
p_phenofit <- nls2(median_var~SSlogis(median,Asym,xmid,scal),data=test,
                         subset=(test$type=="PHENOFIT"))
p_phenofitfitted <- nls2(median_var~SSlogis(median,Asym,xmid,scal),data=test,
                  subset=(test$type=="PHENOFIT (fitted)"))

# one model with same parameters for all type
fit_all <- nls(median_var~SSlogis(median,Asym,xmid,scal),data=test, weights = 1/sd_var^2)

# one model with different parameters per type
test2 <- test
test2$type <- factor(test2$type, levels = c("cSDM", "PHENOFIT (fitted)", "PHENOFIT"))
fit_type <- nls(median_var ~ Asym[type]/(1+exp((xmid[type]-median)/scal[type])), data = test2,
                      start=list(Asym=c(0.48,0.50,0.39), 
                                 xmid = c(1.65,1.81, 2.18), 
                                 scal = c(-0.058,-0.09, -0.57)))

anova(fit_all, fit_type)

# pairwise comparison
tab <- summary(fit_type)
Asym <- as.numeric(tab$coef[1:3,1])
Asym_sd  <- as.numeric(tab$coef[1:3,2])
df <- tab$df[2]




pframe0 <- data.frame(median=seq(min(test$median),max(test$median), length.out=1000))
# pp <- rbind(
#   data.frame(pframe0,median_var=predict(p_csdm,pframe0),
#              intconf = predictNLS(p_csdm, newdata=pframe0, interval="confidence", alpha=0.05, nsim=10000)$summary,
#              intpred = predictNLS(p_csdm, newdata=pframe0, interval="prediction", alpha=0.05, nsim=10000)$summary,
#              type="cSDM",wts=FALSE),
#   data.frame(pframe0,median_var=predict(p_phenofit,pframe0),
#              intconf = predictNLS(p_phenofit, newdata=pframe0, interval="confidence", alpha=0.05, nsim=10000)$summary,
#              intpred = predictNLS(p_phenofit, newdata=pframe0, interval="prediction", alpha=0.05, nsim=10000)$summary,
#              type="PHENOFIT",wts=FALSE),
#   data.frame(pframe0,median_var=predict(p_phenofitfitted,pframe0),
#              intconf = predictNLS(p_phenofitfitted, newdata=pframe0, interval="confidence", alpha=0.05, nsim=10000)$summary,
#              intpred = predictNLS(p_phenofitfitted, newdata=pframe0, interval="prediction", alpha=0.05, nsim=10000)$summary,
#              type="PHENOFIT (fitted)",wts=FALSE)
# )
# 
# 
# y.conf <- predictNLS(p_phenofitfitted, pframe0, interval="confidence", alpha=0.05, nsim=1000000)$summary
# y.pred <- predictNLS(p_phenofitfitted, pframe0, interval="prediction", alpha=0.05, nsim=10000)$summary
# 
# 
# plot(test2$median_var ~ test2$median)
# matlines(pframe0$median, y.conf[,c("Sim.2.5%", "Sim.97.5%")], col="red", lty="dashed")
# matlines(pframe0$median, y.pred[,c("Sim.2.5%", "Sim.97.5%")], col="blue", lty="solid")



library(investr)
# only first-order Taylor expansion
interval <- 
pp <- rbind(
  as.data.frame(predFit(p_csdm, newdata = pframe0, interval = "confidence", level= 0.95)) %>% 
    mutate(median= pframe0$median, type = "cSDM"),
  as.data.frame(predFit(p_phenofitfitted, newdata = pframe0, interval = "confidence", level= 0.95)) %>% 
    mutate(median= pframe0$median, type = "PHENOFIT (fitted)"),
  as.data.frame(predFit(p_phenofit, newdata = pframe0, interval = "confidence", level= 0.95)) %>% 
    mutate(median= pframe0$median, type = "PHENOFIT"))


plot <- ggplot(test, aes(x = median, y = median_var, color = type, fill = type)) +
  geom_pointrange(aes(ymin=median_var-sd_var, ymax=median_var+sd_var), position = position_dodge(width = 0.015), size = 0.3)+
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                expand = c(0.0001,0.0001),
                name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = toupper(var)) +
  geom_line(data=pp, aes(y = fit)) +
  geom_ribbon(data=pp, aes(y = fit, ymin = lwr, ymax = upr), 
              alpha = 0.2) + 
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


# propagate R package
# second-order Taylor expansion
library(propagate)
pp <- rbind(
  data.frame(pframe0,median_var=predict(p_csdm,pframe0),
             intconf = predictNLS(p_csdm, newdata=pframe0, interval="confidence", alpha=0.1, nsim=100000)$summary,
             intpred = predictNLS(p_csdm, newdata=pframe0, interval="prediction", alpha=0.1, nsim=100000)$summary,
             type="cSDM",wts=FALSE),
  data.frame(pframe0,median_var=predict(p_phenofit,pframe0),
             intconf = predictNLS(p_phenofit, newdata=pframe0, interval="confidence", alpha=0.1, nsim=100000)$summary,
             intpred = predictNLS(p_phenofit, newdata=pframe0, interval="prediction", alpha=0.1, nsim=100000)$summary,
             type="PHENOFIT",wts=FALSE),
  data.frame(pframe0,median_var=predict(p_phenofitfitted,pframe0),
             intconf = predictNLS(p_phenofitfitted, newdata=pframe0, interval="confidence", alpha=0.1, nsim=100000)$summary,
             intpred = predictNLS(p_phenofitfitted, newdata=pframe0, interval="prediction", alpha=0.1, nsim=100000)$summary,
             type="PHENOFIT (fitted)",wts=FALSE)
)


ggplot(test, aes(x = median, y = median_var, color = type, fill = type)) +
  geom_pointrange(aes(ymin=median_var-sd_var, ymax=median_var+sd_var), position = position_dodge(width = 0.015), size = 0.3)+
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.0001,0.0001),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = toupper(var)) +
  geom_ribbon(data=pp[pp$type != "PHENOFIT",], aes(ymin = intconf.Sim.5., ymax = intconf.Sim.95.), 
              alpha = 0.2) + 
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

