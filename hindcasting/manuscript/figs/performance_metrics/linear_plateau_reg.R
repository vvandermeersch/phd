library(minpack.lm)
library(nlraa)
library(propagate)

test <- model_performance[model_performance$mod != "CASTANEA" & model_performance$mod != "CASTANEA (fitted)" &
                            model_performance$mod != "CASTANEA (fitted) - CO2 fixed" &
                            model_performance$mod != "CASTANEA - CO2 fixed",]
test <- test %>%
  group_by(type, median) %>%
  dplyr::summarise(median_var=median(!!sym(var)), sd_var=sd(!!sym(var))) %>%
  as.data.frame()


# find starting values 
p_csdm <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), data = test[test$type == "cSDM",], weights = 1/sd_var^2)

p_phenofit <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), data = test[test$type == "PHENOFIT",], weights = 1/sd_var^2)

p_phenofitfitted <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), data = test[test$type == "PHENOFIT (fitted)",], weights = 1/sd_var^2)



pframe0 <- data.frame(median=seq(min(test$median),round(max(test$median),0), length.out=1000))

# second-order Taylor expansion
pp <- rbind(
  data.frame(pframe0,median_var=predict(p_csdm,pframe0),
             intconf = predictNLS(p_csdm, newdata=pframe0, interval="confidence", alpha=0.05, nsim=100000)$summary,
             type="cSDM",wts=FALSE),
  data.frame(pframe0,median_var=predict(p_phenofit,pframe0),
             intconf = predictNLS(p_phenofit, newdata=pframe0, interval="confidence", alpha=0.05, nsim=100000)$summary,
             type="PHENOFIT",wts=FALSE),
  data.frame(pframe0,median_var=predict(p_phenofitfitted,pframe0),
             intconf = predictNLS(p_phenofitfitted, newdata=pframe0, interval="confidence", alpha=0.05, nsim=100000)$summary,
             type="PHENOFIT (fitted)",wts=FALSE)
)


critpoint_csdm <- coef(p_csdm)[["jp"]]
critpoint_csdm_y <- coef(p_csdm)[["a"]]+critpoint_csdm*coef(p_csdm)[["b"]]


# find intersection of cSDM and PHENOFIT
A <- matrix(c(coef(p_phenofit)[["b"]], -1,
              coef(p_csdm)[["b"]], -1), byrow = T, nrow = 2)

b <- c(-coef(p_phenofit)[["a"]], -coef(p_csdm)[["a"]])
inter_coords <- solve(A, b)

plot <- ggplot(test, aes(x = median, y = median_var, color = type, fill = type)) +
  geom_pointrange(aes(ymin=median_var-sd_var, ymax=median_var+sd_var), position = position_dodge(width = 0.015), size = 0.3)+
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0.2,0.8,0.2),
                     name = toupper(var)) +
  geom_line(data=pp, aes(y = intconf.Sim.Median)) +
  geom_ribbon(data=pp, aes(ymin = intconf.Sim.2.5., ymax = intconf.Sim.97.5.), 
              alpha = 0.2, linetype = "dashed") + 
  geom_segment(aes(x = 2-critpoint_csdm, y = -Inf, 
                   xend = 2-critpoint_csdm, 
                   yend = critpoint_csdm_y ), 
               size = 0.8, linetype = "dashed", col = "#e86117") +
  geom_segment(aes(x = 2-inter_coords[1], y = -Inf, 
                   xend = 2-inter_coords[1], 
                   yend = inter_coords[2]), 
               size = 0.8, linetype = "dashed", col = "#457b9d") +
  annotate("rect", xmin=round(max(test$median),1),xmax=round(max(test$median),0), ymin=-Inf, ymax=Inf, alpha=0.7, fill="white") +
  geom_segment(aes(x = round(max(test$median),1), y = -Inf, 
                   xend = round(max(test$median),1), 
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
  coord_cartesian(ylim=c(-0.3, 0.9))
