
library(mgcv)

newdata <- data.frame(median=seq(min(model_performance_lp$median),round(max(model_performance_lp$median),0), length.out=100))

conf_intervals <- rbind(
  data.frame(median = newdata$median,
             predict(gam(median_var ~ s(median, bs = "cs"), data = model_performance_lp[model_performance_lp$type == "cSDM",]), 
                      se.fit = TRUE, newdata=newdata), 
             type = "cSDM"),
  data.frame(median = newdata$median,
             predict(gam(median_var ~ s(median, bs = "cs"), data = model_performance_lp[model_performance_lp$type == "PHENOFIT",]), 
                     se.fit = TRUE, newdata=newdata), 
             type = "PHENOFIT"),
  data.frame(median = newdata$median,
             predict(gam(median_var ~ s(median, bs = "cs"), data = model_performance_lp[model_performance_lp$type == "PHENOFIT (fitted)",]), 
                     se.fit = TRUE, newdata=newdata), 
             type = "PHENOFIT (fitted)"))



tss_performance_gam <- ggplot(model_performance_lp, aes(x = median, y = median_var, color = type, fill = type)) +
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0.2,0.8,0.2),
                     name = toupper(var)) +
  geom_pointrange(aes(ymin=median_var-sd_var, ymax=median_var+sd_var), position = position_dodge(width = 0.015),
                  linewidth = 0.3, size = 0.1, stroke = 0.4) +
  geom_line(data=conf_intervals, aes(y = fit), size = 0.5) +
  geom_ribbon(data=conf_intervals, aes(y = fit, ymin = fit-2*se.fit, ymax =  fit+2*se.fit), 
              alpha = 0.2, linetype = "dashed") + 
  geom_segment(aes(x = 2-critpoint_csdm, y = -Inf, 
                   xend = 2-critpoint_csdm, 
                   yend = critpoint_csdm_y ), 
               size = 0.8, linetype = "dashed", col = "#e86117") +
  geom_segment(aes(x = 2-critpoint_phenofit, y = -Inf, 
                   xend = 2-critpoint_phenofit, 
                   yend = critpoint_phenofit_y ), 
               size = 0.8, linetype = "dashed", col = "#457b9d") +
  # geom_segment(aes(x = 2-inter_coords[1], y = -Inf, 
  #                  xend = 2-inter_coords[1], 
  #                  yend = inter_coords[2]), 
  #              size = 0.8, linetype = "dashed", col = "#457b9d") +
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
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank()) +
  coord_cartesian(ylim=c(-0.3, 1))
