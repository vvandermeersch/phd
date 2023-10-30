#---------------------------#
# Figure 5: migration_rates #
#---------------------------#

f <- function(x) {
  r <- quantile(x, probs = c(0.01, 0.25, 0.5, 0.75, 0.99))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# TEMPORARY SCRIPT
meanmig_rates  <- migration_rates %>%
  group_by(species, type, type2, model, year) %>%
  summarize(mean_rate = max(mig_rate))

maxmig_rates <- migration_rates %>%
  group_by(species, type, type2, model) %>%
  summarize(max_rate = max(mig_rate))


csdm_rates <- ggplot(meanmig_rates[meanmig_rates$type =="cSDM",], aes(x = model, y = mean_rate, color = model, fill = model)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.5,
               position=position_dodge(0.1)) +
  geom_point(data = maxmig_rates[maxmig_rates$type == "cSDM",], aes(x = model, y = max_rate, color = model), shape = 18, size = 2) +
  geom_text(data = maxmig_rates[maxmig_rates$type == "cSDM",], aes(x = model, y = max_rate, label = round(max_rate,2), color = model), 
            size = 3.5, vjust = -1.5, family = "Noto Sans", show.legend = FALSE) +
  geom_text(data = maxmig_rates[maxmig_rates$type == "cSDM",], aes(x = model, y = y_geomtext_csdm, label = model, color = model), 
            size = 3.5, vjust = -0.5, family = "Noto Sans", show.legend = FALSE, angle = 90) +
  scale_color_manual(breaks= c('BRT', 'RF', 'GAM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                     values= c("#e86117", "#e86117", "#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('BRT','RF','GAM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                    values= c("#e86117","#e86117", "#e86117",  "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  labs(x= NULL, y="Mean yearly migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x =  element_blank(),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank(),
        plot.margin = unit(c(0,-0.1,0.5,0), "cm")) +
  ylim(0, 215)


phenofit_rates <-ggplot(meanmig_rates[meanmig_rates$type =="PHENOFIT",], aes(x = model, y = mean_rate, color = model, fill = model)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.5,
               position=position_dodge(0.1)) +
  geom_point(data = maxmig_rates[maxmig_rates$type == "PHENOFIT",], aes(x = model, y = max_rate, color = model), shape = 18, size = 2) +
  geom_text(data = maxmig_rates[maxmig_rates$type == "PHENOFIT",], aes(x = model, y = max_rate, label = round(max_rate,2), color = model), 
            size = 3.5, vjust = -1.5, family = "Noto Sans", show.legend = FALSE) +
  scale_color_manual(breaks= c('BRT', 'Random Forest', 'GAM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                     values= c("#e86117", "#e86117", "#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('BRT','Random Fores','GAM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                    values= c("#e86117","#e86117", "#e86117",  "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  labs(x= NULL, y="Migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks =  element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank(),
        plot.margin = unit(c(0,-0.1,0.5,-0.1), "cm")) +
  ylim(0, 215)

all_legend <- ggplot(meanmig_rates, aes(x = model, y = mean_rate, color = type2, fill = type2)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.5,
               position=position_dodge(0.1)) +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  
  theme_bw() + 
  labs(x= NULL, y="Migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks =  element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 10),
        legend.position="bottom", legend.title=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  ylim(0, 215)

assemble <- plot_grid(csdm_rates + theme(legend.position = "none"), phenofit_rates + theme(legend.position = "none"), rel_widths = c(0.8, 0.7), ncol = 2)
fig_migration_rates <- plot_grid(assemble, get_legend(all_legend), nrow = 2, rel_heights = c(1,0.1))