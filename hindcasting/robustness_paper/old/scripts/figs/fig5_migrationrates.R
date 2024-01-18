#---------------------------#
# Figure 5: migration_rates #
#---------------------------#

f <- function(x) {
  r <- quantile(x, probs = c(0.01, 0.25, 0.5, 0.75, 0.99))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# TEMPORARY SCRIPT
fagus_migration_rates[is.na(fagus_migration_rates$mig_rate), "mig_rate"] <- 0
fagus_meanmig_rates  <- fagus_migration_rates %>%
  group_by(species, type, type2, model, year) %>%
  summarize(mean_rate = max(mig_rate))

fagus_maxmig_rates <- fagus_migration_rates %>%
  group_by(species, type, type2, model) %>%
  summarize(max_rate = max(mig_rate))

fagus_fig_migration_rate <- ggplot(fagus_meanmig_rates, aes(x = type, y = mean_rate, color = type, fill = type)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.4,
               position=position_dodge(0.1)) +
  geom_point(data = fagus_maxmig_rates, aes(x = type, y = max_rate, color = type), shape = 18, size = 2) +
  geom_text(data = fagus_maxmig_rates, aes(x = type, y = max_rate, label = round(max_rate,2), color = type), 
            size = 3, vjust = -1.5, family = "Noto Sans", show.legend = FALSE) +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  
  theme_bw() + 
  labs(x= NULL, y="Migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x =  element_blank(),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank()) +
  ylim(0, 215)




fagus_evol_mig_rates <- fagus_migration_rates %>%
  group_by(species, type, year) %>%
  summarize(mean_rate = mean(mig_rate), sd_rate = sd(mig_rate))

fagus_fig_evolution_mig_rate <- ggplot(fagus_evol_mig_rates, aes(x = year, color = type, fill = type)) +
  geom_line(aes(y = mean_rate)) +
  geom_ribbon(aes(ymin = mean_rate-sd_rate, ymax = mean_rate+sd_rate), alpha = 0.2, col = NA) +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  theme_bw() + 
  labs(x= "Years BP", y="Migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank()) +
  ylim(0, 150) +
  scale_x_reverse(limits=c(11500, 250), breaks = seq(11500, 500,-1000), expand = c(0,0))






fig_evolution_occupied_area <- ggplot(model_occupied_area,
                                      aes(x = year, color = mod, fill = mod)) +
  geom_line(aes(y = area_occupied_km)) +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  scale_fill_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  theme_bw() +
  labs(x= "Years BP", y="Modelled occupied area (km2)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank()) +
  scale_x_reverse(limits=c(11500, 250), breaks = seq(11500, 500,-1000), expand = c(0,0))


# TEMPORARY SCRIPT
abies_migration_rates[is.na(abies_migration_rates$mig_rate), "mig_rate"] <- 0

abies_meanmig_rates  <- abies_migration_rates %>%
  group_by(species, type, model, year) %>%
  summarize(mean_rate = max(mig_rate))

abies_maxmig_rates <- abies_migration_rates %>%
  group_by(species, type) %>%
  summarize(max_rate = max(mig_rate))

abies_fig_migration_rate <- ggplot(abies_meanmig_rates, aes(x = type, y = mean_rate, color = type, fill = type)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.4,
               position=position_dodge(0.1)) +
  geom_point(data = abies_maxmig_rates, aes(x = type, y = max_rate, color = type), shape = 18, size = 2) +
  geom_text(data = abies_maxmig_rates, aes(x = type, y = max_rate, label = round(max_rate,2), color = type), 
            size = 4, vjust = -1.5, family = "Noto Sans", show.legend = FALSE) +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT\n(fitted)", "CASTANEA"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81")) +
  
  theme_bw() + 
  labs(x= NULL, y="Migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x =  element_blank(),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank()) +
  ylim(0, 215)






ggplot(fagus_meanmig_rates, aes(x = model, y = mean_rate, color = model, fill = model)) +
  facet_wrap(~type, scales="free") + 
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.4,
               position=position_dodge(0.1)) +
  theme_bw() + 
  labs(x= NULL, y="Migration rate (m/year)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x =  element_blank(),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank()) +
  ylim(0, 215)



test1 <- ggplot(fagus_meanmig_rates[fagus_meanmig_rates$type =="cSDM",], aes(x = model, y = mean_rate, color = model, fill = model)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.5,
               position=position_dodge(0.1)) +
  geom_point(data = fagus_maxmig_rates[fagus_maxmig_rates$type == "cSDM",], aes(x = model, y = max_rate, color = model), shape = 18, size = 2) +
  geom_text(data = fagus_maxmig_rates[fagus_maxmig_rates$type == "cSDM",], aes(x = model, y = max_rate, label = round(max_rate,2), color = model), 
            size = 3.5, vjust = -1.5, family = "Noto Sans", show.legend = FALSE) +
  geom_text(data = fagus_maxmig_rates[fagus_maxmig_rates$type == "cSDM",], aes(x = model, y = 80, label = model, color = model), 
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


test2 <-ggplot(fagus_meanmig_rates[fagus_meanmig_rates$type =="PHENOFIT",], aes(x = model, y = mean_rate, color = model, fill = model)) +
  stat_summary(fun.data = f, geom="boxplot", 
               alpha = 0.3, width=0.5,
               position=position_dodge(0.1)) +
  geom_point(data = fagus_maxmig_rates[fagus_maxmig_rates$type == "PHENOFIT",], aes(x = model, y = max_rate, color = model), shape = 18, size = 2) +
  geom_text(data = fagus_maxmig_rates[fagus_maxmig_rates$type == "PHENOFIT",], aes(x = model, y = max_rate, label = round(max_rate,2), color = model), 
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

test3 <- ggplot(fagus_meanmig_rates, aes(x = model, y = mean_rate, color = type2, fill = type2)) +
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

assemble <- plot_grid(test1 + theme(legend.position = "none"), test2 + theme(legend.position = "none"), rel_widths = c(0.8, 0.7), ncol = 2)
final <- plot_grid(assemble, get_legend(test3), nrow = 2, rel_heights = c(1,0.1))
