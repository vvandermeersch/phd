#------------------------------#
# Figure 4: TSS with migration #
#------------------------------#

# TEMPORARY SCRIPT
tss_performance_withoutmig <- ggplot(model_performance_withmig, aes(x = median, y = tss, color = type, fill = type)) +
  geom_point(size = 0.3)+
  geom_smooth(method = "loess", alpha = 0.3, size = 0.6) +
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0.2,0.8,0.2),
                     name = toupper("tss")) +
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

tss_performance_withmig <- ggplot(model_performance_withmig, aes(x = median, y = mig_tss, color = type, fill = type)) +
  geom_point(size = 0.3)+
  geom_smooth(method = "loess", alpha = 0.3, size = 0.6) +
  scale_x_continuous(breaks = seq(0.80,2.05,0.1),
                     expand = c(0.001,0.001),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-0.2,0.8,0.2),
                     name = toupper("tss")) +
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
