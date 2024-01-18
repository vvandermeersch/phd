model_performance_relchg <- model_performance_relchg  %>%
  filter(!is.na(rel_chg_tss))

median_relchg <- model_performance_relchg[model_performance_relchg$test == "1Observedreference",] %>%
  group_by(type2, crit) %>%
  summarize(median=median(rel_chg_tss, na.rm = TRUE))

median_relchg$ymin <- ifelse(median_relchg$crit==TRUE, -4.35, 0)
median_relchg$ymax <- ifelse(median_relchg$crit==TRUE, 0, 4.35)


ggplot(data = model_performance_relchg[model_performance_relchg$test == "1Observedreference",], aes(fill = type2)) +
  #facet_grid(rows = vars(type2)) +
  
  geom_vline(aes(xintercept = 0), linetype = "dotted", alpha =0.2) +
  
  geom_density(data = model_performance_relchg[model_performance_relchg$crit == TRUE & 
                                                   model_performance_relchg$test == "1Observedreference",,], 
                 aes(x = rel_chg_tss, y = -..density..), alpha = 0.35, size = 0.1, bins = 30, color = "white") +
  
  geom_density(data = model_performance_relchg[model_performance_relchg$crit == FALSE &
                                                   model_performance_relchg$test == "1Observedreference",,], 
                 aes(x = rel_chg_tss, y = ..density..), alpha = 0.35, size = 0.1, bins = 30, color = "white") +
  # median
  geom_segment(data = median_relchg, aes(x = median, xend= median,
                                         y = ymin, yend = ymax,
                                         color = type2), linetype = "dashed", size = 0.6) +
  
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-4,4,1),
                     name = "Density") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = round(seq(-1.5,1.5,0.5),1),
                     name = "Transferability") +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81")) +
  
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        strip.background = element_blank(), strip.text = element_blank()) +
  coord_flip(xlim=c(-1.32, 0.95), ylim = c(-4,4))



d <- density(model_performance_relchg$rel_chg_tss, na.rm = TRUE)
range(d$x)
