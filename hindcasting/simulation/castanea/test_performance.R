
years <- c(seq(1000,11000, 500))
brk <- 2000
combine_models <- FALSE
pollen_folder <- "D:/species/pollen/processed/abies/025deg/001thr_500yrunc"
models <- data.frame(name = c("CASTANEA"),
                     type = c("CASTANEA"),
                     simfolder = c("D:/simulations/castanea/paleo/expert/025deg/abies_alba/24yr_inventory_modcode/NPP"),
                     modfolder = c("C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/expert/fagus_sylvatica"),
                     mod = c("fagus_sylvatica_280ppm.rds"))
source(file.path(wd, "performance_metrics", "continuous_approach.R"))



ggplot(data = model_performance[model_performance$mod == "CASTANEA",]) +
  geom_point(aes(x = median, y = auc, col = type), size = 0.7) +
  geom_smooth(aes(x = median, y = auc, col = type, fill = type, linetype = type), method = "loess", alpha = 0.3, size = 0.6) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  # annotate("rect", xmin=min(model_performance$median), xmax=max(model_performance$median), ymin=0.37, ymax=0.5, alpha=0.05, fill="red") +
  # geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.5) +  # annotate("rect", xmin=min(model_performance$median), xmax=max(model_performance$median), ymin=0.37, ymax=0.5, alpha=0.05, fill="red") +
  # geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.5) +
  # scale_x_continuous(breaks = seq(0.9,1.85,0.1),
  scale_x_continuous(breaks = seq(0.9,1.85,0.1),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "AUC") +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)", "CASTANEA - CO2 fixed", "CASTANEA (fitted) - CO2 fixed"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81", "#c29ab2",  "#995D81", "#c29ab2")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)", "CASTANEA - CO2 fixed", "CASTANEA (fitted) - CO2 fixed"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81", "#c29ab2", "#995D81", "#c29ab2")) +
  scale_linetype_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)", "CASTANEA - CO2 fixed", "CASTANEA (fitted) - CO2 fixed"),
                        values= c("solid", "solid", "solid", "solid", "solid", "dashed", "dashed")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

model_performance[model_performance$mod == "CASTANEA",]
mean(model_performance[model_performance$mod == "CASTANEA", "auc"])
median(model_performance[model_performance$mod == "CASTANEA", "auc"])
