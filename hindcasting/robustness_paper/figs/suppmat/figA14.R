castanea_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/expert",
            c("abies_alba/abies_alba_240ppm.rds",
              "fagus_sylvatica/fagus_sylvatica_240ppm.rds", 
              "quercus_robur/quercus_robur_240ppm.rds", 
              "quercus_petraea/quercus_petraea_240ppm.rds", 
              "quercus_ilex/quercus_ilex_240ppm_frostVV.rds"))
castanea <- 
  data.frame(sorensen = as.numeric(sapply(castanea_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(castanea_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(castanea_fit_files, function(i) readRDS(i)$auc_all)),
             model = "CASTANEA",
             type = "3Expertprocessbased", 
             test = "1Observedreference")

castaneafitted_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/castanea/fit/fitted",
            c("abies_alba/abies_alba_240ppm.rds",
              "fagus_sylvatica/fagus_sylvatica_240ppm.rds", 
              "quercus_robur/quercus_robur_240ppm.rds", 
              "quercus_petraea/quercus_petraea_240ppm.rds", 
              "quercus_ilex/quercus_ilex_240ppm_frostVV.rds"))
castaneafitted <- 
  data.frame(sorensen = as.numeric(sapply(castaneafitted_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(castaneafitted_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(castaneafitted_fit_files, function(i) readRDS(i)$auc_all)),
             model = "CASTANEA (fitted)",
             type = "2Fittedprocessbased", 
             test = "1Observedreference")

phenofit_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert",
            c("abies_alba/Abies_alba_VVanderMeersch2.rds",
              "fagus_sylvatica/Fagus_sylvatica_VVanderMeersch.rds", 
              "quercus_robur/Quercus_robur_ADuputie_Chuine.rds", 
              "quercus_petraea/Quercus_petraea_VanderMeersch2023_Chuine.rds", 
              "quercus_ilex/Quercus_ilex_FTauc_ptype3.rds"))
phenofit <- 
  data.frame(sorensen = as.numeric(sapply(phenofit_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(phenofit_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(phenofit_fit_files, function(i) readRDS(i)$auc_all)),
             model = "PHENOFIT",
             type = "3Expertprocessbased", 
             test = "1Observedreference")

phenofitfitted_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted",
            c("abies_alba/cmaes_fit_subset2_rep5.rds",
              "fagus_sylvatica/cmaes_fit_subset4_rep1.rds", 
              "quercus_robur/cmaes_fit_subset1_rep1.rds", 
              "quercus_petraea/cmaes_fit_subset1_rep2.rds", 
              "quercus_ilex/cmaes_fit_subset1_rep1.rds"))
phenofitfitted <- 
  data.frame(sorensen = as.numeric(sapply(phenofitfitted_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(phenofitfitted_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(phenofitfitted_fit_files, function(i) readRDS(i)$auc_all)),
             model = "PHENOFIT (fitted)",
             type = "2Fittedprocessbased", 
             test = "1Observedreference")

glm_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit",
            c("abies_alba/lasso_glm_finalcov_fullmodel.rds",
              "fagus_sylvatica/lasso_glm_finalcov_fullmodel.rds", 
              "quercus_robur/lasso_glm_finalcov_fullmodel.rds", 
              "quercus_petraea/lasso_glm_finalcov_fullmodel.rds", 
              "quercus_ilex/lasso_glm_finalcov_fullmodel.rds"))
glm <- 
  data.frame(sorensen = as.numeric(sapply(glm_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(glm_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(glm_fit_files, function(i) readRDS(i)$auc_all[1,4])),
             model = "Lasso GLM",
             type = "1Correlative", 
             test = "1Observedreference")
glmcv <- 
  data.frame(sorensen = as.numeric(sapply(glm_fit_files, function(i) readRDS(i)$sorensen_test)),
             tss = as.numeric(sapply(glm_fit_files, function(i) readRDS(i)$tss_test)),
             auc = as.numeric(sapply(glm_fit_files, function(i) readRDS(i)$auc_test[1,4])),
             model = "Lasso GLM",
             type = "1Correlative", 
             test = "2Crossvalidationreference")

gam_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit",
            c("abies_alba/gam_finalcov_fullmodel.rds",
              "fagus_sylvatica/gam_finalcov_fullmodel.rds", 
              "quercus_robur/gam_finalcov_fullmodel.rds", 
              "quercus_petraea/gam_finalcov_fullmodel.rds", 
              "quercus_ilex/gam_finalcov_fullmodel.rds"))
gam <- 
  data.frame(sorensen = as.numeric(sapply(gam_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(gam_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(gam_fit_files, function(i) readRDS(i)$auc_all[1,4])),
             model = "GAM",
             type = "1Correlative", 
             test = "1Observedreference")
gamcv <- 
  data.frame(sorensen = as.numeric(sapply(gam_fit_files, function(i) readRDS(i)$sorensen_test)),
             tss = as.numeric(sapply(gam_fit_files, function(i) readRDS(i)$tss_test)),
             auc = as.numeric(sapply(gam_fit_files, function(i) readRDS(i)$auc_test[1,4])),
             model = "GAM",
             type = "1Correlative", 
             test = "2Crossvalidationreference")

brt_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit",
            c("abies_alba/brt_finalcov_fullmodel.rds",
              "fagus_sylvatica/brt_finalcov_fullmodel.rds", 
              "quercus_robur/brt_finalcov_fullmodel.rds", 
              "quercus_petraea/brt_finalcov_fullmodel.rds", 
              "quercus_ilex/brt_finalcov_fullmodel.rds"))
brt <- 
  data.frame(sorensen = as.numeric(sapply(brt_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(brt_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(brt_fit_files, function(i) readRDS(i)$auc_all[1,4])),
             model = "BRT",
             type = "1Correlative", 
             test = "1Observedreference")
brtcv <- 
  data.frame(sorensen = as.numeric(sapply(brt_fit_files, function(i) readRDS(i)$sorensen_test)),
             tss = as.numeric(sapply(brt_fit_files, function(i) readRDS(i)$tss_test)),
             auc = as.numeric(sapply(brt_fit_files, function(i) readRDS(i)$auc_test[1,4])),
             model = "BRT",
             type = "1Correlative", 
             test = "2Crossvalidationreference")

rf_fit_files <- 
  file.path("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit",
            c("abies_alba/random_forest_finalcov_fullmodel.rds",
              "fagus_sylvatica/random_forest_finalcov_fullmodel.rds", 
              "quercus_robur/random_forest_finalcov_fullmodel.rds", 
              "quercus_petraea/random_forest_finalcov_fullmodel.rds", 
              "quercus_ilex/random_forest_finalcov_fullmodel.rds"))
rf <- 
  data.frame(sorensen = as.numeric(sapply(rf_fit_files, function(i) readRDS(i)$sorensen_all)),
             tss = as.numeric(sapply(rf_fit_files, function(i) readRDS(i)$tss_all)),
             auc = as.numeric(sapply(rf_fit_files, function(i) readRDS(i)$auc_all[1,4])),
             model = "Random Forest",
             type = "1Correlative", 
             test = "1Observedreference")
rfcv <- 
  data.frame(sorensen = as.numeric(sapply(rf_fit_files, function(i) readRDS(i)$sorensen_test)),
             tss = as.numeric(sapply(rf_fit_files, function(i) readRDS(i)$tss_test)),
             auc = as.numeric(sapply(rf_fit_files, function(i) readRDS(i)$auc_test[1,4])),
             model = "Random Forest",
             type = "1Correlative", 
             test = "2Crossvalidationreference")

historical_performances <- rbind(
  phenofit, phenofitfitted, castanea, castaneafitted,
  glm, gam, rf, brt,
  glmcv, gamcv, rfcv, brtcv
)

historical_performances$type_test <- paste0(historical_performances$type, historical_performances$test)
historical_performances[, "sign"] <- NA
historical_performances[, "sign_position"] <- NA
historical_performances[historical_performances$test == "2Crossvalidationreference", "sign"] <- "CV"
historical_performances[historical_performances$test == "2Crossvalidationreference", "sign_position"] <- 0.52

boxplot_sorensen <- ggplot(historical_performances) +
  geom_boxplot(aes(x=type, y=sorensen, col = type, fill = type, group = type_test),  
               position = position_dodge2(preserve = "single"),
               alpha = 0.3, outlier.size = 0.1) +
  geom_text(data = unique(historical_performances[,c("type_test", "sign_position", "sign", "type")]), 
            aes(x = type, label = sign, y = sign_position, group = type_test), 
            vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.3, position=position_dodge(width=0.75)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0.3,1,0.1),
                     name = "Sorensen index") +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9)) +
  coord_cartesian(ylim=c(0.3, 1), clip = "off")

historical_performances[historical_performances$test == "2Crossvalidationreference", "sign_position"] <- 0.5
boxplot_tss <- ggplot(historical_performances) +
  geom_boxplot(aes(x=type, y=tss, col = type, fill = type, group = type_test),  
               position = position_dodge2(preserve = "single"),
               alpha = 0.3, outlier.size = 0.1) +
  geom_text(data = unique(historical_performances[,c("type_test", "sign_position", "sign", "type")]), 
            aes(x = type, label = sign, y = sign_position, group = type_test), 
            vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.3, position=position_dodge(width=0.75)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0.3,1,0.1),
                     name = "TSS") +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9)) +
  coord_cartesian(ylim=c(0.3, 1), clip = "off")

historical_performances[historical_performances$test == "2Crossvalidationreference", "sign_position"] <- 0.8
boxplot_auc <- ggplot(historical_performances) +
  geom_boxplot(aes(x=type, y=auc, col = type, fill = type, group = type_test),  
               position = position_dodge2(preserve = "single"),
               alpha = 0.3, outlier.size = 0.1) +
  geom_text(data = unique(historical_performances[,c("type_test", "sign_position", "sign", "type")]), 
            aes(x = type, label = sign, y = sign_position, group = type_test), 
            vjust = 0, inherit.aes = F,
            family = "Helvetica Narrow", size = 2.3, position=position_dodge(width=0.75)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0.3,1,0.05),
                     name = "AUC") +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("Correlative", "Fitted process-based", "Expert process-based")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.title=element_blank(), legend.position = "bottom",
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9)) +
  coord_cartesian(ylim=c(0.7, 1), clip = "off")


figA14_main <-
  plot_grid(
  plot_grid(boxplot_sorensen, boxplot_tss, boxplot_auc + theme(legend.position="none"), ncol = 3,
            labels = c("a", "b", "c"), 
            label_fontfamily = "Helvetica Narrow", label_size = 11),
  get_legend(boxplot_auc), ncol = 1, rel_heights = c(1,.15)
  )
