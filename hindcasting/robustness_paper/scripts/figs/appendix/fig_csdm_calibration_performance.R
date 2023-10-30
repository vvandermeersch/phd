# Appendix figure: cSDM calibration

dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv"
models <- c("brt", "gam", "lasso_glm","random_forest")
species <- c("fagus_sylvatica", "abies_alba", "quercus_robur", "quercus_petraea", "quercus_pubescens", "quercus_ilex")

csdm_calibration <- lapply(models,function(m){
  
  species_auc_test <- lapply(species, function(s){
    
    calibration_info <- readRDS(file.path(dir, m, "fit", s, paste0(m, "_finalcov_fullmodel.rds")))
    return(data.frame("cv_auc" = calibration_info$auc_test[1,4], "species" = s))
  
  })
  species_auc <- do.call(rbind.data.frame, species_auc_test)
  species_auc$model <- m
  return(species_auc)
})
csdm_calibration_performance <- do.call(rbind.data.frame, csdm_calibration)
csdm_calibration_performance$model <- factor(csdm_calibration_performance$model)
levels(csdm_calibration_performance$model) <- list(BRT="brt", GAM="gam", GLM="lasso_glm", RF="random_forest")

csdm_cal_perf_boxplot <- ggplot(data = csdm_calibration_performance) +
  geom_boxplot(aes(y = cv_auc, x = model), col = "#e86117", fill = "#e86117", alpha = 0.3, width=0.5) +
  theme_bw() + 
  labs(y="AUC", x=NULL) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 12),
        legend.position="bottom", legend.title=element_blank(),
        plot.margin = unit(c(0,-0.1,0.5,0), "cm"))
