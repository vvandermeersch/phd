
#----------------------------#
# Evaluate model performance #
#----------------------------#

library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)

pred_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

.round_acc = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

.get_response_curve <- function(data, variable, calibration_data = NULL, fullname, round_factor = 0.1){
  
  data_plot <- data.frame(model = data$mod, var = data[,variable], fitness = data$fitness)
  data_plot$var <- .round_acc(data_plot$var, round_factor)
  data_plot <- data_plot %>%
    dplyr::group_by(model, var) %>% 
    dplyr::summarise(fitness = median(fitness))
  
  plot <- ggplot(data = data_plot, aes(x = var, y = fitness, col = model)) + 
    geom_line() +
    theme_minimal() +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                       name = "Fitness") +
    scale_x_continuous(name = fullname) +
    scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                       values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
          axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
          legend.position="bottom", legend.title=element_blank())
  
  if(!is.null(calibration_data)){
    plot <- plot + 
      geom_vline(xintercept = min(calibration_data[,variable]),linetype="dashed", color = "red", linewidth = 0.7) +
      geom_vline(xintercept = max(calibration_data[,variable]), linetype="dashed", color = "red", linewidth = 0.7)
  }
  
  return(plot)
  
}



model_response <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(seq(500,9000,500), function(year){
    
    fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    predictors <- readRDS(file.path(pred_folder, paste0("predictors_", year, "BP.rds")))
  
    fitness <- left_join(fitness, predictors, by = c("lat", "lon"))
    
    return(data.frame(year = year, mod = mod$name,
                      fitness = fitness$pred, 
                      bio6 = fitness$bio6,
                      bio12 = fitness$bio12,
                      sum_apsep_GDD5 = fitness$sum_apsep_GDD5,
                      w_bal = fitness$w_bal)
           )})
  return(do.call(rbind.data.frame, perf))}
)

model_response <- do.call(rbind.data.frame, model_response)

# test <- model_performance[model_performance$mod == "Lasso GLM",]
# data_plot <- test[order(test$bio6),]
# #data_plot$group <- cut(seq_along(data_plot$bio12), 100, labels = FALSE)
# data_plot$group <- as.numeric(cut_interval(data_plot$bio6,100))
# 
# ggplot(data = data_plot, aes(x = bio6, y = fitness, group = group)) + 
#   geom_boxplot(notch=FALSE, outlier.shape=NA, fill="red", alpha=0.2, coef = 0) +
#   theme_minimal()


bio6_plot <- .get_response_curve(model_response, variable = "bio6", 
                                 calibration_data = calibration_predictors,
                                 fullname = "Min. temperature of coldest month", 
                                 round_factor = 0.5)

bio12_plot <- .get_response_curve(model_response, variable = "bio12", 
                                  calibration_data = calibration_predictors,
                                  fullname = "Annual precipitation", 
                                  round_factor = 100)

wbal_plot <- .get_response_curve(model_response, variable = "w_bal",
                                 calibration_data = calibration_predictors,
                                 fullname = "Annual water balance", 
                                 round_factor = 20)

gdd_plot <- .get_response_curve(model_response, variable = "sum_apsep_GDD5", 
                                calibration_data = calibration_predictors,
                                fullname = "Sum of GDD (>5°C, April-Sept.)", 
                                round_factor = 100)


response_figure <- plot_grid(
  bio6_plot + theme(legend.position = 'none'), bio12_plot + theme(legend.position = 'none'),
  wbal_plot + theme(legend.position = 'none'), gdd_plot + theme(legend.position = 'none'),
  ncol = 2,
  align = "v"
) 

response_figure <- plot_grid(
  response_figure,
  get_legend(gdd_plot),
  ncol = 1,
  rel_heights = c(1,0.1)
)
  

