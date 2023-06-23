
#--------------------------#
# Evaluate modelled niches #
#--------------------------#


pred_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

model_response <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(seq(500,9000,500), function(year){
    
    fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    predictors <- readRDS(file.path(pred_folder, paste0("predictors_", year, "BP.rds")))
    
    fitness <- left_join(fitness, predictors, by = c("lat", "lon"))
    
    return(data.frame(year = year, mod = mod$name,
                      lat = fitness$lat, lon = fitness$lon,
                      fitness = fitness$pred, 
                      bio6 = fitness$bio6,
                      bio12 = fitness$bio12,
                      sum_apsep_GDD5 = fitness$sum_apsep_GDD5,
                      w_bal = fitness$w_bal)
    )})
  return(do.call(rbind.data.frame, perf))}
)
model_response <- do.call(rbind.data.frame, model_response)

ggplot(data = model_response[model_response$mod == "PHENOFIT",], 
       aes(x = bio6, y = sum_apsep_GDD5, z = fitness)) +
  stat_summary_hex(fun = median) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 8),
        legend.title.align = 0.5, legend.position = "bottom",
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(1, 'cm')) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", 
                               frame.linewidth = 0.3, ticks = FALSE))
  
