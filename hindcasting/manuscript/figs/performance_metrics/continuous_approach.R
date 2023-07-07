
#----------------------------#
# Evaluate model performance #
#----------------------------#

library(ecospat)
library(dplyr)
library(precrec)
library(ggplot2)
library(modEvA)
library(terra)
library(cowplot)


model_performance <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(years, function(year){
    print(year)
    
    if(combine_models){
      fitness1 <- readRDS(file.path(mod$simfolder1, paste0(year, "BP.rds")))
      fitness2 <- readRDS(file.path(mod$simfolder2, paste0(year, "BP.rds")))
      fitness3 <- readRDS(file.path(mod$simfolder3, paste0(year, "BP.rds")))
      fitness4 <- readRDS(file.path(mod$simfolder4, paste0(year, "BP.rds")))
      fitness <- data.frame(lat = fitness1$lat, lon = fitness1$lon, 
                            pred = rowMeans(cbind(fitness1$pred,fitness2$pred, fitness3$pred, fitness4$pred)))
    }else{
      
      fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
      
    }
    
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
    
    fitness$lat <- round(fitness$lat,2)
    fitness$lon <- round(fitness$lon,2)
    fitness <- left_join(fitness, pollen, by = c("lat", "lon"))
    
    # boyce index
    # boyce_ind <- ecospat.boyce(fit = fitness$pred, obs = na.omit(fitness[fitness$pres == 1,]$pred), 
    #                                   nclass=0, window.w="default", res=100, 
    #                                   PEplot = FALSE, rm.duplicate = TRUE,  method = 'spearman' )$cor
    
    boyce_ind <- as.numeric(Boyce(obs = fitness[fitness$pres == 1, c("lon", "lat")], pred = rast(fitness[,c(2,1,3)]), 
                                  main = "Boyce index", plot= FALSE,
                                  res = 100)$Boyce)
  
    
    # binarized predictions
    if(combine_models){
      ths <- readRDS(file.path(mod$modfolder1, mod$mod1))$best_threshold
      fitness1$bin_pred <- 0
      fitness1[fitness1$pred >= ths, "bin_pred"] <- 1
      
      ths <- readRDS(file.path(mod$modfolder2, mod$mod2))$best_threshold
      fitness2$bin_pred <- 0
      fitness2[fitness2$pred >= ths, "bin_pred"] <- 1
      
      ths <- readRDS(file.path(mod$modfolder3, mod$mod3))$best_threshold
      fitness3$bin_pred <- 0
      fitness3[fitness3$pred >= ths, "bin_pred"] <- 1
      
      ths <- readRDS(file.path(mod$modfolder4, mod$mod4))$best_threshold
      fitness4$bin_pred <- 0
      fitness4[fitness4$pred >= ths, "bin_pred"] <- 1
      
      fitness$bin_pred <- matrixStats::rowMaxs(cbind(fitness1$bin_pred,fitness2$bin_pred, fitness3$bin_pred, fitness4$bin_pred))
      fitness[fitness$bin_pred > 0, "bin_pred"] <- 1
   
    }else{
      
      ths <- readRDS(file.path(mod$modfolder, mod$mod))$best_threshold
      fitness$bin_pred <- 0
      fitness[fitness$pred >= ths, "bin_pred"] <- 1
      
    }
    
    # proportion of suitable area
    prop_suit <- nrow(fitness[fitness$bin_pred == 1,])/nrow(fitness)
    
    # auc
    fitness <- na.omit(fitness)
    eval_obj <- evalmod(scores = fitness$pred, labels = fitness$pres)
    aucroc <- precrec::auc(eval_obj)
    
    # confusion matrix
    tp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 1,])
    fp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 0,])
    tn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 0,])
    fn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 1,])
    
    # metrics
    sens = tp/(tp+fn)
    spec = tn/(tn+fp)
    opr = fp/(tp+fp) # overprediction rate
    upr = fn/(tp+fn) # underprediction rate
    tss = sens + spec - 1
    sorensen = 2*tp/(fn + 2*tp + fp)
  
    return(data.frame(year = year, 
                      bi = boyce_ind, auc = round(aucroc$aucs[1],5), 
                      sens = sens, spec= spec, opr = opr, upr = upr, tss = tss,
                      sorensen = sorensen,
                      prop_suit = prop_suit,
                      npollen_pres = length(which(fitness$pres==1)), 
                      npollen =length(which(!is.na(fitness$pres))), 
                      mod = mod$name, type = mod$type))})
  return(do.call(rbind.data.frame, perf))}
)



model_performance <- do.call(rbind.data.frame, model_performance)


# model_performance_1518k <- lapply(1:nrow(models),function(i){
#   mod <- models[i,]
#   
#   fitness <- lapply(seq(15000,18000,500), function(year) readRDS(file.path(mod$simfolder, paste0(year, "BP.rds"))))
#   fitness <- do.call(rbind.data.frame, fitness)
#   fitness$lat <- round(fitness$lat, 2)
#   fitness$lon <- round(fitness$lon, 2)
#   fitness <- fitness %>% group_by(lat, lon) %>% dplyr::summarize(pred = mean(pred))
#   
#   pollen <- lapply(seq(15000,18000,500), function(year) readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds"))))
#   pollen <- do.call(rbind.data.frame, pollen)
#   pollen <- pollen %>% group_by(lat, lon) %>% dplyr::summarize(pres = max(pres))
#   
#   fitness <- left_join(fitness, pollen, by = c("lat", "lon"))
#   
#   # boyce index
#   # boyce_ind <- ecospat.boyce(fit = fitness$pred, obs = na.omit(fitness[fitness$pres == 1,]$pred), 
#   #                                   nclass=0, window.w="default", res=100, 
#   #                                   PEplot = FALSE, rm.duplicate = TRUE,  method = 'spearman' )$cor
#   
#   boyce_ind <- as.numeric(Boyce(obs = fitness[fitness$pres == 1, c("lon", "lat")], pred = rast(fitness[,c(2,1,3)]), 
#                                 main = "Boyce index", plot= FALSE,
#                                 res = 100)$Boyce)
#   
#   # auc
#   fitness <- na.omit(fitness)
#   eval_obj <- evalmod(scores = fitness$pred, labels = fitness$pres)
#   aucroc <- precrec::auc(eval_obj)
#   
#   # binarized predictions
#   ths <- readRDS(file.path(mod$modfolder, mod$mod))$best_threshold
#   fitness$bin_pred <- 0
#   fitness[fitness$pred >= ths, "bin_pred"] <- 1
# 
#   
#   # confusion matrix
#   tp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 1,])
#   fp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 0,])
#   tn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 0,])
#   fn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 1,])
#   
#   # metrics
#   sens = tp/(tp+fn)
#   spec = tn/(tn+fp)
#   opr = fp/(tp+fp) # overprediction rate
#   upr = fn/(tp+fn) # underprediction rate
#   tss = sens + spec - 1
#   sorensen = 2*tp/(fn + 2*tp + fp)
#   
#   perf <- data.frame(year = 15000, 
#                      bi = boyce_ind, auc = round(aucroc$aucs[1],5), 
#                      sens = sens, spec= spec, opr = opr, upr = upr, tss = tss,
#                      sorensen = sorensen,
#                      npollen_pres = length(which(fitness$pres==1)), 
#                      npollen =length(which(!is.na(fitness$pres))), 
#                      mod = mod$name, type = mod$type)
#     
#   return(perf)}
# )
# model_performance_1518k <- do.call(rbind.data.frame, model_performance_1518k)





auc_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = auc, col = mod)) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=min(model_performance$auc)-0.01, ymax=0.5, alpha=0.05, fill="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "AUC") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                       values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

tss_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = tss, col = mod)) +
  #geom_point(aes(x = year, y = tss, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=0, ymax=min(model_performance$tss)-0.01, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") + 
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "TSS") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4","#995d81", "#c29ab2")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

boyceindex_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = bi, col = mod)) +
  #geom_point(aes(x = year, y = bi, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=0, ymax=min(model_performance$bi, na.rm = T)-0.01, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "Boyce index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81", "#c29ab2")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

sorensen_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = sorensen, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=0.25, ymax=0.5, alpha=0.05, fill=NA) + # just a trick
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Sørensen index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81", "#c29ab2")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

suitability_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = prop_suit, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=0.25, ymax=0.5, alpha=0.05, fill=NA) + # just a trick
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Prop. of predicted suitable area") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81", "#c29ab2")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


# coeff <- 2
# ggplot(data = model_performance) +
#   geom_line(aes(x = year, y = npollen)) +
#   geom_line(aes(x = year, y = npollen_pres*coeff)) +
#   scale_x_reverse(breaks = seq(0,15000,2000),
#                   expand = c(0, 500),
#                   name = "YEARS (BP)") +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
#                      name = "Number of pollen data",
#                      sec.axis = sec_axis(~./coeff, name="Number of pollen presences")) +
#   theme_bw() + 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
#         axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
#         legend.position="bottom", legend.title=element_blank())

pollen_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = npollen), col= "#6867AC") +
  annotate("rect", xmin=0, xmax=max(years), ymin=400, ymax=500, alpha=0.05, fill=NA) + # just a trick
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "No. of\npollen records",
                     ) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

presence_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = npollen_pres), col= "#CE7BB0") +
  annotate("rect", xmin=0, xmax=max(years), ymin=100, ymax=150, alpha=0.05, fill=NA) + # just a trick
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "No. of\npresence",
  ) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())



performance_figure <- plot_grid(
  pollen_plot, presence_plot,
  auc_plot + theme(legend.position = 'none'), tss_plot + theme(legend.position = 'none'),
  boyceindex_plot + theme(legend.position = 'none'), suitability_plot + theme(legend.position = 'none'),
  ncol = 2,
  align = "v",
  rel_heights = c(0.4,1,1)
) 

performance_figure <- plot_grid(
  performance_figure,
  get_legend(auc_plot),
  ncol = 1,
  rel_heights = c(1,0.1)
)












model_performance <- left_join(model_performance, burke_climatenovelty, by = c("year"))

auc_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = auc, col = mod)) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  annotate("rect", xmin=min(model_performance$median), xmax=max(model_performance$median), ymin=0.35, ymax=0.5, alpha=0.05, fill="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_continuous(breaks = seq(1.15,2.05,0.1),
                  expand = c(0, 0),
                  name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "AUC") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4","#995d81", "#c29ab2")) +
  scale_fill_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

tss_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = tss, col = mod)) +
  #geom_point(aes(x = year, y = tss, col = mod)) +
  annotate("rect", xmin=min(model_performance$median), xmax=max(model_performance$median), ymin=0, ymax=-0.25, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(1.15,2.05,0.1),
                  expand = c(0, 0),
                  name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "TSS") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  scale_fill_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                    values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

boyceindex_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = bi, col = mod)) +
  #geom_point(aes(x = year, y = bi, col = mod)) +
  annotate("rect", xmin=min(model_performance$mean), xmax=max(model_performance$mean), ymin=0, ymax=-0.75, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(1.15,2.05,0.1),
                  expand = c(0, 0),
                  name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "Boyce index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4","#995d81", "#c29ab2")) +
  scale_fill_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                    values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

sorensen_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = sorensen, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  scale_x_reverse(breaks = seq(1.15,2.05,0.1),
                  expand = c(0, 0),
                  name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Sørensen index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4","#995d81", "#c29ab2")) +
  scale_fill_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                    values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4", "#995d81","#c29ab2")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


performance_figure_climdistance <- plot_grid(
  auc_plot + theme(legend.position = 'none'), tss_plot + theme(legend.position = 'none'),
  boyceindex_plot + theme(legend.position = 'none'), sorensen_plot + theme(legend.position = 'none'),
  ncol = 2,
  align = "v"
)

performance_figure_climdistance <- plot_grid(
  performance_figure_climdistance,
  get_legend(auc_plot),
  ncol = 1,
  rel_heights = c(1,0.1)
)


model_performance <- left_join(model_performance, clim_hypv_similarity, by = c("year"))

sorensen_hpv <- ggplot(data = model_performance) +
  geom_line(aes(x = hypv_sorensen, y = sorensen, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  scale_x_reverse(breaks = seq(0.5,0.8,0.01),
                  expand = c(0, 0),
                  name = "Sørensen hypervolume similarity") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Sørensen index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4","#995d81", "#c29ab2")) +
  scale_fill_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                    values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4","#995d81","#c29ab2")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())




auc_plot <- ggplot(data = model_performance) +
  geom_point(aes(x = median, y = auc, col = type), size = 0.7) +
  geom_smooth(aes(x = median, y = auc, col = type, fill = type), method = "loess", alpha = 0.3, size = 0.6) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  annotate("rect", xmin=min(model_performance$median), xmax=max(model_performance$median), ymin=min(model_performance$auc), ymax=0.5, alpha=0.05, fill="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(0.9,1.85,0.1),
                     expand = c(0, 0),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "AUC") +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81", "#c29ab2")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81", "#c29ab2")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


tss_plot <- ggplot(data = model_performance) +
  geom_point(aes(x = median, y = tss, col = type), size = 0.7) +
  geom_smooth(aes(x = median, y = tss, col = type, fill = type), method = "loess", alpha = 0.3, size = 0.6) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  annotate("rect", xmin=min(model_performance$median), xmax=max(model_performance$median), ymin=0, ymax=min(model_performance$tss), alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(0.9,1.85,0.1),
                     expand = c(0, 0),
                     name = "Climate novelty") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "TSS") +
  scale_color_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                     values= c("#e86117", "#457b9d", "#82BCC4", "#995D81", "#c29ab2")) +
  scale_fill_manual(breaks= c('cSDM', "PHENOFIT", "PHENOFIT (fitted)", "CASTANEA", "CASTANEA (fitted)"),
                    values= c("#e86117", "#457b9d", "#82BCC4", "#995D81", "#c29ab2")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


performance_figure_summary <- plot_grid(
  auc_plot + theme(legend.position = 'none'), tss_plot + theme(legend.position = 'none'),
  ncol = 2,
  align = "v"
)

performance_figure_summary <- plot_grid(
  performance_figure_summary,
  get_legend(auc_plot),
  ncol = 1,
  rel_heights = c(1,0.1)
)


