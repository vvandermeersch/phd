
#----------------------------#
# Evaluate model performance #
#----------------------------#

library(ecospat)
library(dplyr)
library(precrec)
library(ggplot2)
library(modEvA)
library(terra)



model_performance <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(seq(500,9000,500), function(year){
    fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
  
    fitness <- left_join(fitness, pollen, by = c("lat", "lon"))
    
    # boyce index
    # boyce_ind <- ecospat.boyce(fit = fitness$pred, obs = na.omit(fitness[fitness$pres == 1,]$pred), 
    #                                   nclass=0, window.w="default", res=100, 
    #                                   PEplot = FALSE, rm.duplicate = TRUE,  method = 'spearman' )$cor
    
    boyce_ind <- as.numeric(Boyce(obs = fitness[fitness$pres == 1, c("lon", "lat")], pred = rast(fitness[,c(2,1,3)]), 
                                  main = "Boyce index", plot= FALSE,
                                  res = 100)$Boyce)
  
    # auc
    fitness <- na.omit(fitness)
    eval_obj <- evalmod(scores = fitness$pred, labels = fitness$pres)
    aucroc <- precrec::auc(eval_obj)
    
    # binarized predictions
    ths <- readRDS(file.path(mod$modfolder, mod$mod))$best_threshold
    fitness$bin_pred <- 0
    fitness[fitness$pred >= ths, "bin_pred"] <- 1
    
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
                      bi = boyce_ind, auc = round(aucroc$aucs[1],2), 
                      sens = sens, spec= spec, opr = opr, upr = upr, tss = tss,
                      sorensen = sorensen,
                      npollen_pres = nrow(pollen[pollen$pres == 1,]), 
                      npollen = nrow(pollen), mod = mod$name))})
  return(do.call(rbind.data.frame, perf))}
)

model_performance <- do.call(rbind.data.frame, model_performance)

auc_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = auc, col = mod)) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  annotate("rect", xmin=0, xmax=9500, ymin=0.25, ymax=0.5, alpha=0.05, fill="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(0,9000,1500),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "AUC") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                       values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

tss_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = tss, col = mod)) +
  #geom_point(aes(x = year, y = tss, col = mod)) +
  annotate("rect", xmin=0, xmax=9500, ymin=0, ymax=-0.25, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(0,9000,1500),
                  expand = c(0, 0),
                  name = "YEARS (BP)") + 
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "TSS") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

boyceindex_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = bi, col = mod)) +
  #geom_point(aes(x = year, y = bi, col = mod)) +
  annotate("rect", xmin=0, xmax=9500, ymin=0, ymax=-0.75, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(0,9000,1500),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "Boyce index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

sorensen_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = sorensen, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  scale_x_reverse(breaks = seq(0,9000,1500),
                  expand = c(0, 500),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Sørensen index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


performance_figure <- plot_grid(
  auc_plot + theme(legend.position = 'none'), tss_plot + theme(legend.position = 'none'),
  boyceindex_plot + theme(legend.position = 'none'), sorensen_plot + theme(legend.position = 'none'),
  ncol = 2,
  align = "v"
) 

performance_figure <- plot_grid(
  performance_figure,
  get_legend(auc_plot),
  ncol = 1,
  rel_heights = c(1,0.1)
)

model_performance <- left_join(model_performance, climatic_distance, by = c("year"))

auc_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = auc, col = mod)) +
  #geom_point(aes(x = year, y = auc, col = mod)) +
  annotate("rect", xmin=1.1, xmax=1.7, ymin=0.25, ymax=0.5, alpha=0.05, fill="red") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(1.2,1.8,0.2),
                  expand = c(0, 0),
                  name = "Median climatic distance") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "AUC") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

tss_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = tss, col = mod)) +
  #geom_point(aes(x = year, y = tss, col = mod)) +
  annotate("rect", xmin=1.1, xmax=1.7, ymin=0, ymax=-0.25, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(1.2,1.8,0.2),
                  expand = c(0, 0),
                  name = "Median climatic distance") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "TSS") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

boyceindex_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = bi, col = mod)) +
  #geom_point(aes(x = year, y = bi, col = mod)) +
  annotate("rect", xmin=1.1, xmax=1.7, ymin=0, ymax=-0.75, alpha=0.05, fill="red") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.7) +
  scale_x_reverse(breaks = seq(1.2,1.8,0.2),
                  expand = c(0, 0),
                  name = "Median climatic distance") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)),
                     name = "Boyce index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

sorensen_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = median, y = sorensen, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  scale_x_reverse(breaks = seq(1.2,1.8,0.2),
                  expand = c(0, 0),
                  name = "Median climatic distance") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Sørensen index") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
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


  

