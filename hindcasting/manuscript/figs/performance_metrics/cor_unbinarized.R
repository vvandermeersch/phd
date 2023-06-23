
model_performance <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(years, function(year){
    print(year)
    fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    pollen <- readRDS(file.path(pollen_folder, paste0("prop_", year, "BP.rds")))
    
    fitness <- inner_join(fitness, pollen, by = c("lat", "lon"))
    
    spearman <- cor(scale(fitness$pred), scale(fitness$prop), method = "spearman")
    pearson <- cor(scale(fitness$pred), scale(fitness$prop), method = "pearson")
    
    # binarized predictions
    ths <- readRDS(file.path(mod$modfolder, mod$mod))$best_threshold
    fitness$bin_pred <- 0
    fitness[fitness$pred >= ths, "bin_pred"] <- 1
    
    eval_obj <- evalmod(scores = fitness$prop, labels = fitness$bin_pred)
    aucroc <- precrec::auc(eval_obj)
    
    return(data.frame(year = year, 
                      spearman = spearman, 
                      pearson = pearson,
                      inv_auc = round(aucroc$aucs[1],2),
                      npollen =length(which(!is.na(fitness$prop))), mod = mod$name))})
  return(do.call(rbind.data.frame, perf))}
)
model_performance <- do.call(rbind.data.frame, model_performance)

spearman_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = spearman, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=0.25, ymax=0.5, alpha=0.05, fill=NA) + # just a trick
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Spearman correlation") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

pearson_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = pearson, col = mod)) +
  #geom_point(aes(x = year, y = sorensen, col = mod)) +
  annotate("rect", xmin=0, xmax=max(years), ymin=0.25, ymax=0.5, alpha=0.05, fill=NA) + # just a trick
  scale_x_reverse(breaks = seq(0,max(years),brk),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "Pearson correlation") +
  scale_color_manual(breaks= c('Lasso GLM', 'GAM', 'Random Forest', "BRT", "PHENOFIT", "PHENOFIT (fitted)"),
                     values= c("#e86117","#f9844a", "#5ab078", "#b5e48c", "#457b9d", "#82BCC4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())
