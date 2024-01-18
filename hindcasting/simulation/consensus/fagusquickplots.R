models <- c("phenofit", "phenofitfitted", "castanea", "castaneafitted",
            "brt", "randomforest", "gam", "lassoglm")
species <- c("fagus_sylvatica")






plots <- lapply(species, function(s){
  cowplot::plot_grid(plotlist = unlist(lapply(models, function(m){
                       lapply(c(11500, 11000, 8000, 1000), function(y){
                         if(m %in% c("castanea", "castaneafitted")){
                           out <- readRDS(file.path(get(paste0(m, "_simdir_", s)), paste0(y,"BP.rds")))
                         }else{out <- readRDS(file.path(get(paste0(m, "_simdir")), s, paste0(y,"BP.rds")))}
                         
                         
                         out$pred <- ifelse(out$pred < get(paste0(m, "_", s)), 0, 1)
                         ggplot(data = out, aes(x = lon, y = lat, fill = as.factor(pred))) +
                           geom_raster() +
                           scale_fill_manual(values = c("grey", "darkgreen")) +
                           theme_void() +
                           ggtitle(paste(m, y)) + 
                           theme(legend.position = 'none')
                         
                       })
                     }), recursive = F), ncol = 5)
})


ggsave(filename= file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/consensus", "fsylvatica_quickplot.pdf"), 
       plot= plots[[1]], height=35, width=14)
