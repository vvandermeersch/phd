models <- data.frame(name = 
                       c("BRT",
                         "Random Forest",
                         "GAM",
                         "GLM",
                         "PHENOFIT",
                         "CASTANEA",
                         "PHENOFIT (fitted)",
                         "CASTANEA (fitted)"),
                     simfolder = 
                       c("D:/simulations/csdm/brt/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                         "D:/simulations/csdm/random_forest/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from11750",
                         "D:/simulations/csdm/gam/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                         "D:/simulations/csdm/lasso_glm/paleo/migration/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                         "D:/simulations/phenofit/paleo/migration/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                         "D:/simulations/castanea/paleo/migration/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                         "D:/simulations/phenofit/paleo/migration/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000",
                         "D:/simulations/castanea/paleo/migration/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000"))

pollen_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc"
add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"

plotlist <- unlist(lapply(1:nrow(models), function(i){
  plots <- lapply(c(11750, 10500, 8000, 500), function(y){
      out <- crop(readRDS(file.path(models[i, "simfolder"], paste0(y,"BP.rds"))), ext(-10,30,34,66)) %>%
        as.data.frame(xy = TRUE)
      names(out) <- c("lon", "lat", "value")
      
      yr_ICE6G <- y%/%500*0.5
      ice_sheet <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), ext(-10,30,34,66))
      ice_sheet_pr <- ice_sheet
      ice_sheet[ice_sheet == 0] <- NA
      
      plot <- ggplot() +
        geom_raster(data = out, aes(x=lon, y=lat, fill = factor(value))) +
        scale_fill_manual(values = c("0" = "#ededed", "1" = "#F2B880", "2" = "#C98686")) +
        new_scale_fill() + 
        geom_spatraster(data = ice_sheet) +
        geom_spatraster_contour(data = ice_sheet_pr, linewidth = 0.2, breaks = c(90), col = "#a6bfe1") +
        scale_fill_gradient2(
          low = "#e3ebf5", 
          mid = "#e3ebf5", 
          high = "#e3ebf5", 
          na.value = NA,
          limits = c(0,100), 
          breaks = c(0,50,100), 
          midpoint = 50) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        coord_cartesian(xlim=c(-10, 30), ylim = c(34.5, 66), clip = "off") +
        theme_void() +
        theme(legend.position = "none",
              plot.margin = unit(c(0.2, 0.05, 0, 0.05), "cm"), 
              panel.border = element_rect(colour = "darkgrey", fill=NA, size=0.2))
      
      # add model name
      if(y == 11750){
        plot <- plot +
          annotate("text", x = -12.1, y = 50, label = models[i, "name"], color = "black",
                   family= "Helvetica", size = 3, angle = 90)
      }else{
        # add pollen points
        pollen <- readRDS(file.path(pollen_folder, paste0("pres_", y, "BP.rds")))
        add_pollen <- readRDS(file.path(add_pollen_folder, paste0("pres_", y, "BP.rds")))
        pollen <- dplyr::full_join(pollen, add_pollen, by = c("lat", "lon"))
        pollen$pres <- rowSums(pollen[, c("pres.x", "pres.y")], na.rm = T)
        pollen$pres <- ifelse(pollen$pres > 0, 1, 0)
        
        plot <- plot +
          geom_point(data = pollen[pollen$pres == 1 & pollen$lon < 29.9,], aes(x = lon, y = lat), size = 0.05)
      }
      
      # add years (only first line)
      if(models[i, "name"] == "BRT"){
        plot <- plot +
          annotate("text", x = 10, y = 68, label = paste0(y,"BP"), color = "black",
                   family= "Helvetica", size = 3)
      }
      
      return(plot)
    })
  
  return(plots)}), 
  recursive = F)

plotlist <- rlist::list.append(rep(NA,5),
                        NA, plotlist[1:4],
                        NA, plotlist[5:8],
                        NA, plotlist[9:12],
                        NA, plotlist[13:16],
                        NA, plotlist[17:20],
                        NA, plotlist[21:24],
                        NA, plotlist[25:28],
                        NA, plotlist[29:32])

plots <- cowplot::plot_grid(plotlist = plotlist, ncol = 5, align = "hv", rel_widths = c(0.1,1,1,1,1), 
                            rel_heights = c(0.05,rep(1,8)))


# ggsave(filename= file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/", "quercus_deciduous3.pdf"), 
#        plot = plots, height=26, width=14, units = c("cm"))

