
pollen_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc"
add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"

plotlist <- unlist(lapply(1:nrow(quercusdeciduous_models), function(i){
  plots <- lapply(c(11750, 11000, 9000, 7000, 500), function(y){
    out <- crop(readRDS(file.path(quercusdeciduous_models[i, "simfolder"], paste0(y,"BP.rds"))), ext(-10,30,34,66)) %>%
      as.data.frame(xy = TRUE)
    names(out) <- c("lon", "lat", "value")
    
    yr_ICE6G <- y%/%500*0.5
    ice_sheet <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), ext(-10,30,34,66))
    ice_sheet_pr <- ice_sheet
    ice_sheet[ice_sheet == 0] <- NA
    
    plot <- ggplot() +
      geom_raster(data = out, aes(x=lon, y=lat, fill = factor(value))) +
      scale_fill_manual(values = c("0" = "#ededed", "1" = "#9dd0c6", "2" = "#4f928e")) +
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
        annotate("text", x = -12.7, y = 50, label = quercusdeciduous_models[i, "name"], color = "black",
                 family= "Helvetica", size = 3, angle = 90)
    }else{
      # add pollen points
      pollen <- readRDS(file.path(pollen_folder, paste0("pres_", y, "BP.rds")))
      add_pollen <- readRDS(file.path(add_pollen_folder, paste0("pres_", y, "BP.rds")))
      pollen <- dplyr::full_join(pollen, add_pollen, by = c("lat", "lon"))
      pollen$pres <- rowSums(pollen[, c("pres.x", "pres.y")], na.rm = T)
      pollen$pres <- ifelse(pollen$pres > 0, 1, 0)
      
      plot <- plot +
        geom_point(data = pollen[pollen$pres == 1 & pollen$lon < 29.9,], aes(x = lon, y = lat), size = 0.03,
                   shape = 18)
    }
    
    # add years (only first line)
    if(quercusdeciduous_models[i, "name"] == "BRT"){
      plot <- plot +
        annotate("text", x = 10, y = 68, label = paste0(y,"BP"), color = "black",
                 family= "Helvetica", size = 3)
    }
    
    # particular case when migration had to start at 11750 rather than 12000
    if(fagus_models[i, "name"] %in% c("Random Forest") & y == 11750){
      plot <- plot +
        annotate("text", x = -8, y = 63, label = "*", color = "black",
                 family= "Helvetica", size = 6)
    }
    
    return(plot)
  })
  
  return(plots)}), 
  recursive = F)

plotlist <- rlist::list.append(rep(NA,6),
                               NA, plotlist[1:5],
                               NA, plotlist[6:10],
                               NA, plotlist[11:15],
                               NA, plotlist[16:20],
                               NA, plotlist[21:25],
                               NA, plotlist[26:30],
                               NA, plotlist[31:35],
                               NA, plotlist[36:40])

fig2_main <- cowplot::plot_grid(plotlist = plotlist, ncol = 6, align = "hv", 
                                rel_widths = c(0.13,1,1,1,1,1), 
                                rel_heights = c(0.05,rep(1,8)))
