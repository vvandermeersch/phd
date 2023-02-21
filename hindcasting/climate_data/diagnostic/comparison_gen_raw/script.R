
library(terra)
library(ggplot2)
library(data.table)
library(future.apply)
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/functions/years_functions.R")

data_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset"
extent <- ext(c(-14,40,34,72))

# create dataframe
min_temp_df <- data.frame(year = seq(21000,1000, -100))

# load raw monthly data
raw_min_temp <- future_sapply(min_temp_df$year, function(yr){
  # print(yr)
  extent <- ext(c(-14,40,34,72))
  file <- years_to_file(yr)
  mn <- year_to_months(yr, yr, file$max)
  # r <- rast(file.path(data_dir, "raw","temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr",
  #                paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
  #           subds = "temp_mm_1_5m", lyrs = mn$min:mn$max, opts="HONOUR_VALID_RANGE=NO")
  r <- rast(file.path(data_dir, "raw", "tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                           paste0("tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc")),
                 subds = "tempmin_av", lyrs = mn$min:mn$max)
  r <- crop(r, extent)
  r <- mean(r)
  val <- values(r)
  return(mean(val, na.rm = T))
})

min_temp_df$raw <- raw_min_temp
ggplot(data = min_temp_df) +
  geom_point(aes(x = year, y = raw)) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 1000),1)) +
  theme_minimal()


# load generated daily values
gen_temp <- sapply(seq(21000,1000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg", paste0(yr, "BP"),
                       paste0("HadCM3B_tmn_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(mean(as.matrix(d)))
})

# load generated daily values
gen_tempcor <- sapply(1000, function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg_cor", paste0(yr, "BP"),
                       paste0("HadCM3B_tmn_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(mean(as.matrix(d)))
})


temp_df <- data.frame(year = seq(21000,1000,-2000), gen = gen_temp)

ggplot() +
  geom_line(data = min_temp_df[min_temp_df$year >= 1000,], aes(x = year, y = raw), col = "#a8e6cf", size = 1.3, alpha = 0.6) +
  geom_point(data = temp_df, aes(x = year, y = gen), col = "#ffaaa5", size = 3, shape=20) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 4000)) +
  scale_y_continuous(breaks = seq(-14, 5, by = 2)) +
  theme_minimal() +
  ylab("Minimal temperature") + 
  xlab("Years BP") +
  theme(axis.text=element_text(size=9, color = alpha("#49657b",0.7)),
        axis.title=element_text(size=10, color = alpha("#0d2f4a",0.8)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = alpha("#49657b",0.1)),
        axis.ticks = element_line(color = alpha("#49657b",0.7)),
        axis.ticks.length=unit(.1, "cm"),
        axis.line = element_line(color = alpha("#49657b",0.7))
        )

         






# create dataframe
pre_df <- data.frame(year = seq(21000,1000, -100))

# load raw monthly data
raw_pre <- future_sapply(pre_df$year, function(yr){
  # print(yr)
  extent <- ext(c(-14,40,34,72))
  file <- years_to_file(yr)
  mn <- year_to_months(yr, yr, file$max)
  # r <- rast(file.path(data_dir, "raw","temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
  #                paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
  #           subds = "temp_mm_1_5m", lyrs = mn$min:mn$max)
  r <- rast(file.path(data_dir, "raw", "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                      paste0("precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
            subds = "precip_mm_srf", lyrs = mn$min:mn$max)
  r <- crop(r, extent)
  r <- sum(r)
  val <- values(r)
  return(mean(val, na.rm = T))
})

pre_df$raw <- raw_pre
ggplot(data = pre_df) +
  geom_point(aes(x = year, y = raw)) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 1000),1)) +
  theme_minimal()


# load generated daily values
gen_pre <- sapply(seq(21000,1000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg", paste0(yr, "BP"),
                       paste0("HadCM3B_pre_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(sum(as.matrix(d))/nrow(d))
})

gpre_df <- data.frame(year = seq(21000,1000,-2000), gen = gen_pre)

ggplot() +
  geom_line(data = pre_df[pre_df$year >= 1000,], aes(x = year, y = raw*30), col = "#a8e6cf", size = 1.5, alpha = 0.6) +
  geom_point(data = gpre_df, aes(x = year, y = gen), col = "#ffaaa5", size = 3) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 2000)) +
  theme_minimal() +
  ylab("Precipitation") + 
  xlab("Years BP") +
  theme(axis.text=element_text(size=12, color = "#c0c2ce"),
        axis.title=element_text(size=12, color = "#49657b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3, color = "#c0c2ce"))     
























# create dataframe
glo_df <- data.frame(year = seq(21000,1000, -100))

# load raw monthly data
raw_glo <- future_sapply(glo_df$year, function(yr){
  # print(yr)
  extent <- ext(c(-14,40,34,72))
  # extent <- ext(c(-14,40,34,55))
  file <- years_to_file(yr)
  mn <- year_to_months(yr, yr, file$max)
  # r <- rast(file.path(data_dir, "raw","temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
  #                paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
  #           subds = "temp_mm_1_5m", lyrs = mn$min:mn$max)
  r <- rast(file.path(data_dir, "raw", "downSol_Seaice_mm_s3_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                      paste0("downSol_Seaice_mm_s3_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc")),
            subds = "downSol_Seaice_mm_s3_srf", lyrs = mn$min:mn$max, opts="HONOUR_VALID_RANGE=NO")
  r <- crop(r, extent)
  r <- sum(r*30*24*3600/1E6)
  val <- values(r)
  return(mean(val, na.rm = T))
})


glo_df$raw <- raw_glo
ggplot(data = glo_df) +
  geom_point(aes(x = year, y = raw)) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 1000),1)) +
  theme_minimal()


# load generated daily values
gen_glo <- sapply(seq(21000,1000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg_cor", paste0(yr, "BP"),
                       paste0("HadCM3B_glo_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(sum(as.matrix(d))/nrow(d))
})

gglo_df <- data.frame(year = seq(21000,1000,-2000), gen = gen_glo)

ggplot() +
  geom_line(data = glo_df[glo_df$year >= 1000,], aes(x = year, y = raw), col = "#a8e6cf", size = 10, alpha = 0.6) +
  geom_point(data = gglo_df, aes(x = year, y = gen), col = "#ffaaa5", size = 18) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 2000)) +
  theme_minimal() +
  ylab("Global radiation") + 
  xlab("Years BP") +
  theme(axis.text=element_text(size=50, color = "#c0c2ce"),
        axis.title=element_text(size=46, color = "#49657b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 1, color = "#c0c2ce"))     









# create dataframe
toa_df <- data.frame(year = seq(21000,1000, -100))

# load raw monthly data
raw_toa <- future_sapply(toa_df$year, function(yr){
  # print(yr)
  extent <- ext(c(-14,40,34,72))
  file <- years_to_file(yr)
  mn <- year_to_months(yr, yr, file$max)
  # r <- rast(file.path(data_dir, "raw","temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
  #                paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
  #           subds = "temp_mm_1_5m", lyrs = mn$min:mn$max)
  r <- rast(file.path(data_dir, "raw", "downSol_mm_TOA_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                      paste0("downSol_mm_TOA_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc")),
            subds = "downSol_mm_TOA", lyrs = mn$min:mn$max, opts="HONOUR_VALID_RANGE=NO")
  r <- crop(r, extent)
  r <- sum(r*30*24*3600/1E6)
  val <- values(r)
  return(mean(val, na.rm = T))
})


toa_df$raw <- raw_toa
ggplot(data = toa_df) +
  geom_point(aes(x = year, y = raw)) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal()

ggplot(data = glo_df) +
  geom_point(aes(x = year, y = raw)) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal()

# load generated daily values
gen_toa <- sapply(seq(21000,3000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg", paste0(yr, "BP"),
                       paste0("HadCM3B_TOA_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(sum(as.matrix(d))/nrow(d))
})

gtoa_df <- data.frame(year = seq(21000,3000,-2000), gen = gen_toa/1000)


coef <- 1.5
ggplot() +
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "#FC9601", size = 3) + 
  geom_point(data = gtoa_df, aes(x = year, y = gen/coef), color = "#FFCC33", size = 3) + 
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~.*coef , name="TOA radiation")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#FFCC33")
  ) 


ggplot() +
  geom_point(data = toa_df[glo_df$year >= 1000,], aes(x = year, y = raw), col = "#a8e6cf", size = 10, alpha = 0.6) +
  geom_point(data = gtoa_df, aes(x = year, y = gen), col = "#ffaaa5", size = 18) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 2000)) +
  theme_minimal() +
  ylab("Global radiation") + 
  xlab("Years BP") +
  theme(axis.text=element_text(size=50, color = "#c0c2ce"),
        axis.title=element_text(size=46, color = "#49657b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 1, color = "#c0c2ce"))    






# load generated daily values
gen_pet <- sapply(seq(21000,1000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg", paste0(yr, "BP"),
                       paste0("HadCM3B_pet_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(sum(as.matrix(d))/nrow(d))
})
gpet_df <- data.frame(year = seq(21000,1000,-2000), gen = gen_pet)

ggplot(data = gpet_df) +
  geom_point(aes(x = year, y = gen)) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 1000)) +
  theme_minimal()






# load generated daily values
gen_cld <- sapply(seq(21000,17000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg_cor", paste0(yr, "BP"),
                       paste0("HadCM3B_cld_-", yr,"_dly.fit")))
  d <- d[,-c(1,2)]
  return(mean(as.matrix(d)))
})
gcld_df <- data.frame(year = seq(21000,17000,-2000), gen = gen_cld)

ggplot(data = gcld_df) +
  geom_point(aes(x = year, y = gen)) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 1000)) +
  theme_minimal()



data_df <- data.frame(year = seq(21000,1000,-2000), cld = gen_cld)




# create dataframe
cld_df <- data.frame(year = seq(21000,1000, -100))

# load raw monthly data
raw_cld <- future_sapply(cld_df$year, function(yr){
  # print(yr)
  extent <- ext(c(-14,40,34,72))
  file <- years_to_file(yr)
  mn <- year_to_months(yr, yr, file$max)
  # r <- rast(file.path(data_dir, "raw","temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
  #                paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
  #           subds = "temp_mm_1_5m", lyrs = mn$min:mn$max)
  r <- rast(file.path(data_dir, "raw", "totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                      paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
            subds = "totCloud_mm_ua", lyrs = mn$min:mn$max, opts="HONOUR_VALID_RANGE=NO")
  r <- crop(r, extent)
  # r <- sum(r*30*24*3600/1E6)
  val <- values(r)
  return(mean(val, na.rm = T))
})

cld_df$raw <- raw_cld

ggplot() +
  geom_point(data = cld_df[cld_df$year >= 1000,], aes(x = year, y = raw), col = "#a8e6cf", size = 10, alpha = 0.6) +
  geom_point(data = gcld_df, aes(x = year, y = gen), col = "#ffaaa5", size = 18) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 2000)) +
  theme_minimal() +
  ylab("Cloudiness") + 
  xlab("Years BP") +
  theme(axis.text=element_text(size=50, color = "#c0c2ce"),
        axis.title=element_text(size=46, color = "#49657b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 1, color = "#c0c2ce"))  


data_newglo <- data.frame(year = c(21000,19000), gen = c(sum(glodata$glo)/nrow(d), sum(glodata19$glo)/nrow(d)))

coef <- 6000
ggplot() +
  geom_point(data = data_newglo, aes(x = year, y = gen/1000), color = "red", size = 3) + 
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "#FC9601", size = 3) + 
  geom_point(data = glo_df[glo_df$year >= 1000,], aes(x = year, y = raw), color = "#FC9601", size = 1, alpha = 0.5) + 
  geom_point(data = gcld_df, aes(x = year, y = (0.94*gen)*coef), color = "#81a5ba", size = 3) + 
  geom_point(data = cld_df[cld_df$year >= 1000,], aes(x = year, y = raw*coef), color = "#81a5ba", size = 1, alpha = 0.5) + 
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~./coef , name="Cloudiness")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#81a5ba")
  ) 

coef <- 6
ggplot() +
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "#FC9601", size = 3) + 
  geom_point(data = glo_df[glo_df$year >= 1000,], aes(x = year, y = raw), color = "#FC9601", size = 1, alpha = 0.5) + 
  geom_point(data = gpet_df, aes(x = year, y = gen*coef), color = "#87bbad", size = 3) + 
  geom_point(data = pre_df[pre_df$year >= 1000,], aes(x = year, y = raw*30*coef), color = "#87bbad", size = 1, alpha = 0.5) + 
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~./coef , name="Precipitation")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#81a5ba")
  ) 



ggplot() +
  geom_point(data = cld_df[cld_df$year >= 1000,], aes(x = year, y = raw), col = "#a8e6cf", size = 10, alpha = 0.6) +
  geom_point(data = gcld_df, aes(x = year, y = gen), col = "#ffaaa5", size = 18) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 2000)) +
  theme_minimal() +
  ylab("Cloudiness") + 
  xlab("Years BP") +
  theme(axis.text=element_text(size=50, color = "#c0c2ce"),
        axis.title=element_text(size=46, color = "#49657b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 1, color = "#c0c2ce"))  


coef <- 6000
ggplot() +
  geom_point(data = gglo55_df, aes(x = year, y = gen), color = "red", size = 3) + 
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "#FC9601", size = 3) + 
  geom_line(data = glo_df55[glo_df55$year >= 1000,], aes(x = year, y = raw), color = "red", size = 1, alpha = 0.5) + 
  geom_line(data = glo_df[glo_df$year >= 1000,], aes(x = year, y = raw), color = "#FC9601", size = 1, alpha = 0.5) + 
  scale_y_continuous(
    name = "Global radiation"
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "#81a5ba")
  ) 

data_newglo55 <- data.frame(year = c(21000,19000), gen = c(sum(glodata[glodata$id >= 3712,"glo"])/nrow(unique(glodata[glodata$id >= 3712,"id"])), 
                                                           sum(glodata19[glodata19$id >= 3716,"glo"])/nrow(unique(glodata19[glodata19$id >= 3716,"id"]))))





# load generated daily values
gen_glo <- sapply(seq(21000,13000,-2000), function(yr){
  print(yr)
  d <- fread(file.path(data_dir, "phenofit_format", "05deg_cor", paste0(yr, "BP"),
                       paste0("HadCM3B_glo_-", yr,"_dly.fit")))
  d <- data.frame(d)
  colnames(d)[1] <- "lat"
  #d <- d[d$lat <= 55,-c(1,2)]
  return(sum(as.matrix(d))/nrow(d))
})

gglo_df <- data.frame(year = seq(21000,13000,-2000), gen = gen_glo)




file <- years_to_file(year)
albedo_file <- file.path(raw_clim_dir, "albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr",
                         paste0("albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc"))
years <- file$min:file$max
mn_e <- which(years == year)*12
mn_b <- mn_e-11
albedo <- rast(albedo_file, subds = "albedos", lyrs = mn_b:mn_e)










ggplot() +
  geom_line(data = cld_df[cld_df$year >= 1000,], aes(x = year, y = raw), col = "#a8e6cf", size = 1.5, alpha = 0.6) +
  geom_point(data = gcld_df, aes(x = year, y = gen), col = "#ffaaa5", size = 3) +
  scale_x_continuous(breaks = seq(1000, 21000, by = 2000)) +
  theme_minimal() +
  ylab("Cloudiness") + 
  xlab("Years BP") +
  ylim(c(0,1)) +
  theme(axis.text=element_text(size=12, color = "#c0c2ce"),
        axis.title=element_text(size=12, color = "#49657b"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3, color = "#c0c2ce"))    


gglo_df <- data.frame(year = c(21000, 19000, 17000, 15000, 13000, 11000, 9000, 3000),
                      gen = c(4914, 4887, 5207, 5019, 4859, 4616, 4382, 4487))
coef <- 1.5
ggplot() +
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "red", fill = "#FC9601", size = 2, pch = 21) + 
  geom_point(data = gglouncor_df, aes(x = year, y = gen), color = "#FC9601", size = 1.5) + 
  geom_point(data = gtoa_df, aes(x = year, y = gen/coef), color = "#FFCC33", size = 3) + 
  geom_line(data = toa_df[toa_df$year >= 1000,], aes(x = year, y = raw/coef), col = "#FFCC33", size = 1.5, alpha = 0.4) +
  geom_line(data = glo_df[toa_df$year >= 1000,], aes(x = year, y = raw), col = "#FC9601", size = 1, alpha = 0.4) +
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~.*coef , name="TOA radiation")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#FFCC33")
  ) 

ggplot() +
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "red", fill = "#FC9601", size = 2, pch = 21) + 
  geom_point(data = gglouncor_df, aes(x = year, y = gen), color = "#FC9601", size = 1.5) + 
  geom_point(data = gtoa_df, aes(x = year, y = gen/coef), color = "#FFCC33", size = 3) + 
  geom_line(data = toa_df[toa_df$year %in% gglo_df$year,], aes(x = year, y = raw/coef), col = "#FFCC33", size = 1.5, alpha = 0.4) +
  geom_line(data = glo_df[toa_df$year %in% gglo_df$year,], aes(x = year, y = raw), col = "#FC9601", size = 1, alpha = 0.4) +
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~.*coef , name="TOA radiation")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#FFCC33")
  )



coef <- 6000
ggplot() +
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "#FC9601", size = 3) + 
  geom_point(data = glo_df[glo_df$year >= 1000,], aes(x = year, y = raw), color = "#FC9601", size = 1, alpha = 0.5) + 
  geom_point(data = gcld_df, aes(x = year, y = (gen)*coef), color = "#81a5ba", size = 3) + 
  geom_point(data = gcld_df[gcld_df$year >= 12000,], aes(x = year, y = (0.95*gen)*coef), color = "purple", size = 3) + 
  geom_point(data = gcld_df[gcld_df$year < 12000,], aes(x = year, y = (0.92*gen)*coef), color = "purple", size = 3) +
  geom_point(data = cld_df[cld_df$year >= 1000,], aes(x = year, y = raw*coef), color = "#81a5ba", size = 1, alpha = 0.5) + 
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~./coef , name="Cloudiness")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#81a5ba")
  ) 




alb_df <- data.frame(year = seq(21000,6000, -100))

# load raw monthly data
raw_alb <- future_sapply(alb_df$year, function(yr){
  # print(yr)
  extent <- ext(c(-14,40,34,72))
  file <- years_to_file(yr)
  mn <- year_to_months(yr, yr, file$max)
  # r <- rast(file.path(data_dir, "raw","temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
  #                paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file$name, ".nc")),
  #           subds = "temp_mm_1_5m", lyrs = mn$min:mn$max)
  r <- rast(file.path(data_dir, "raw", "albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                      paste0("albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc")),
            subds = "albedos", lyrs = mn$min:mn$max, opts="HONOUR_VALID_RANGE=NO")
  r <- crop(r, extent)
  val <- values(r)
  return(mean(val, na.rm = T))
})


alb_df$raw <- raw_alb




coef <- 10000
ggplot() +
  geom_point(data = gglo_df, aes(x = year, y = gen), color = "#FC9601", size = 3) + 
  geom_point(data = glo_df[glo_df$year >= 6000,], aes(x = year, y = raw), color = "#FC9601", size = 1, alpha = 0.5) + 
  geom_point(data = alb_df[alb_df$year >= 6000 & alb_df$raw <0.5,], aes(x = year, y = raw*coef), color = "#5D3891", size = 1, alpha = 0.5) + 
  scale_y_continuous(
    name = "Global radiation",
    sec.axis = sec_axis(~./coef , name="Cloudiness")
  ) +
  scale_x_continuous(breaks = round(seq(min(min_temp_df$year), max(min_temp_df$year), by = 5000),1)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "#FC9601"),
    axis.title.y.right = element_text(color = "#81a5ba")
  )



