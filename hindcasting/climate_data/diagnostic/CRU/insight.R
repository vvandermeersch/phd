

library(terra)
library(ggplot2)
library(dplyr)

ext <- ext(c(-14,40,34,72))

# Load CRU data
CRU_folder <- "D:/climate/CRU"
list_years <- c("1901.1910", "1911.1920", "1921.1930", "1931.1940", "1941.1950")

tmin_CRU <- rast(lapply(list_years, function(i) rast(file.path(CRU_folder, "TMN", paste0("cru_ts4.05.",i,".tmn.dat.nc")),subds = "tmn")))
tmax_CRU <- rast(lapply(list_years, function(i) rast(file.path(CRU_folder, "TMX", paste0("cru_ts4.05.",i,".tmx.dat.nc")),subds = "tmx")))
tmin_CRU <- crop(tmin_CRU, ext)
tmax_CRU <- crop(tmax_CRU, ext)

CRU_df <- data.frame(year = rep(1901:1950, each = 12), month= rep(1:12, length(1901:1950)))
CRU_df$tmin <- as.numeric(sapply(1:(12*length(1901:1950)), function(i) global(subset(tmin_CRU, i), "mean", na.rm = TRUE)))
CRU_df$tmax <- as.numeric(sapply(1:(12*length(1901:1950)), function(i) global(subset(tmax_CRU, i), "mean", na.rm = TRUE)))
CRU_df$d <- as.Date(paste(CRU_df$year, "-", CRU_df$month, "-01", sep=""))
ggplot(data = CRU_df) +
  geom_line(aes(x = d, y = tmin))
CRU_yrly_df <- CRU_df %>%
  group_by(year) %>%
  dplyr::summarize(mean_tmin = mean(tmin, na.rm=FALSE),
                   mean_tmax = mean(tmax, na.rm=FALSE))
ggplot(data = CRU_yrly_df) +
  geom_line(aes(x = year, y = mean_tmin), col = "blue") +
  geom_line(aes(x = year, y = mean_tmax), col = "red") +
  theme_minimal()

# Load HadCM3B data
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_novelty/functions/load_raster.R")
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/functions/years_functions.r")
HadCM3B_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"

tmin_HadCM3B <- load_raster(yr = 25, var = "tempmin", dir = HadCM3B_folder, wnd = 25) # 25BP = 1925
tmin_HadCM3B <- subset(tmin_HadCM3B, 13:612) # remove 1900
tmin_HadCM3B  <- crop(tmin_HadCM3B, ext)
tmax_HadCM3B <- load_raster(yr = 25, var = "tempmax", dir = HadCM3B_folder, wnd = 25) # 25BP = 1925
tmax_HadCM3B <- subset(tmax_HadCM3B, 13:612) # remove 1900
tmax_HadCM3B  <- crop(tmax_HadCM3B, ext)

HadCM3B_df <- data.frame(year = rep(1901:1950, each = 12), month= rep(1:12, length(1901:1950)))
HadCM3B_df$tmin <- as.numeric(sapply(1:(12*length(1901:1950)), function(i) global(subset(tmin_HadCM3B, i), "mean", na.rm = TRUE)))
HadCM3B_df$tmax <- as.numeric(sapply(1:(12*length(1901:1950)), function(i) global(subset(tmax_HadCM3B, i), "mean", na.rm = TRUE)))
HadCM3B_df$d <- as.Date(paste(HadCM3B_df$year, "-", HadCM3B_df$month, "-01", sep=""))
ggplot(data = HadCM3B_df) +
  geom_line(aes(x = d, y = tmin))
HadCM3B_yrly_df <- HadCM3B_df %>%
  group_by(year) %>%
  dplyr::summarize(mean_tmin = mean(tmin, na.rm=FALSE),
                   mean_tmax = mean(tmax, na.rm=FALSE))
ggplot(data = HadCM3B_yrly_df) +
  geom_line(aes(x = year, y = mean_tmin), col = "blue") +
  geom_line(aes(x = year, y = mean_tmax), col = "red") +
  theme_minimal()



# Combine
ggplot() +
  geom_line(data = HadCM3B_yrly_df, aes(x = year, y = mean_tmin), col = "blue") +
  geom_line(data = HadCM3B_yrly_df, aes(x = year, y = mean_tmax), col = "red") +
  geom_line(data = CRU_yrly_df, aes(x = year, y = mean_tmin), col = "blue", linetype = "dashed") +
  geom_line(data = CRU_yrly_df, aes(x = year, y = mean_tmax), col = "red", linetype = "dashed") +
  theme_minimal() +
  ylab("Yearly mean min. temperature across Europe") +
  xlab("Year")




tmax_HadCM3B_mn <- mean(tmax_HadCM3B)
tmin_HadCM3B_mn <- mean(tmin_HadCM3B)
tmax_CRU_mn <- mean(tmax_CRU)
tmin_CRU_mn <- mean(tmin_CRU)
ext(tmin_CRU_mn) <- ext(tmin_HadCM3B_mn)

tmax_CRU_mn <- resample(tmax_CRU_mn , tmax_HadCM3B_mn)
tmin_CRU_mn <- resample(tmin_CRU_mn , tmin_HadCM3B_mn)
dif_tmax <- tmax_CRU_mn - tmax_HadCM3B_mn
dif_tmin <- tmin_CRU_mn - tmin_HadCM3B_mn
