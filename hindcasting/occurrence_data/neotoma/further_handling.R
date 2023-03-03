
#############################
# Handling Neotoma database #
#############################

# Author: V. Van der Meersch
# Date: 28/02/2023

# Inspired by this page: https://github.com/NeotomaDB/RangeMapper/blob/master/workflows/walkthroughs/RangeMapper_walkthrough_pollen.Rmd

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/neotoma"


# Load packages and functions
library(neotoma2)
library(dplyr)
library(future.apply)
library(ggplot2)
library(cowplot)
library(Bchron)
source(file.path(wd, "functions/basic_functions.R"))


# Triggers
download_data <- F


# Download or reading raw data (we kept only "Trees and Shrubs", "Upland Herbs" and "Terrest. Vasc. Cryptogams")
source(file.path(wd, "scripts", "download_read_data.R"))


# Select samples with known chronologies, and with pollen counts only ("NISP")
pollen_samples_fltd <- pollen_samples %>%
  dplyr::filter(agetype != "Calendar years AD/BC" & agetype != "NA" & !is.na(age_range_young)) %>%
  dplyr::filter(elementtype == "pollen" & units == "NISP")
length(unique(pollen_samples_fltd$siteid))

# Select samples with correct count and site with at least 5 samples
pollen_samples_fltd2 <- pollen_samples_fltd %>%
  dplyr::filter(value > 0) %>% # remove some -1 value of pollen count
  group_by(siteid) %>%
  dplyr::filter(n() >= 5) %>% # at least 5 samples
  ungroup()
length(unique(pollen_samples_fltd2$siteid))

# Samples already calibrated or from calendar-year age-depth model 
# (we don't want to override the expert knowledge that went into inferring the original age)
pollen_samples_fltd_cal <- pollen_samples_fltd2 %>%
  dplyr::filter(agetype == "Calibrated radiocarbon years BP" | agetype == "Calendar years BP")
length(unique(pollen_samples_fltd_cal$siteid))

# Harmonize taxa names
mottletal_data <- fread(file.path(wd, "Mottl_etal_EU_HarmonizationTable.csv"))
pollen_samples_fltd_cal <- left_join(pollen_samples_fltd_cal, mottletal_data, by = join_by(variablename == NeotomaTaxonName))
length(unique(pollen_samples_fltd_cal$variablename))
length(unique(pollen_samples_fltd_cal$MHVar.2))

# Compute pollen proportions
pollen_prop_fltd <- pollen_samples_fltd_cal %>%
  group_by(sampleid) %>%
  mutate(pollensum = sum(value, na.rm = TRUE)) %>%
  dplyr::filter(pollensum >= 100) %>% # set a minimum pollen count (rather conservative, see Djamali & Cilleros, 2020)
  group_by(MHVar.2) %>% 
  mutate(prop = value / pollensum) %>%
  group_by(age, lat, long, sitename, datasetid, sampleid, MHVar.2) %>%
  dplyr::filter(age < 21250) %>%
  summarize(prop = sum(prop)) 

# Filter taxa
taxa <- c("Abies", "Betula", "Corylus", "Fagus", "Fraxinus", "Larix", "Picea", "Pinus", "Quercus")
pollen_prop_fltd <- na.omit(filter_taxa(pollen_prop_fltd, taxa))

# Compute the variable threshold (0.05*maximum relative abundance recorded at any site at any time)
spec_threshold <- pollen_prop_fltd %>% group_by(MHVar.2) %>%
  summarize(ths = 0.05*max(prop))
pollen_prop_fltd <- left_join(pollen_prop_fltd, spec_threshold)

# Temporal interpolation, in 500 year intervals, and filtering prop>=ths (see above)
pollen_prop_int <- pollen_prop_fltd %>%
  mutate(time = (round(age / 500, 0) * 500)) %>%
  group_by(time, lat, long, sitename, datasetid, MHVar.2, ths) %>%
  summarize(prop = mean(prop)) %>%
  dplyr::filter(prop >= ths)
length(unique(pollen_pro_int$sitename))

# Example with Fagus
fagus_pollen <- filter_taxa(pollen_prop_int, "Fagus")
source(file.path(wd, "scripts", "load_europe_map.R"))
ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey', size = 0.1) +
  geom_point(data = fagus_pollen[fagus_pollen$time == 10000,], aes(x = long, y = lat), col = "orange", shape = 8) +
  theme_void() +
  annotate("text", x = -4, y = 65, label = paste(10000, "BP"), colour = 'darkgrey', size = 3) +
  ylab("") +
  xlab("")


macrofossil_samples_fltd <- macrofossil_samples %>%
  dplyr::filter(agetype == "Calibrated radiocarbon years BP" | agetype == "Calendar years BP")
macrofossil_samples_fltd <- left_join(macrofossil_samples_fltd, mottletal_data, by = join_by(variablename == NeotomaTaxonName))
macrofossil_samples_fltd  <- macrofossil_samples_fltd  %>%
  mutate(time = (round(age / 500, 0) * 500)) %>% 
  group_by(time, lat, long, sitename, MHVar.2) %>%
  summarize(count = n())
macrofossil_samples_fltd<- na.omit(filter_taxa(macrofossil_samples_fltd, taxa))


fagus_macfoss <- filter_taxa(macrofossil_samples_fltd, "Fagus")
ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey', size = 0.1) +
  geom_point(data = fagus_pollen[fagus_pollen$time == 3000,], aes(x = long, y = lat), col = "orange", shape = 8) +
  geom_point(data = fagus_macfoss [fagus_macfoss $time == 7000,], aes(x = long, y = lat), col = "brown", shape = 18, size = 2) +
  theme_void() +
  annotate("text", x = -4, y = 65, label = paste(3000, "BP"), colour = 'darkgrey', size = 3) +
  ylab("") +
  xlab("")






# Uncalibrated  samples on which we may able to run Bchron
pollen_samples_fltd_uncal <- pollen_samples_fltd2 %>%
  dplyr::filter(agetype == "Radiocarbon years BP") %>%
  dplyr::filter(!is.na(ageyounger))
length(unique(pollen_samples_fltd_uncal$siteid))




chrono <- function(i){
  site <- get_downloads(i)
  controls <- chroncontrols(site)
  print(nrow(controls))
}

for(i in unique(pollen_samples_fltd_uncal$siteid)){
  print(i)
  try(chrono(i))
}



# Running Bchron on set 1 (see above)
calibrated_ages <- BchronCalibrate(ages = pollen_samples_fltd_uncal$age, 
                                  ageSds = (pollen_samples_fltd_uncal$ageolder-pollen_samples_fltd_uncal$ageyounger)/2, 
                                  calCurves = rep('intcal20', nrow(pollen_samples_fltd_uncal)),
                                  allowOutside = TRUE)
age_samples = sampleAges(calibrated_ages)
cal_lb <- apply(age_samples, 2, quantile, prob=c(0.025)) # lower bound, 95% credible interval
cal_ub <- apply(age_samples, 2, quantile, prob=c(0.975)) # upper bound, 95% credible interval
cal_med <- apply(age_samples, 2, quantile, prob=c(0.5)) # median
rm(age_samples, calibrated_ages)
gc()


# Running Bchron on set 2 (see above)
test <- get_downloads(3912)
chronotest <- chroncontrols(test)

