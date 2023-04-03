
#####################################
# Handling Herzschuh et al. dataset #
#####################################

# Author: V. Van der Meersch
# Date: 01/03/2023

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"

input_folder <- "D:/species/pollen/herzschuhetal2021"
output_folder <- "D:/species/pollen/processed/fagus_sylvatica"

library(data.table)
library(dplyr)

# Load European dataset (Herzschuh et al. 2022)
dataset <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-percentages.tab"), skip = 1302)
dataset$un_ID <- rownames(dataset)
dataset_count <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-counts.tab"), skip = 1302) %>%
  mutate(pollen_count = rowSums(across(15:223))) %>%
  dplyr::select(c("pollen_count"))
dataset_count$un_ID <- rownames(dataset_count)
metadata <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))

# Removing sequences with less thant 150 pollen grain counts
dataset <- left_join(dataset, dataset_count) %>%
  dplyr::filter(pollen_count >= 150)

# Removing marine sites following Maguire et al. (2016)
dataset <- left_join(dataset, metadata) %>%
  dplyr::filter(`Loc type` != "Marine" & `Loc type` != "Lagoon")

# Example Fagus
fagus_threshold <- max(dataset$`Fagus [%]`)*0.05 # threshold scaled to 5% of the maximum abundance 
                                                 # following Nieto-Lugilde et al. (2015), Maguire et al. (2016)
fagus_presence <- dataset %>% 
  dplyr::filter(`Fagus [%]` >= fagus_threshold) %>%
  dplyr::select(c(1:14, "Loc type", "Fagus [%]"))

fagus_absence <- dataset %>% 
  dplyr::filter(`Fagus [%]` < fagus_threshold) %>%
  dplyr::select(c(1:14, "Loc type", "Fagus [%]"))


year <- 1000
ggplot() +
  field
  geom_point(data = fagus_absence[fagus_absence$`Age min [ka]` <= year/1000 &
                                    fagus_absence$`Age max [ka]` >= year/1000,], 
             aes(x = Longitude, y = Latitude), col = "darkgrey", shape = 20, alpha = 0.5) +
  geom_point(data = fagus_presence[fagus_presence$`Age min [ka]` <= year/1000 &
                                 fagus_presence$`Age max [ka]` >= year/1000,], 
             aes(x = Longitude, y = Latitude), col = "orange", shape = 8) +
  theme_void() +
  annotate("text", x = -4, y = 65, label = paste0(year, "ka BP"), colour = 'darkgrey', size = 3) +
  ylab("") +
  xlab("") +
  xlim(-10, 41)


# Example Abies
abies_threshold <- max(dataset$`Abies [%]`)*0.05
abies_data <- dataset %>% 
  dplyr::filter(`Abies [%]` >= abies_threshold) %>%
  dplyr::select(c(1:14, "Loc type", "Abies [%]"))

year <- 3000/1000 # ka
ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey', size = 0.1) +
  geom_point(data = abies_data[abies_data$`Age min [ka]` <= year &
                                 abies_data$`Age max [ka]` >= year,], 
             aes(x = Longitude, y = Latitude), col = "orange", shape = 8) +
  theme_void() +
  annotate("text", x = -4, y = 65, label = paste0(year, "ka BP"), colour = 'darkgrey', size = 3) +
  ylab("") +
  xlab("") +
  xlim(-10, 41)


# Notes:
# - Fitzpatrick et al. 2018: if multiple sites fell within the same grid cell, their pollen abundances were averaged
#   Because the majority of fossil-pollen types considered here can be consistently identified, absences are considered true absence


