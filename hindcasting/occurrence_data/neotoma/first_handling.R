#####################################
# First tests with Neotoma database #
#####################################

# Author: V. Van der Meersch
# Date: 29/11/2022

# Inspired by this page: https://github.com/NeotomaDB/RangeMapper/blob/master/workflows/walkthroughs/RangeMapper_walkthrough_pollen.Rmd

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/hindcasting/occurrence_data/neotoma"

# Load packages and functions
library(neotoma2)
library(dplyr)
library(future.apply)
library(ggplot2)
library(cowplot)
source(file.path(wd, "functions/basic_functions.R"))


# Triggers
download_data <- F


# Download or reading raw data
source(file.path(wd, "scripts", "download_read_data.R"))


# Select samples with calibrated radiocarbon dates
pollen_samples_fltd <- pollen_samples %>%
  dplyr::filter(agetype != "Radiocarbon years BP" & agetype != "NA")
macrofossil_samples_fltd <- macrofossil_samples %>%
  dplyr::filter(agetype != "Radiocarbon years BP" & agetype != "NA")


# Select pollen counts only ("NISP") for pollen, and age interval (+/- 250 for linear interpolation, see below)
age_young <- 9000
age_old <- 21000

other_samples_fltd <- pollen_samples_fltd %>%       # keep on hand other sample types (e.g. wood, stomata), exclude spores
  dplyr::filter(elementtype != "spore" & elementtype != "pollen") %>%
  dplyr::filter(age <= age_old + 250 & age >= age_young - 250)

pollen_samples_fltd <- pollen_samples_fltd %>%
  dplyr::filter(elementtype == "pollen" & units == "NISP") %>%
  dplyr::filter(age <= age_old + 250 & age >= age_young - 250)

macrofossil_samples_fltd <- macrofossil_samples_fltd %>%
  dplyr::filter(age <= age_old + 250 & age >= age_young - 250)


# Compute pollen proportions
pollen_prop_fltd <- pollen_samples_fltd %>%
  dplyr::filter(value > 0) %>% # remove some -1 value of pollen count
  group_by(sampleid) %>%
  mutate(pollensum = sum(value, na.rm = TRUE)) %>%
  dplyr::filter(pollensum >= 100) %>% # set a minimum pollen count (rather conservative, see Djamali & Cilleros, 2020)
  group_by(variablename) %>% 
  mutate(prop = value / pollensum) %>%
  group_by(age, lat, long, sitename, datasetid, sampleid, variablename) %>%
  summarize(prop = sum(prop)) 


# Select taxa of interest
taxa <- c("Abies", "Betula", "Corylus", "Fagus", "Fraxinus", "Larix", "Picea", "Pinus", "Quercus")
macrofossil_samples_fltd <- filter_taxa(macrofossil_samples_fltd, taxa) 
pollen_prop_fltd <- filter_taxa(pollen_prop_fltd, taxa)
other_samples_fltd <- filter_taxa(other_samples_fltd, taxa)
taxa_included <- data.frame(unique(c(macrofossil_samples_fltd$variablename, pollen_prop_fltd$variablename, other_samples_fltd$variablename)))


# Europe map
source(file.path(wd, "scripts", "load_europe_map.R"))


# Fagus
taxa <- c("Fagus") 
source(file.path(wd, "scripts", "extract_taxa_data.R"))
years <- rev(unique(pollen_int$time))
years <- c(21000, 19000, 17000, years)
fagus_maps <- create_chrono_maps(pollen_data = pollen_int, 
                                 macrofossil_data = macrofossils_int, 
                                 other_data = others_int, 
                                 years, ice_frac = T)


# Quercus ilex
taxa <- c("Quercus ilex-type", "Quercus ilex/Q. coccifera-type", "Quercus ilex/Q. coccifera",
          "Quercus ilex", "Quercus cf. Q. ilex", "Quercus ilex-type") 
source(file.path(wd, "scripts", "extract_taxa_data.R"))
years <- rev(unique(pollen_int$time))
quercusilex_maps <- create_chrono_maps(pollen_data = pollen_int, 
                                 macrofossil_data = macrofossils_int, 
                                 other_data = others_int, 
                                 years, ice_frac = T)  


# Betula pendula
taxa <- c("Betula pendula", "Betula pendula/B. pubescens", "Betula pendula subsp. mandshurica") 
source(file.path(wd, "scripts", "extract_taxa_data.R"))
years <- rev(unique(pollen_int$time))
years <- c(21000, 19000, 17000, years)
betulapendula_maps <- create_chrono_maps(pollen_data = pollen_int, 
                                       macrofossil_data = macrofossils_int, 
                                       other_data = others_int, 
                                       years, ice_frac = T) 

# Abies alba
taxa <- c("Abies alba", "Abies") 
source(file.path(wd, "scripts", "extract_taxa_data.R"))
years <- rev(unique(pollen_int$time))
abiesalba_maps <- create_chrono_maps(pollen_data = pollen_int, 
                                         macrofossil_data = macrofossils_int, 
                                         other_data = others_int, 
                                         years, ice_frac = T) 

taxa <- c("Quercus robur-type", "Quercus robur", "Quercus robur group", "Quercus pubescens/Q. robur-type",
          "Quercus cf. Q. robur")

taxa <-c("Quercus pubescens-type", "Quercus pubescens/Q. robur-type", "Quercus pubescens/Q. cerris-type",
         "Quercus pubescens")


taxa <- c("Quercus petraea")
