
#----------------------------#
# Format future climate data #
#----------------------------#

library(terra)
terraOptions(memfrac=0.9)
library(data.table)

cmip6_dir <- "D:/climate/CMIP6_Adjust"
scenario <- "ssp585"


# IPSL model
model <- "IPSL-CM6A-LR"
format_phenofit(years = c(2010,2019), extent = ext(c(-13,40,33,72)), model, scenario, folder = cmip6_dir)
