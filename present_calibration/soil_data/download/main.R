###############################################
# Script to download data from SoilGrids v2.0 #
###############################################

# nitrogen, pH and carbon stocks
# need local VRT to modify the source filename (relativeToVRT="0")

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/soil_data/download"
vrt_dir <- file.path(wd, "vrt_files")

gdal_path <- "C:/Program Files/QGIS 3.16.16/bin"
cd_comline <- paste("cd", gdal_path)

igh='"+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"' # proj string for Homolosine projection
bb <- c(-14, 34, 40, 72) #xmin, ymin, xmax ymax in degree

output_dir <- "D:/soil/SoilGrids250m_v2"


# NITROGEN #
vrt_list <- list.files(file.path(vrt_dir, "hom"), pattern='nitrogen')

## convert vrt files from Homolosine to LatLon, and crop according to bounding box
for(i in vrt_list){
  gdalwarp_comline <- paste("gdalwarp.exe", "-s_srs", igh, '-t_srs "EPSG:4326"', "-te", paste(bb,collapse=" "),'-of "VRT"',
                            file.path(vrt_dir, "hom", i), dstfile = file.path(vrt_dir, "latlon", i))
  shell(paste(cd_comline, gdalwarp_comline, sep = ' && '))
}

## download TIF files
for(i in vrt_list){
  gdaltranslate_comline <- paste("gdal_translate.exe", '-of GTiff', '-projwin_srs "EPSG:4326"',
                            file.path(vrt_dir, "latlon", i), file.path(output_dir, paste0(substr(i, 1, stop = nchar(i)-4), ".tif")))
  shell(paste(cd_comline, gdaltranslate_comline, sep = ' && '))
}


# pH #
vrt_list <- list.files(file.path(vrt_dir, "hom"), pattern='ph')

## convert vrt files from Homolosine to LatLon, and crop according to bounding box
for(i in vrt_list){
  gdalwarp_comline <- paste("gdalwarp.exe", "-s_srs", igh, '-t_srs "EPSG:4326"', "-te", paste(bb,collapse=" "),'-of "VRT"',
                            file.path(vrt_dir, "hom", i), dstfile = file.path(vrt_dir, "latlon", i))
  shell(paste(cd_comline, gdalwarp_comline, sep = ' && '))
}

## download TIF files
for(i in vrt_list){
  gdaltranslate_comline <- paste("gdal_translate.exe", '-of GTiff', '-projwin_srs "EPSG:4326"',
                                 file.path(vrt_dir, "latlon", i), file.path(output_dir, paste0(substr(i, 1, stop = nchar(i)-4), ".tif")))
  shell(paste(cd_comline, gdaltranslate_comline, sep = ' && '))
}

# Organic carbon stocks #
vrt_list <- list.files(file.path(vrt_dir, "hom"), pattern='ocs')

## convert vrt files from Homolosine to LatLon, and crop according to bounding box
for(i in vrt_list){
  gdalwarp_comline <- paste("gdalwarp.exe", "-s_srs", igh, '-t_srs "EPSG:4326"', "-te", paste(bb,collapse=" "),'-of "VRT"',
                            file.path(vrt_dir, "hom", i), dstfile = file.path(vrt_dir, "latlon", i))
  shell(paste(cd_comline, gdalwarp_comline, sep = ' && '))
}

## download TIF files
for(i in vrt_list){
  gdaltranslate_comline <- paste("gdal_translate.exe", '-of GTiff', '-projwin_srs "EPSG:4326"',
                                 file.path(vrt_dir, "latlon", i), file.path(output_dir, paste0(substr(i, 1, stop = nchar(i)-4), ".tif")))
  shell(paste(cd_comline, gdaltranslate_comline, sep = ' && '))
}

