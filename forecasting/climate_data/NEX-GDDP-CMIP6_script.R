
# NEX-GDDP-CMIP6
## Small script to create a .sh file to download data

server <- "https://ds.nccs.nasa.gov/thredds/ncss/grid/AMES/NEX/GDDP-CMIP6"

models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0")
variants <- c("r1i1p1f1", "r1i1p1f1", "r1i1p1f1", "r1i1p1f1")
grid_labels <- c("gr1", "gr", "gn", "gn")

models <- c("ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
            "CESM2", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2", 
            "CNRM-CM6-1", "CNRM-ESM2-1", "EC-Earth3", "EC-Earth3-Veg-LR", 
            "FGOALS-g3", "GFDL-CM4", "GFDL-CM4_gr2", "GISS-E2-1-G",
            "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "IITM-ESM", "INM-CM4-8",
            "INM-CM5-0", "KACE-1-0-G", "KIOST-ESM", "MIROC-ES2L",
            "MIROC6", "MPI-ESM1-2-LR", "NESM3", "NorESM2-LM", 
            "NorESM2-MM", "TaiESM1", "UKESM1-0-LL")
variants <- c("r1i1p1f1", "r1i1p1f1", "r1i1p1f1", "r1i1p1f1",
              "r4i1p1f1", "r3i1p1f1", "r1i1p1f1", "r1i1p1f1", 
              "r1i1p1f2", "r1i1p1f2", "r1i1p1f1", "r1i1p1f1",
              "r3i1p1f1", "r1i1p1f1", "r1i1p1f1", "r1i1p1f2",
              "r1i1p1f3", "r1i1p1f3", "r1i1p1f1", "r1i1p1f1",
              "r1i1p1f1", "r1i1p1f1", "r1i1p1f1", "r1i1p1f2",
              "r1i1p1f1", "r1i1p1f1", "r1i1p1f1", "r1i1p1f1", 
              "r1i1p1f1", "r1i1p1f1", "r1i1p1f2")
grid_labels <- c("gn", "gn", "gn", "gn",
                 "gn", "gn", "gn", "gn",
                 "gr", "gr", "gr", "gr",
                 "gn", "gr1", "gr2", "gn",
                 "gn", "gn", "gn", "gr1",
                 "gr1", "gr", "gr1", "gn",
                 "gn", "gn", "gn", "gn", 
                 "gn", "gn", "gn")


vars <- c("pr", "tas")

window <- "north=72&west=-13&east=40&south=33"

scenario <- "historical"
years <- c(1951:2014)

models360d <- c( "HadGEM3-GC31-LL", "HadGEM3-GC31-MM","KACE-1-0-G", "KIOST-ESM", "UKESM1-0-LL") # exceptions with 360-day calendar

wget_script <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/climate_data/", "nexgddp_", scenario, ".sh")
file.create(wget_script)
for(i in 1:length(models)){
  model <- models[i]
  variant <- variants[i]
  grid_lab <- grid_labels[i]
  dir.create(file.path("F:/NEX-GDDP-CMIP6", model, scenario, "raw"), recursive = TRUE)
  for(var in vars){
    for(year in years){
      print(year)
      time <- ifelse(model %in% models360d, 
                     paste0("time_start=",year,"-01-01T12:00:00Z&time_end=",year,"-12-30T12:00:00Z"), #360-day model or not?
                     paste0("time_start=",year,"-01-01T12:00:00Z&time_end=",year,"-12-31T12:00:00Z"))
      command <- paste0('wget "', file.path(server, model, scenario, variant, var, paste(
        var, "day", ifelse(model == "GFDL-CM4_gr2", "GFDL-CM4", model), scenario, variant, grid_lab, paste0(
          year, ".nc?", paste(
            paste0("var=", var), window, "horizStride=1", time, "&", "accept=netcdf3", 'addLatLon=true"',
            sep = "&")),
        sep = "_"
      )), " -O ", file.path("F:/NEX-GDDP-CMIP6", model, scenario, "raw", paste0(var, "_day_", model, "_", scenario, "_", year, ".nc")))
      write(command,file=wget_script,append=TRUE)
    }
  }
}


scenari <- c("ssp245", "ssp585")
years <- c(2015:2100)

wget_script <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/climate_data/", "nexgddp_", "ssp", ".sh")
file.create(wget_script)
for(scenario in scenari){
  for(i in 1:length(models)){
    model <- models[i]
    variant <- variants[i]
    grid_lab <- grid_labels[i]
    dir.create(file.path("F:/NEX-GDDP-CMIP6", model, scenario, "raw"), recursive = TRUE)
    for(var in vars){
      for(year in years){
        print(year)
        time <- ifelse(model %in% models360d, 
                       paste0("time_start=",year,"-01-01T12:00:00Z&time_end=",year,"-12-30T12:00:00Z"), #360-day model or not?
                       paste0("time_start=",year,"-01-01T12:00:00Z&time_end=",year,"-12-31T12:00:00Z"))
        command <- paste0('wget "', file.path(server, model, scenario, variant, var, paste(
          var, "day", ifelse(model == "GFDL-CM4_gr2", "GFDL-CM4", model), scenario, variant, grid_lab, paste0(
            year, ".nc?", paste(
              paste0("var=", var), window, "horizStride=1", time, "&", "accept=netcdf3", 'addLatLon=true"',
              sep = "&")),
          sep = "_"
        )), " -O ", file.path("F:/NEX-GDDP-CMIP6", model, scenario, "raw", paste0(var, "_day_", model, "_", scenario, "_", year, ".nc")))
        write(command,file=wget_script,append=TRUE)
      }
    }
  }
}
