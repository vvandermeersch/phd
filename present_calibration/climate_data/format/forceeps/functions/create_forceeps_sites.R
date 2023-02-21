
create_forceeps_sites <- function(ncells, out_folder, source_folder, data){
  
  
  
  header <- paste0('# Forceps site file generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date())
  
  for(i in 1:ncells){
    
    lines_to_write <- c(header,
                        "",
                        paste("siteName =", paste0("ERA5_", i), sep = " "),
                        paste("siteBucketSize =", data$bucket_size[i], sep = " "),
                        "",
                        paste("siteAverageNitrogen =", data$nitrogen[i], sep = " "),
                        paste("siteLatitude =", data$lat[i], sep = " "),
                        paste("siteSlope =", 0, sep = " "),
                        "",
                        paste("siteRegenerationForclimDensity =", data$reg_forclim_density[i], sep = " "),
                        "",
                        paste("siteBrowsingDensity =", 0, sep = " "),
                        "",
                        paste("siteMaxETPRate =", data$max_ETP_rate[i], sep = " "),
                        "",
                        paste("siteFractionInterceptedPrec =", data$fraction_interc_prec[i], sep = " ")
                        )
    
    climate_file <- paste0(out_folder, i, ".site")
    
    write(lines_to_write, climate_file)
    
  }
  
}
