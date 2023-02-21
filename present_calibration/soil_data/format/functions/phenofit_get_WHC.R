

phenofit_get_WHC <- function(rd, pd_folder){
  
  rd$WHC<- round(rd$WHC, 3)
  whc <- data.frame(lat = rd$lat, lon = rd$lon, WHC = rd$WHC)

  # Fill name
  processed_file <- paste0(pd_folder, "ERA5LAND_WHC.fit")
  
  # Fill file
  con <- file(processed_file, open="wt")
  writeLines("WHC datafile for Phenofit model", con)
  writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
  comments <- get_comments(var = "WHC")
  writeLines(comments, con)
  writeLines(" ", con)
  write.table(whc, file = con, sep = "\t",
              row.names = FALSE, col.names= FALSE)
  close(con)
  
  # Free memory
  gc(verbose = FALSE)
  
}