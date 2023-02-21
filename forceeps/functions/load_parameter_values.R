library(dplyr)

load_parameter_values <- function(type, species_name, wd){
    values_file <- file.path(wd, "calibration/input_files", paste0(type,".species"))
    values <- read.table(file = values_file, skip=1, sep='\t', comment.char = "", header = TRUE) %>%
      filter(kName == species_name) %>%
      dplyr::select(-c("kID", "kName", "kSName", "colorRGB"))
    
    return(values)

}
