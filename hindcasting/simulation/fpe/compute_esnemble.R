#----------------------------------#
# Small script to compute ensemble #
#----------------------------------#

source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

output_folder <- "D:/simulations/phenofit/paleo/fpensemble/025deg/fagus_sylvatica"


for(year in seq(250, 12000, 250)){
  print(year)
  fitness_fpe <- c()
  init <- TRUE
  
  for(s in c(1,2)){
    for(r in c(1:5)){
      print(r)
      sim_folder_sr <- paste0(output_folder, "/", "s",s,"_r", r)
      fitness <- readRDS(file.path(sim_folder_sr, paste0(year, "BP.rds")))
      if(init){
        fitness_fpe <- fitness
      }else{fitness_fpe <- cbind(fitness_fpe, fitness[,3])}
      init <- FALSE
    }
  }
  
  fitness_fpe <- cbind(fitness_fpe[1:2], rowMeans(fitness_fpe[3:12]))
  names(fitness_fpe)[3] <- "pred"
  
  saveRDS(fitness_fpe, file.path(output_folder, "ensemble", paste0(year, "BP.rds")))
}
