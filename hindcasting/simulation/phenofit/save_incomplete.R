
output_folder <- "D:/simulations/phenofit/paleo/expert/025deg/abies_alba"
dir.create(file.path(output_folder, "incomplete"))

for(year in seq(250, 12000, 250)){
  
  survival <- read_mean_outputvalue(file.path(output_folder, paste0(year, "BP")), output_var = "Survival")
  fruitindex <- read_mean_outputvalue(file.path(output_folder, paste0(year, "BP")), output_var = "FruitIndex")
  maturationindex <- read_mean_outputvalue(file.path(output_folder, paste0(year, "BP")), output_var = "MaturationIndex")
  
  fitness <- cbind(survival[,c(1,2)], fruitindex[,3]*maturationindex[,3])
  names(fitness)[3] <- "pred"
  
  saveRDS(fitness, file.path(output_folder, "incomplete", paste0(year, "BP.rds")))
}
