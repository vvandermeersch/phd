library(data.table) #fastest
setDTthreads(1) #only one thread (in case of parallel cmaes)

read_mean_outputvalue <- function(output_folder, output_var = "Fitness"){
  
  output <- fread(paste0(output_folder,"/", output_var, ".txt"), header=T, sep="\t", fill=T)
  output_mean <- apply(output[c(-1,-2),-1], 2, mean)
  output <- data.frame(lat = as.numeric(output[1,-1]), lon = as.numeric(output[2,-1]), value = output_mean)
  
  return(output)
}