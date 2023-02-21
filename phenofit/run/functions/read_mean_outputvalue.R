library(data.table) #fastest
setDTthreads(1) #only one thread (in case of parallel cmaes)

read_mean_outputvalue <- function(lambda, output_folder, output_var = "Fitness"){
  fitness <- fread(paste0(output_folder,as.character(1),"/", output_var, ".txt"), header=T, sep="\t", fill=T)
  fitness_mean <- matrix(ncol=ncol(fitness)-1, nrow=lambda, byrow=TRUE)
  
  fitness <- as.data.frame(fitness)
  fitness_mean[1,] <- apply(fitness[c(-1,-2),-1], 2, mean)
  if(lambda>1){
    for(i in 2:lambda){
      fitness <- fread(paste0(output_folder,as.character(1),"/", output_var, ".txt"), header=T, sep="\t")
      fitness <- as.data.frame(fitness)
      fitness_mean[i,] <- apply(fitness[c(-1,-2),-1], 2, mean)
    }
  }
  
  return(fitness_mean)
}