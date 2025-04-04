library(data.table) #fastest
setDTthreads(1) #only one thread (in case of parallel cmaes)

read_mean_outputvalue <- function(output_folder, output_var = "Fitness", points = NULL){
  if(is.null(points)){
    #read all file
    fitness <- fread(paste0(output_folder,"/", output_var, ".txt"), header=T, sep="\t", fill=T)
    fitness_mean <- matrix(ncol=ncol(fitness)-1, nrow=1, byrow=TRUE)
    
    fitness <- as.data.frame(fitness)
    fitness_mean[1,] <- apply(fitness[c(-1,-2),-1], 2, mean)
  }
  else{
    #select some points
    fitness <- fread(paste0(output_folder,"/", output_var, ".txt"), header=F, sep="\t", fill=T)
    col_names <- t(fitness[-1,1])
    fitness <- fitness[-1,-1]
    fitness <- as.data.frame(t(fitness))
    names(fitness) <- col_names
    names(fitness)[1:2] <- c("lat", "lon")
    fitness$lat <- round(as.numeric(fitness$lat), 1)
    fitness$lon <- round(as.numeric(fitness$lon), 1)
    fitness <- inner_join(points, fitness, by = c("lat", "lon")) %>% dplyr::select(-c('pres'))
    fitness <- as.data.frame(t(fitness))
    fitness_mean <- matrix(ncol=ncol(fitness), nrow=1, byrow=TRUE)
    fitness_mean[1,] <- apply(fitness[c(-1,-2),], 2, mean)
  }
  
  return(fitness_mean)
  
}
