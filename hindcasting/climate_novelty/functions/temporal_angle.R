temporal_angle <- function(focal_stack){
  
  focal_stack[focal_stack > 0] <- 1
  focal_stack[focal_stack < 0] <- 0
  
  temporal_angle <- sum(focal_stack)
  temporal_angle[temporal_angle == 2 | temporal_angle == 0] <- 0
  temporal_angle[temporal_angle == 1] <- 180
  
  return(temporal_angle)
}