# Orbital parameters

# These 3 orbital parameters are available from the boundary conditions of E. Amstrong dataset (2019)
# https://www.nature.com/articles/s41597-019-0277-1/tables/2:
# ecc: eccentricity parameter
# pre: precession parameter
# xob: obliquity (tilt) (degrees)


# We just need to calculate this one:
# perh: longitude of perhelion
calculate_perh <- function(ecc, pre){
  
  pir <- pi/180
  
  perh <- 1/pir * asin(pre/ecc)

}












