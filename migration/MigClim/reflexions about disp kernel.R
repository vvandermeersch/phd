
gaussian_disp <- function(coords){
  
  a_sdd <- 25/2
  
  d <- sqrt(sum((coords)^2))
  
  pdf <- (1/(pi*a_sdd^2) * exp(-(d/a_sdd)^2))
  
  return(pdf)
  
}

# centroïd to area, centroïd in c(0,0) cartesian coordinates, mx distance = 50
adaptIntegrate(gaussian_disp, lower = c(-25,-25), upper = c(25,25))$integral
4 * adaptIntegrate(gaussian_disp, lower = c(0,0), upper = c(25,25))$integral
adaptIntegrate(gaussian_disp, lower = c(-50,-50), upper = c(50,50))$integral-adaptIntegrate(gaussian_disp, lower = c(-25,-25), upper = c(25,25))$integral

# but we don't take into account the probability of falling in the source cell (species with short dispersal)
adaptIntegrate(gaussian_disp, lower = c(-37.5,-37.5), upper = c(37.5,37.5))$integral-adaptIntegrate(gaussian_disp, lower = c(-12.5,-12.5), upper = c(12.5,12.5))$integral
adaptIntegrate(gaussian_disp, lower = c(-62.5,-62.5), upper = c(62.5,62.5))$integral-adaptIntegrate(gaussian_disp, lower = c(-37.5,-37.5), upper = c(37.5,37.5))$integral



# in the same spirit as Engler et al. (ie CDF)
adaptIntegrate(gaussian_disp, lower = c(-Inf,-Inf), upper = c(Inf,Inf))$integral
1 - adaptIntegrate(gaussian_disp, lower = c(-25,-25), upper = c(25,25))$integral

# in the same spirit as Engler et al. (ie CDF), corrected for source cell
1 - adaptIntegrate(gaussian_disp, lower = c(-12.5,-12.5), upper = c(12.5,12.5))$integral
1 - adaptIntegrate(gaussian_disp, lower = c(-37.5,-37.5), upper = c(37.5,37.5))$integral






gaussian_disp_p2a <- function(coords, coords_s){
  
  a_sdd <- 25/2
  
  d <- sqrt(sum((coords - coords_s)^2))
  
  pdf <- (1/(pi*a_sdd^2) * exp(-(d/a_sdd)^2))
  
  return(pdf)
  
}


# don't take into account the probability of falling in the source cell
4*adaptIntegrate(gaussian_disp, lower = c(12.5,-12.5), upper = c(37.5,12.5))$integral+
  4*adaptIntegrate(gaussian_disp, lower = c(12.5,12.5), upper = c(37.5,37.5))$integral


adaptIntegrate(gaussian_disp_p2a, lower = c(-25,-25), upper = c(0,0), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(-25, 0), upper = c(0,25), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(-25,25), upper = c(0,50), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(0,-25), upper = c(25,0), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(0, 25), upper = c(25,50), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(25,-25), upper = c(50,0), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(25,0), upper = c(50,25), coords_s = c(12.5, 12.5))$integral +
  adaptIntegrate(gaussian_disp_p2a, lower = c(25,25), upper = c(50,50), coords_s = c(12.5, 12.5))$integral
  

gaussian_disp_a2a <- function(coords, res){
  
  p <- adaptIntegrate(gaussian_disp_p2a, lower = c(-res,-res), upper = c(0,0), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(-res, 0), upper = c(0,res), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(-res,res), upper = c(0,2*res), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(0,-res), upper = c(res,0), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(0, res), upper = c(res,2*res), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(res,-res), upper = c(2*res,0), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(res,0), upper = c(2*res,res), coords_s = c(coords[1], coords[2]))$integral +
    adaptIntegrate(gaussian_disp_p2a, lower = c(res,res), upper = c(2*res,2*res), coords_s = c(coords[1], coords[2]))$integral
  
  return(p)
  
}

gaussian_disp_a2a(c(50, 50), res = 1000)
gaussian_disp_a2a(c(12.5, 12.5), res = 25)

gaussian_disp_a2a(c(50, 50), res = 1000)

res = 1000
adaptIntegrate(gaussian_disp_a2a, lower = c(0,0), upper = c(res,res), res = res)$integral* 1/(res^2)
adaptIntegrate(gaussian_disp_a2a_t2, lower = c(0,0), upper = c(res,res), res = res)$integral* 1/(res^2)








gaussian_disp_a2a_t2 <- function(coords, res){
  
  p <- adaptIntegrate(gaussian_disp_p2a, lower = c(-res,-res), upper = c(2*res,2*res), coords_s = c(coords[1], coords[2]))$integral -
    adaptIntegrate(gaussian_disp_p2a, lower = c(0,0), upper = c(res,res), coords_s = c(coords[1], coords[2]))$integral
  
  return(p)
  
}



