library(cubature) # multidimensional integration

# coords is a vector, coords = c(x_source, y_source, x_dest, y_dest)

euclidean <- function(a, b) sqrt(sum((a - b)^2))

negexp_pdf <- function(coords, a_sdd, a_ldd, pldd){
  
  d <- euclidean(coords[1:2], coords[3:4])
  
  pdf <- (1-pldd)*(1/(pi*a_sdd^2) * exp(- d/a_sdd)) + pldd * (1/(pi*a_ldd^2) * exp(- d/a_ldd)) # a = d/2
  return(pdf)
  
}

prob_disp <- function(n, res, pdf, ...){
  
  p <- adaptIntegrate(pdf, lower = c(0,0,n*res,n*res), upper = c(res,res,(n+1)*res,(n+1)*res), ...)

  
  return(p$integral * 1/(res^2)) #location of source: represented as a uniform distribution, prob = 1/res^2
  
}

prob_disp2 <- function(n, res, pdf, ...){
  
  p <- adaptIntegrate(pdf, lower = c(0,0,n*res,n*res), upper = c(res,res,Inf,Inf), ...)
  
  
  return(p$integral * 1/(res^2)) #location of source: represented as a uniform distribution, prob = 1/res^2
  
}

# prob_disp(n = 1, res = 1000, pdf = negexp_pdf, a_sdd = 25/2, a_ldd = 200/2, pldd = 0.01)









