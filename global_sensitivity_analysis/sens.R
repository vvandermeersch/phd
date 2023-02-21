







N <- 2 ^ 10
params <- paste("$x_", 1:10, "$", sep = "")
mat <- sobol_matrices(N = N, params = params)




R <- 10^3
type <- "percent"
conf <- 0.95
y <- sobol_Fun(mat)




population_growth_run <- function (dt) {
  return(mapply(population_growth, dt[, 1], dt[, 2], dt[, 3]))
}

