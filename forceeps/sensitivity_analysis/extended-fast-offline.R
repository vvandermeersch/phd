# Test offline fast99

response <- function(x, loop = FALSE, other_types_allowed = FALSE, ...) {
  id <- deparse(substitute(x))
  
  if (class(x$model) == "function") {
    if (loop) {
      n <- nrow(x$X)
      y <- sapply(1:n, function(i){
        x$model(x$X[i,], ...)
      }, simplify = "array")
      if(is.matrix(y)){
        if(typeof(y) == "list"){
          stop("The model function returns a list when applied to a row of x$X")
        } else{
          # This means the model function returned vectors, so y is a 
          # (m times n)-matrix (with unknown m). For better consistency with the 
          # default case (that y is a vector of length n), y is transposed to a 
          # (n times m)-matrix.
          y <- t(y)
        }
      } else if(is.array(y) && length(dim(y)) == 3){
        # This means the model function returned matrices of the same sizes.
        # Let m be the number of rows and z be the number of columns.
        # For better consistency with the other cases (y is a vector or a 
        # matrix), y is transformed to an array of dimensions c(n, m, z).
        
        # Change the order of the dimensions:
        y <- aperm(y, perm = c(3, 1, 2))
      } else if(!is.numeric(y)){
        stop("x$model returned an object that can't be handled")
      }
    } else {
      y <- x$model(x$X, ...)
    }
  } else if (TRUE %in% (paste("predict.", class(x$model), sep="") %in% methods(predict))) {
    y <- predict(x$model, x$X, ...)
  } else {
    stop("The model isn't a function or does not have a predict method")
  }
  
  if(other_types_allowed){
    #    if (!class(y)[1] %in% c("numeric", "matrix", "array") ||
    if ((!inherits(y, "numeric") && !inherits(y, "matrix") && !inherits(y, "array")) ||
        (is.array(y) && typeof(y) == "list")) {
      y <- as.numeric(y)
      warning("Conversion of the response to numeric")
    } else if(inherits(y, "array") && length(dim(y)) > 3){
      stop("If the model returns an array, it must not have more ",
           "than 3 dimensions")
    }
  } else{
    if(!inherits(y, "numeric")) {
      y <- as.numeric(y)
      warning("Conversion of the response to numeric")
    }
  }
  
  # Assign column names resp. dimnames if not existing:
  if(inherits(y, "matrix") && 
     (is.null(colnames(y)) || any(nchar(colnames(y)) == 0))){
    colnames(y) <- paste0("ycol", 1:ncol(y))
  } else if(inherits(y, "array")){
    if(is.null(dimnames(y))){
      dimnames(y) <- list(NULL, paste0("ycol", 1:dim(y)[2]), 
                          paste0("ydim3_", 1:dim(y)[3]))
    } else{
      if(is.null(dimnames(y)[[2]]) || any(nchar(dimnames(y)[[2]]) == 0)){
        dimnames(y)[[2]] <- paste0("ycol", 1:dim(y)[2])
      }
      if(is.null(dimnames(y)[[3]]) || any(nchar(dimnames(y)[[3]]) == 0)){
        dimnames(y)[[3]] <- paste0("ydim3_", 1:dim(y)[3])
      }
    } 
  }
  
  x$y <- y
  assign(id, x, parent.frame())
}

model <- forceeps_model_gsa_auc
factors <- parameters 
n <- 800 
q <- rep("qunif", 11)
q.arg <- lapply(1:n_parameters, function(i){return(list(min = parameters_lb[i], max = parameters_ub[i]))})
omega <- NULL
M <- 4


# factors numbers and names

if (is.character(factors)) {
  X.labels <- factors
  p <- length(X.labels)
} else {
  p <- factors
  X.labels <- paste("X", 1 : p, sep = "")
}

# quantiles

if (is.null(q)) {
  q <- rep("qunif", p)
} else if (length(q) == 1) {
  q <- rep(q, p)
}
if (is.null(q.arg)) {
  q.arg <- rep(list(), p)
} else if (FALSE %in% sapply(q.arg, is.list)) { # q.arg isn't a list of lists
  q.arg <- rep(list(q.arg), p)
}

# set of frequencies

if (is.null(omega)) {
  omega <- numeric(p)
  omega[1] <- floor((n - 1) / (2 * M))
  m <- floor(omega[1] / (2 * M))
  if (m >= p - 1) {
    omega[-1] <- floor(seq(from = 1, to = m, length.out = p - 1))
  } else {
    omega[-1] <- (0 : (p - 2)) %% m + 1
  }
}

# discretization of the s-space

s <- 2 * pi / n * (0 : (n - 1))

# transformation to get points in the x-space

X <- as.data.frame(matrix(nrow = n * p, ncol = p))
colnames(X) <- X.labels
omega2 <- numeric(p)
for (i in 1 : p) {
  omega2[i] <- omega[1]
  omega2[-i] <- omega[-1]
  l <- seq((i - 1) * n + 1, i * n)
  for (j in 1 : p) {
    g <- 0.5 + 1 / pi * asin(sin(omega2[j] * s))
    X[l, j] <- do.call(q[j], c(list(p = g), q.arg[[j]]))
  }
}

# object of class "fast99"

x <- list(model = model, M = M, s = s, omega = omega, X = X,
          call = match.call())
class(x) <- "fast99"


response(x)


pattern <- "\\.siteproductivity.txt$"
files <- list.files(path = output_path, pattern = pattern, recursive = T, full.names = TRUE)
print(length(files))
cnames <- c("date", "patchId", "speciesId", "speciesShortName", "adultProdBasalArea", "adultProdBiomass", "adultTreeBasalArea", "adultTreeBiomass", 
            "deadBasalArea", "deadBiomass", "saplingBasalArea", "saplingBiomass", "adultTreeNumber", "deadNumber", "saplingNumber", "droughtIndexAnnual",
            "droughtIndexSeasonal")
nyears <- commandfile_options$numberOfYears
yearly_results <- vroom(files, show_col_types = FALSE, progress=FALSE, skip = 9, col_names=cnames) %>%
  dplyr::select(var)
mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), 
                                                   each = nyears, len = nrow(yearly_results))), mean)[-1]
mean_results <- matrix(unlist(mean_results), ncol=length(commandfile_options$grid), byrow = T)
obs <- as.factor(Yobs$pres)
auc_pred <- sapply(1:nrow(mean_results), FUN= function(x){
  pred <- mean_results[x,]
  roc_pred <- roc(pred, obs)
  auc(roc_pred)
})
y <- auc_pred


tell(x)

return(x)