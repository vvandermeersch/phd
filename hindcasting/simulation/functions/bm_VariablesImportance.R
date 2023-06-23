
bm_VariablesImportance <- function (bm.model, expl.var, variables, method = "full_rand", 
          nb.rep = 1, seed.val = NULL, do.progress = TRUE, ...) 
{
  
  ref <- try(as.numeric(predict(bm.model, newdata = expl.var, temp_workdir = temp_workdir, 
                     seedval = seed.val)))
  if (inherits(ref, "try-error")) {
    stop("Unable to make model prediction")
  }

  if (do.progress) {
    PROGRESS = txtProgressBar(min = 0, max = nb.rep * length(variables), 
                              style = 3)
    i.iter = 0
  }
  out = foreach(r = 1:nb.rep, .combine = "rbind") %:% foreach(v = variables, 
                                                              .combine = "rbind") %do% {
                                                                data_rand <- .randomise_data(expl.var, v, method)
                                                                shuffled.pred <- as.numeric(predict(bm.model, data_rand, temp_workdir = temp_workdir, 
                                                                                         seedval = seed.val))
                                                                out_vr <- 1 - max(round(cor(x = ref, y = shuffled.pred, 
                                                                                            use = "pairwise.complete.obs", method = "pearson"), 
                                                                                        digits = 6), 0, na.rm = TRUE)
                                                                if (do.progress) {
                                                                  i.iter = i.iter + 1
                                                                  setTxtProgressBar(pb = PROGRESS, value = i.iter)
                                                                }
                                                                return(data.frame(expl.var = v, rand = r, var.imp = out_vr))
                                                              }
  if (do.progress) {
    close(PROGRESS)
  }
  return(out)
}



.randomise_data <- function(expl.var, variable, method, seedval = NULL)
{
  if (method == 'full_rand') {
    return(.full_shuffling(expl.var, variable, seedval))
  }
}

.full_shuffling <- function(x, id = NULL, seedval = NULL)
{
  if (!(is.vector(x) | is.matrix(x) | is.data.frame(x))) {
    stop("x must be a 1 or 2 dimension odject")
  }
  
  ## Set a new random seed to ensure that sampling is random
  ## (issue when CTA is involved and seed needs to be set to a fix number)
  if (is.null(seedval)) {
    set.seed(as.double(Sys.time()) + as.numeric(format(Sys.time(), "%OS6")) * 1000000)
  }
  
  out <- NULL
  if (is.null(id)) {
    out <- x[sample.int(length(x))]
  } else {
    out <- x
    for (idd in id) { out[, idd] <- out[sample.int(nrow(x)), idd]  }
  }
  
  return(out)
}
