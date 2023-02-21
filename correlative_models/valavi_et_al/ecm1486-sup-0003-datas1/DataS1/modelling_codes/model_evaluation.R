# devtools::install_github("meeliskull/prg/R_package/prg")
library(prg)
library(precrec)
library(disdat)

# path to the folder contain all models output (in separate folders). 
modelsdir <- "models_output"
modelslist <- list.dirs(modelsdir, full.names = FALSE, recursive = FALSE)
print(modelslist)

# path to save the result of each model
outdir <- "summary_results"

for(i in modelslist){
  # get the names of all the prediction files
  sp_preds <- list.files(file.path(modelsdir, i), pattern = ".csv$", full.names = FALSE)
  
  # a data.frame to save the evaluation result
  mod_eval <- data.frame(region = rep(NA, length(sp_preds)), 
                         species = NA, 
                         roc = NA,
                         prg = NA,
                         cor = NA,
                         model = NA,
                         time = NA)
  
  # now a loop to evaluate each species, one at a time:
  n <- 0
  for(j in sp_preds){
    pred <- read.csv(file.path(modelsdir, i, j), stringsAsFactors = FALSE)
    # get information on the species (s) and region (r) for each file:
    r <- pred$region[1]
    s <- pred$spid[1]
    
    # now evaluate predictions, finding the right column for this species
    # find the evaluation file – for some regions this means identifying the taxonomic group
    if (r %in% c("AWT", "NSW")) {
      grp <- pred$group[1]
      evals <- disdat::disPa(r, grp)
    } else {
      evals <- disdat::disPa(r)
    }
    
    n <- n + 1
    # save the evaluation in the output data.frame
    mod_eval$region[n] <- r
    mod_eval$species[n] <- s
    mod_eval$roc[n] <- precrec::auc(precrec::evalmod(scores = pred$prediction, 
                                                     labels = evals[, s]))[1,4]
    mod_eval$prg[n] <- prg::calc_auprg(prg::create_prg_curve(labels = evals[, s], 
                                                             pos_scores = pred$prediction))
    mod_eval$cor[n] <- cor(pred$prediction, evals[, s])
    mod_eval$model <- i
    mod_eval$time[n] <- pred$time[1]
    print(s)
  }
  # save the data.frame of the evaluation
  write.csv(mod_eval, sprintf("%s/eval_%s.csv", outdir, i), row.names = FALSE)
}
