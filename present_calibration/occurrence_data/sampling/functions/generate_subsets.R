source(paste0(wd, 'functions/sample_presence_by_env.R')) # to sample records in environmental clusters
source(paste0(wd, 'functions/sample_absence.R')) # random sample of pseudo-absences
source(paste0(wd, 'functions/create_occurrence.R')) # merge presence and absence


generate_subsets <- function(N, npres, nabs, nclusters, occ_data, output_dir){
  
  sub_dir <- paste0(npres, "pres_", nabs, "abs")
  work_dir <- file.path(output_dir, sub_dir)
  dir.create(work_dir, showWarnings = FALSE)
  
  for(i in 1:N){
    
    fagussylvatica_pres <- sample_presence_by_env(occ_data, biovars_30y, k = nclusters, nb_samples = npres)
    fagussylvatica_abs <- sample_absence(occ_data, EUForest, ERA5land, nb_samples = nabs, env_data = biovars_30y)
    
    species_occurrence <- create_occurrence(fagussylvatica_pres, fagussylvatica_abs)
    
    save(species_occurrence, file=paste0(work_dir, "/occurrence_subset_", i, ".Rdata"))
    gc()
    
  }
  
}