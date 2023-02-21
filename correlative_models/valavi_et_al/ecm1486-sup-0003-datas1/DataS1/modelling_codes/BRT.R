options(stringsAsFactors = FALSE)

# loading the library
suppressPackageStartupMessages(library(disdat))
suppressPackageStartupMessages(library(dismo))
suppressPackageStartupMessages(library(forcats)) # for handling factor variables

# set output directory for prediction csv files:
outdir <- "models_output/brt"
if(!file.exists(outdir)){
  dir.create(file.path(outdir))
  print("The directory is created")
}
# provide names for regions to be modeled - here we model all 6:
regions <- c("AWT", "CAN", "NSW", "NZ", "SA", "SWI")
# specify names of all categorical variables across all regions:
categoricalvars <- c("ontveg", "vegsys", "toxicats", "age", "calc")
# the list of uncorrelated variables
covars <- list(
  AWT = c("bc04",  "bc05",  "bc06",  "bc12",  "bc15",  "slope", "topo", "tri"),
  CAN = c("alt", "asp2", "ontprec", "ontslp", "onttemp", "ontveg", "watdist"), 
  NSW = c("cti", "disturb", "mi", "rainann", "raindq", "rugged", "soildepth", "soilfert", "solrad", "tempann", "topo", "vegsys"), 
  NZ = c("age", "deficit", "hillshade", "mas", "mat", "r2pet", "slope", "sseas", "toxicats", "tseas", "vpd"), 
  SA = c("sabio12", "sabio15", "sabio17", "sabio18", "sabio2", "sabio4", "sabio5", "sabio6"), 
  SWI = c("bcc", "calc", "ccc", "ddeg", "nutri", "pday", "precyy", "sfroyy", "slope", "sradyy", "swb", "topo")
)

n <- 0
for(r in regions){
  # reading presence-only and background species data for this region, one file per region:
  presences <- disPo(r)
  background <- read.csv(paste0("background_50k/", r, ".csv"))
  
  # extract names for all species
  species <- unique(presences$spid)
  
  # now for each species, prepare data for modeling and evaluation 
  for(s in species){
    n <- n + 1
    
    # subset presence records of species for this species
    sp_presence <- presences[presences$spid == s, ]
    # add background data
    pr_bg <- rbind(sp_presence, background)
    
    # if species file exist go to the next species
    if(file.exists(paste0(outdir, "/", s, "_brt.csv"))) next
    if(sum(pr_bg$occ) < 5) next
    
    # find the evaluation file – for some regions this means identifying the taxonomic group
    if(r %in% c("AWT", "NSW")) {
      grp <- sp_presence[, "group"][1]
      evaluation <- disEnv(r, grp)
    } else {
      evaluation <- disEnv(r)
    }
    
    # convert categorical vars to factor in both training and testing data. We use the package forcats to ensure that the levels of the factor in the evaluation data match those in the training data, regardless of whether all levels are present in the evaluation data. 
    for(i in 1:ncol(pr_bg)){
      if(colnames(pr_bg)[i] %in% categoricalvars){
        fac_col <- colnames(pr_bg)[i]
        pr_bg[ ,fac_col] <- as.factor(pr_bg[ ,fac_col])
        evaluation[ ,fac_col] <- as.factor(evaluation[ ,fac_col])
        evaluation[ ,fac_col] <- forcats::fct_expand(evaluation[,fac_col], levels(pr_bg[,fac_col]))
        evaluation[ ,fac_col] <- forcats::fct_relevel(evaluation[,fac_col], levels(pr_bg[,fac_col]))
      }
    }
    
    # extract the relevant columns for modelling
    model_data <- pr_bg[, c("occ", covars[[r]])]
    
    # normalising covariates
    for(v in covars[[r]]){
      if(!v %in% categoricalvars){
        meaanv <- mean(model_data[, v])
        sdv <- sd(model_data[, v])
        model_data[, v] <- (model_data[, v] - meaanv) / sdv
        evaluation[, v] <- (evaluation[, v] - meaanv) / sdv
      }
    }


    myseed <- sum(model_data$occ) + 300 + n
    set.seed(myseed)

    # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
    mod_brt <- NULL
    # calculating the case weights (down-weighted or equal weights)
    prNum <- as.numeric(table(model_data$occ)["1"]) # number of presences
    bgNum <- as.numeric(table(model_data$occ)["0"]) # number of backgrounds
    wt <- ifelse(model_data$occ == 1, 1, prNum / bgNum)
    ntrees <- 50
    tcomplexity <- ifelse(prNum < 50, 1, 5)
    lrate <- 0.001
    m <- 0
    while(is.null(mod_brt)){
      m <- m + 1
      if(m < 11){
        ntrees <- 50
        lrate <- 0.001
      } else if(m < 21){
        lrate <- 0.0001
      } else if(m < 31){
        ntrees <- 25
        lrate <- 0.0001
      } else if(m < 41){
        ntrees <- 25
        lrate <- 0.0001
        tcomplexity <- ifelse(prNum < 50, 1, 3)
      } else{
        break
      }
      ptm <- proc.time()
      if(inherits(try(
        mod_brt <- gbm.step(data = model_data,
                        gbm.x = 2:ncol(model_data),
                        gbm.y = 1, 
                        family = "bernoulli",
                        site.weights = wt,
                        tree.complexity = tcomplexity,
                        learning.rate = lrate,
                        n.trees = ntrees,
                        n.folds = 5,
                        max.trees = 10000)
      ), "try-error")){
        cat("Couldn't fit model", n, "in the iteration", m, "\n")
      }
      t <- proc.time() - ptm
    }
    if(is.null(mod_brt)){
      next
    }
    
    # make and save the prediction
    out_file <- evaluation[, 1:4]
    out_file$spid <- sp_presence$spid[1]
    out_file$region <- r
    out_file$model <- "BRT"
    out_file$prediction <- predict(mod_brt, 
                                   evaluation, 
                                   n.trees = mod_brt$gbm.call$best.trees, 
                                   type = "response")
    out_file$time <- t[3]
    
    write.csv(out_file, sprintf("%s/%s_brt.csv", outdir, s), row.names = FALSE)
    print(n)
  }
}

sessionInfo()

