options(stringsAsFactors = FALSE)

# loading the library
suppressPackageStartupMessages(library(disdat))
suppressPackageStartupMessages(library(mgcv))
suppressPackageStartupMessages(library(forcats)) # for handling factor variables

# set output directory for prediction csv files:
outdir <- "models_output/gam_iwlr"
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

# read GAM formulas
source("modelling_codes/vars.R")

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
    if(file.exists(paste0(outdir, "/", s, "_gam_iwlr.csv"))) next
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
    
    # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
    # 
    mod_gam <- NULL
    # calculating the case weights (IWLR)
    upwt <- (10^6)^(1 - model_data$occ)
    
    ptm <- proc.time()
    set.seed(myseed)
    # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
    if(inherits(try(
      mod_gam <- mgcv::gam(formula = as.formula(myform[[r]]), 
                           data = model_data,
                           family = binomial(link = "logit"),
                           weights = upwt,
                           method = "REML")
    ), "try-error")){
      print(paste("Error for species", s, "from", r))
    }
    t <- proc.time() - ptm
    
    # make and save the prediction
    out_file <- evaluation[, 1:4]
    out_file$spid <- sp_presence$spid[1]
    out_file$region <- r
    out_file$model <- "GAM-IWLR"
    out_file$prediction <- as.numeric(predict(mod_gam, evaluation, type = "response")) 
    out_file$time <- t[3]

    write.csv(out_file, sprintf("%s/%s_gam_iwlr.csv", outdir, s), row.names = FALSE)
    print(n)
  }
}

sessionInfo()

