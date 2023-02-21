options(stringsAsFactors = FALSE)

# loading the library
suppressPackageStartupMessages(library(disdat))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(forcats)) # for handling factor variables

# set output directory for prediction csv files:
outdir <- "models_output/ridge"
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

# load functions for generating quadratic terms
source("modelling_codes/prediction_helper.R")

n <- 0
for(r in regions){
  # reading presence-only and background species data for this region, one file per region:
  presences <- disPo(r)
  background <- read.csv(paste0("background_50k/", r, ".csv"))
  
  # extract names for all species
  species <- unique(presences$spid)
  
  # now for each species, prepare data for modeling and evaluation 
  # with randomForests, model, predict to the evaluation file with 
  # environmental data. 
  for(s in species){
    n <- n + 1
    
    # subset presence records of species for this species
    sp_presence <- presences[presences$spid == s, ]
    # add background data
    pr_bg <- rbind(sp_presence, background)
    
    # if species file exist go to the next species
    if(file.exists(paste0(outdir, "/", s, "_ridge.csv"))) next
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
    
    # # normalising covariates
    # for(v in covars[[r]]){
    #   if(!v %in% categoricalvars){
    #     meaanv <- mean(model_data[, v])
    #     sdv <- sd(model_data[, v])
    #     model_data[, v] <- (model_data[, v] - meaanv) / sdv
    #     evaluation[, v] <- (evaluation[, v] - meaanv) / sdv
    #   }
    # }
    
    
    myseed <- sum(model_data$occ) + 300 + n

    # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
    mod_ridge <- NULL
    
    # calculating the case weights (down-weighted or equal weights)
    prNum <- as.numeric(table(model_data$occ)["1"]) # number of presences
    bgNum <- as.numeric(table(model_data$occ)["0"]) # number of backgrounds
    wt <- ifelse(model_data$occ == 1, 1, prNum / bgNum)
    
    # function to creat quadratic terms for lasso and ridge
    quad_obj <- make_quadratic(model_data, cols = covars[[r]])
    # now we can predict this quadratic object on the training and testing data
    # this make two columns for each covariates used in the transformation
    training_quad <- predict.make_quadratic(quad_obj, newdata = model_data)
    testing_quad <- predict.make_quadratic(quad_obj, newdata = evaluation)
    # convert the data.frames to sparse matrices
    # select all quadratic (and non-quadratic) columns, except the y (occ)
    new_vars <- names(training_quad)[names(training_quad) != "occ"]
    training_sparse <- sparse.model.matrix(~. -1, training_quad[, new_vars])
    testing_sparse <- sparse.model.matrix( ~. -1, testing_quad[, new_vars])
    
    ptm <- proc.time()
    set.seed(myseed)
    # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
    if(inherits(try(
      mod_ridge <- glmnet::cv.glmnet(x = training_sparse,
                                     y = model_data$occ,
                                     family = "binomial",
                                     alpha = 0,
                                     weights = wt,
                                     standardize = TRUE)
    ), "try-error")){
      print(paste("Error for species ", s, "from", r))
    }
    t <- proc.time() - ptm
    
    # make and save the prediction
    out_file <- evaluation[, 1:4]
    out_file$spid <- sp_presence$spid[1]
    out_file$region <- r
    out_file$model <- "Ridge regression"
    out_file$prediction <- as.numeric(predict(mod_ridge, testing_sparse, type = "response", s = "lambda.min"))
    out_file$time <- t[3]
    
    write.csv(out_file, sprintf("%s/%s_ridge.csv", outdir, s), row.names = FALSE)
    print(n)
  }
}

sessionInfo()

