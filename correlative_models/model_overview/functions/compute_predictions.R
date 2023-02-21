compute_predictions <- function(data, fit, wd = NULL){
  
  if(fit$type == "lasso_glm"){
    quad_obj <- make_quadratic(data, cols = fit$covars)
    response_quad <- predict.make_quadratic(quad_obj, newdata = data)
    new_vars <- names(response_quad)[names(response_quad) != "pres"]
    response_sparse <- sparse.model.matrix(~. -1, response_quad[, new_vars])
    prediction <- as.numeric(predict(fit$model, response_sparse, type = "response", s = "lambda.min"))
    return(prediction)
  }
  if(fit$type == "gam"){
    # normalising covariates
    i <- 1
    for(v in fit$covars){
      data[, v] <- (data[, v] - fit$meanv_l[i]) / fit$sdv_l[i]
      i <- i + 1
    }
    prediction <- as.numeric(predict(fit$model, data, type = "response"))
    return(prediction)
  }
  if(fit$type == "maxent"){
    prediction <- predict(fit$model, data, args = c("outputformat=cloglog"))
    return(prediction)
  }
  if(fit$type == "brt"){
    # normalising covariates
    i <- 1
    for(v in fit$covars){
      data[, v] <- (data[, v] - fit$meanv_l[i]) / fit$sdv_l[i]
      i <- i + 1
    }
    prediction <- predict(fit$model, 
                          data, 
                          n.trees = fit$model$gbm.call$best.trees, 
                          type = "response")
    return(prediction)
  }
  if(fit$type == "random_forest"){
    # normalising covariates
    i <- 1
    for(v in fit$covars){
      data[, v] <- (data[, v] - fit$meanv_l[i]) / fit$sdv_l[i]
      i <- i + 1
    }
    prediction <- as.numeric(predict(fit$model, data, type = "prob")[,"1"])
    return(prediction)
  }
  if(fit$type == "biomod"){
    #setwd(wd)
    # normalising covariates
    i <- 1
    for(v in fit$covars){
      data[, v] <- (data[, v] - fit$meanv_l[i]) / fit$sdv_l[i]
      i <- i + 1
    }
    myBiomodProj <- BIOMOD_Projection(bm.mod = fit$models,
                                      new.env = as.data.frame(data[, fit$covars]),
                                      proj.name = "valavietal",
                                      selected.models = "all",
                                      binary.meth = "ROC",
                                      compress = TRUE,
                                      clamping.mask = TRUE)
    myBiomodEnProj <- BIOMOD_EnsembleForecasting(bm.proj = myBiomodProj,
                                                 bm.em = fit$model_ensemble,
                                                 selected.models = "all")
    myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))
    prediction <- myEnProjDF[,1]
    return(prediction)
  }
  
  
}