get_response_curve <- function(variable, var_fullname, fit, data, wd = NULL){
  
  response_data <- data %>%
    dplyr::select(all_of(fit$covars))
  response_data[,variable] <- runif(nrow(data),
                                    min(data[,variable]), max(data[,variable]))
  
  prediction <- compute_predictions(response_data, fit, wd)
  
  data_plot <- data.frame(pred = prediction, var = response_data[,variable])
  data_plot <- data_plot[order(data_plot$var),]
  data_plot$group <- cut(seq_along(data_plot$var), 40, labels = FALSE)
  
  plot <- ggplot(data = data_plot, aes(y = pred, x = var, group = group)) +
    geom_boxplot(notch=FALSE, outlier.shape=NA, fill="red", alpha=0.2, coef = 0) +
    theme_minimal() +
    ylab("Probability of presence") +
    xlab(var_fullname)
  
  return(plot)
  
}