
variable <- "carbon"
var_fullname <- "Min. temp. coldest month"


response_data <- testing_data
response_data[,variable] <- runif(nrow(testing_data),
                                     min(testing_data[,variable]), max(testing_data[,variable]))


quad_obj <- make_quadratic(response_data, cols = covars)
response_quad <- predict.make_quadratic(quad_obj, newdata = response_data)
response_sparse <- sparse.model.matrix(~. -1, response_quad[, new_vars])
prediction <- as.numeric(predict(mod_lasso, response_sparse, type = "response", s = "lambda.min"))


data_plot <- data.frame(pred = prediction, var = response_data[,variable])
data_plot <- data_plot[order(data_plot$var),]
data_plot$group <- cut(seq_along(data_plot$var), 50, labels = FALSE)
ggplot(data = data_plot, aes(y = pred, x = var, group = group)) +
  geom_boxplot(notch=FALSE, outlier.shape=NA, fill="red", alpha=0.2) +
  theme_minimal() +
  ylab("Probability of presence") +
  xlab(var_fullname)


