test <- data.frame(subset = c(1,1,1,2,2,2), value = c(0.91,0.98,0.94,0.87,0.81,0.82))

test %>% ggplot(aes(y = value, x = factor(1), group = subset)) +
  geom_boxplot(width = 0.1) 
