

median_migtss <- model_performance_withmig %>%
  group_by(type2, crit) %>%
  summarize(median=median(mig_tss))

median_migtss$ymin <- ifelse(median_migtss$crit==TRUE, -4.35, 0)
median_migtss$ymax <- ifelse(median_migtss$crit==TRUE, 0, 4.35)


ggplot(data = model_performance_withmig, aes(fill = type2, group = mod)) +
  facet_rep_grid(rows = vars(type2)) + 

  geom_density(data = model_performance_withmig[model_performance_withmig$crit == TRUE,], 
               aes(x = mig_tss, y = -..density..), alpha = 0.35, size = 0.1, bins = 30, color = "white") +

  geom_density(data = model_performance_withmig[model_performance_withmig$crit == FALSE,], 
               aes(x = mig_tss, y = ..density..), alpha = 0.35, size = 0.1, bins = 30, color = "white") +
  # median
  geom_segment(data = median_migtss, aes(x = median, xend= median,
                                         y = ymin, yend = ymax,
                                         color = type2), linetype = "dashed", size = 0.6) +
  
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(-4,4,1),
                     name = "Density") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = round(seq(-0.2,0.8,0.2),1),
                     name = "TSS") +
  scale_color_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased', "4Fittedprocessbased"),
                     values= c( "#457b9d", "#82BCC4", "#e86117","#995D81")) +
  scale_fill_manual(breaks= c("1Treebased", "2Regressionbased", '3Expertprocessbased',  "4Fittedprocessbased"),
                    values= c( "#457b9d", "#82BCC4", "#e86117", "#995D81")) +
  
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        strip.background = element_blank(), strip.text = element_blank()) +
  coord_flip(xlim=c(-0.3, 0.8), ylim = c(-4.35,4.35))


model_performance_withmig %>%
  group_by(type2) %>%
  shapiro_test(sorensen)

ggdensity(model_performance_withmig$sorensen, fill = "lightgray")
# QQ plot
ggqqplot(model_performance_withmig$sorensen)

shapiro_test(model_performance_withmig$sorensen)

k <- 0.6
test <- log(model_performance_withmig$mig_tss/mean(model_performance_withmig$mig_tss)+k)
test <- (model_performance_withmig$mig_tss+1)^3
ggdensity(test, fill = "lightgray")
ggqqplot(test)
shapiro_test(test)

k <- 78
test <- log(model_performance_withmig$sorensen)
ggdensity(test, fill = "lightgray")

summary(lm(test ~ model_performance_withmig$median*(1|model_performance_withmig$type)))

model_performance_withmig$test <- test

lme(test ~ median, random = ~ as.factor(type), data = model_performance_withmig)

modeltest <- lmer(test ~ (0 + median | type2), data = model_performance_withmig)
summary(modeltest)
dotplot(ranef(modeltest, condVar=T))

plot_model(modeltest, type = "re", show.values = TRUE)


#fit the model
m_slp <- lmer(test ~ (0 + median | type2), data = model_performance_withmig, REML=FALSE)
plot_model(m_slp, type = "diag", show.values = TRUE)
#the next line put all the estimated intercept and slope per subject into a dataframe
reaction_slp <- as.data.frame(t(apply(ranef(m_slp)$type2, 1,function(x) x)))  

#to get the predicted regression lines we need one further step, writing the linear equation: Intercept + Slope*Days with different coefficient for each subject
new_median <- seq(0.8,1.8,0.01)
pred_slp <- rbind(
  data.frame(median = new_median, test = fixef(m_slp) + reaction_slp$`1Treebased`*new_median, type2 = "1Treebased"),
  data.frame(median = new_median, test = fixef(m_slp) + reaction_slp$`2Regressionbased`*new_median, type2 = "2Regressionbased"),
  data.frame(median = new_median, test = fixef(m_slp) + reaction_slp$`3Expertprocessbased`*new_median, type2 = "3Expertprocessbased"),
  data.frame(median = new_median, test = fixef(m_slp) + reaction_slp$`4Fittedprocessbased`*new_median, type2 = "4Fittedprocessbased")
  )



#plot with actual data 
ggplot(pred_slp,aes(x=median,y=test^(1/3)-1,color=type2))+
  geom_line()+
  geom_point(data=model_performance_withmig,aes(x=median,y=mig_tss))+
  facet_wrap(~type2,nrow=3)
