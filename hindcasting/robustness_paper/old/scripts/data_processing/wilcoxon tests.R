

model_performance_wilcox <- model_performance_withmig %>%
  group_by(mod, earlyholoc) %>%
  summarise(
    count = n(),
    mean_sorensen = mean(sorensen),
    mean_mig_sorensen =  mean(mig_sorensen),
    mean_tss = mean(tss)
  )


test <- wilcox.test(mean_sorensen ~ earlyholoc, data = model_performance_wilcox, paired = FALSE, alternative = "greater")
test$p.value < 0.0001

test <- wilcox.test(mean_tss ~ earlyholoc, data = model_performance_wilcox, paired = FALSE, alternative = "greater")
test$p.value < 0.0001


view <- model_performance_wilcox %>%
  group_by(mod)%>%
  mutate(
    diff_sorensen = mean_sorensen-lag(mean_sorensen, default=first(mean_sorensen)),
    diff_tss = mean_tss-lag(mean_tss, default=first(mean_tss))) %>%
  filter(earlyholoc) %>%
  ungroup() %>% 
  summarise(mean_diff_sorensen = mean(diff_sorensen), sd_diff_sorensen = sd(diff_sorensen),
            mean_diff_tss = mean(diff_tss), sd_diff_tss = sd(diff_tss))


