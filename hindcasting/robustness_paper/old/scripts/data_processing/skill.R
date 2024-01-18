
names(model_null_performance)[6] <- "null_tss"
names(model_null_performance)[12] <-"null_mig_tss"

model_skill <- left_join(model_null_performance[, c("species", "year", "null_tss", "null_mig_tss")], model_performance_withmig, 
                         multiple = "all", by = join_by(species, year))

model_skill$skill <- model_skill$mig_tss - model_skill$null_tss
model_skill$relchgskill <- (model_skill$mig_tss - model_skill$null_tss)/model_skill$null_tss
model_skill$skill_mig <- model_skill$mig_tss - model_skill$null_mig_tss



skill_plot <- ggplot(data = model_skill, aes(x=median, y=skill, col = type2, fill = type2)) +
  geom_point() +
  stat_smooth(method = "loess")

skillmig_plot <- ggplot(data = model_skill, aes(x=median, y=skill_mig, col = type2, fill = type2)) +
  geom_point() +
  stat_smooth(method = "loess")

relchgskill_plot <- ggplot(data = model_skill, aes(x=median, y=relchgskill, col = type2, fill = type2)) +
  geom_point() +
  stat_smooth(method = "loess")
