


niche_similarity_abies<- ggplot(data = niche_similarity[niche_similarity$species == "abies_alba",], 
                                 aes(x = 1-clim_sorensen, y = sorensen, color = models, fill = models)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(breaks= c('phenofit_vs_random_forest', "phenofit_vs_phenofit_fitted", "phenofit_fitted_vs_random_forest"),
                     values= c("#e86117", "#457b9d", "#bf4628"),
                     labels = c("PHENOFIT / Random Forest", "PHENOFIT / PHENOFIT(fitted)", "PHENOFIT(fitted) / Random Forest")) +
  scale_fill_manual(breaks= c('phenofit_vs_random_forest', "phenofit_vs_phenofit_fitted", "phenofit_fitted_vs_random_forest"),
                    values= c("#e86117", "#457b9d", "#bf4628"),
                    labels = c("PHENOFIT / Random Forest", "PHENOFIT / PHENOFIT(fitted)", "PHENOFIT(fitted) / Random Forest")) +
  theme_bw() +
  ylim(0.45, 0.92) +
  labs(y = "Niche hypervolume similarity", x= "Climate hypervolume dissimilarity") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank()) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE), color=guide_legend(nrow=3,byrow=TRUE))



niche_similarity_fagus <- ggplot(data = niche_similarity[niche_similarity$species == "fagus_sylvatica",], 
                                 aes(x = 1-clim_sorensen, y = sorensen, color = models, fill = models)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_color_manual(breaks= c('phenofit_vs_random_forest', "phenofit_vs_phenofit_fitted", "phenofit_fitted_vs_random_forest"),
                     values= c("#e86117", "#457b9d", "#bf4628"),
                     labels = c("PHENOFIT / Random Forest", "PHENOFIT / PHENOFIT(fitted)", "PHENOFIT(fitted) / Random Forest")) +
  scale_fill_manual(breaks= c('phenofit_vs_random_forest', "phenofit_vs_phenofit_fitted", "phenofit_fitted_vs_random_forest"),
                    values= c("#e86117", "#457b9d", "#bf4628"),
                    labels = c("PHENOFIT / Random Forest", "PHENOFIT / PHENOFIT(fitted)", "PHENOFIT(fitted) / Random Forest")) +
  theme_bw() +
  ylim(0.45, 0.92) +
  labs(y = "Niche hypervolume similarity", x= "Climate hypervolume dissimilarity") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())+
  guides(fill=guide_legend(nrow=3,byrow=TRUE), color=guide_legend(nrow=3,byrow=TRUE))


