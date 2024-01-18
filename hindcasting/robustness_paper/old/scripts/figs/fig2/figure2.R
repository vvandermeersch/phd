
#-------------------------------#
# Fig.2: put together all parts #
#-------------------------------#

boxplots_grid <- plot_grid(NULL, boxplot_performance, boxplot_transferability, NULL, ncol = 1, 
                           rel_heights = c(0.02, 0.5,0.5,0.05), 
                           labels = c(NA, "b", "c", NA), label_fontfamily = "Helvetica Narrow", label_size = 11) +
  annotation_custom(grob = linesGrob(gp = gpar(col = "grey", lty = "dashed")), 
                    xmin = 0.55, xmax = 0.55, 
                    ymin = 0.07, ymax = 0.425) +
  annotation_custom(grob = linesGrob(gp = gpar(col = "grey", lty = "dashed")), 
                    xmin = 0.55, xmax = 0.55, 
                    ymin = 0.54, ymax = 0.895)

fig2_final_grid <- plot_grid(ordbetareg_plot, boxplots_grid, nrow = 1, rel_widths = c(0.6,0.4),
                             labels = c("a", NA), label_fontfamily = "Helvetica Narrow", label_size = 11)
  
  






