
#----------------------------------------------#
# Fig.3: model performance across the Holocene #
#----------------------------------------------#

upper_panel <- plot_grid(ordbetareg_plot + theme(legend.position = "none"), 
                         boxplot_transferability, 
                         ncol = 2, rel_widths = c(0.55,0.4), align = "hv", axis = "tblr",
                         labels = c("a", "b"), label_fontfamily = "Helvetica Narrow", label_size = 11)

lower_panel <- plot_grid(boxplot_performance, NULL, boxplot_sdperformance, get_legend(ordbetareg_plot),
                         ncol = 4, rel_widths = c(0.53, 0.04, 0.33, 0.3), align = "hv", axis = "tblr",
                         labels = c("c", NA, "d", NA), label_fontfamily = "Helvetica Narrow", label_size = 11)


fig3_main <- plot_grid(upper_panel, lower_panel, ncol = 1, rel_heights = c(1.3, 1))
