library(tidyverse)
library(caret)
library(randomForest)
library(ggpubr)

ml <- readRDS('data/ml_output/rf_spatial_cluster_model_20230227_1528.RData')

varimp_plot <- function(.data, plot_label, slice_n = 5) {
  
  ml_varimp <- .data %>%
    varImp() %>%
    .$importance %>%
    rownames_to_column() %>%
    tibble() %>%
    rename(Predictor = rowname,
           Importance = Overall) %>%
    slice_max(order_by = Importance, n = slice_n) %>%
    arrange(desc(Importance)) %>%
    mutate(Predictor = factor(Predictor, levels = rev(.$Predictor)))
  
  vimp_plot = ggplot(
    data = ml_varimp
  ) +
    geom_point(
      mapping = aes(
        x = Importance,
        y = Predictor),
      size = 3
    ) + 
    geom_segment(
      mapping = aes(
        x = 0,
        xend = Importance,
        y = Predictor,
        yend = Predictor
      ),
      linetype = 'dashed',
      linewidth = 0.6
    ) +
    labs(x = NULL, 
         y = NULL,
         title = plot_label)
  
  return(vimp_plot)
  
}


theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(linewidth = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = ggplot2::margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)



lai <- ml$lai_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'f) LAI')

cbd <- ml$cbd_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'e) CBD')

cbh <- ml$cbh_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'c) CBH') +
  scale_x_continuous(labels = NULL)

cc <- ml$densiometer_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'd) CC')

h <- ml$h_mean_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'b) Mean height') +
  scale_x_continuous(labels = NULL)

biomass <- ml$biomass_sum_rf_rfe_spatial_folds %>%
  varimp_plot(plot_label = 'a) Biomass') +
  scale_x_continuous(labels = NULL)



varimp_fig <- ggarrange(
  biomass, h, cbh, cc, cbd, lai,
  nrow = 2,
  ncol = 3,
  widths = c(1, 1, 1),
  align = "hv") %>%
  annotate_figure(
    left = text_grob('Top variables', family = 'serif', size = 16, rot = 90),
    bottom = text_grob('                   Scaled variable importance', family = 'serif', size = 16))
  
varimp_fig

ggsave(
  filename = 'figures/manuscript/rf_variable_importance.png',
  width = 9,
  height = 5,
  units = 'in',
  dpi = 700
)
