library(tidyverse)
library(ggpubr)

predictions <- 'data/ml_output/rf_transfer_learning_predictions_20230301_1200.csv' %>%
  read_csv()

stats <- 'data/ml_output/rf_transfer_learning_test_results_20230301_1200.csv' %>%
  read_csv()

# ================================ GGplot theme ================================ 
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
    legend.margin = margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)

lm_eqn <- function(df, v) {
  eq <- substitute(~  ~ italic(R) ^ 2 ~ "=" ~ r2,
                   list(
                     r2 = df %>%
                       filter(response_var == v) %>%
                       pull(Rsquared) %>%
                       round(digits = 2)
                   ))
  as.character(as.expression(eq))
}

sctr_plot = function(response_var_input, .data = predictions, lab = stats) {
  
  min_lim = .data %>%
    filter(response_var == response_var_input) %>%
    select(predict_val, field_val) %>%
    min(na.rm = TRUE)
  
  max_lim = .data %>%
    filter(response_var == response_var_input) %>%
    select(predict_val, field_val) %>%
    max(na.rm = TRUE)
  
  ggplot(data = .data %>%
           filter(response_var == response_var_input),
         mapping = aes(x = field_val,
                       y = predict_val)) +
    geom_point(size = 3) +
    geom_abline(linetype = 'dashed', linewidth = 0.6) +
    labs(x = NULL, y = NULL) +
    lims(x = c(min_lim, max_lim), y = c(min_lim, max_lim)) +
    geom_label(aes(x = max_lim, 
                   y = min_lim, 
                   label = lm_eqn(df = lab, v = response_var_input)), 
               hjust = 'inward', 
               vjust = 'inward', 
               family = 'serif', 
               fontface = 'plain',
               label.size = 0,
               label.padding = unit(0.25, 'lines'),
               size = 5,
               parse = TRUE)
  
}




biomass = sctr_plot('biomass_sum') +
  labs(title = bquote('a) Biomass'))
# biomass

h = sctr_plot('h_mean')  +
  labs(title = bquote('b) Mean height'))
# h


cbh = sctr_plot('cbh') +
  labs(title = bquote('c) CBH'))
# cbh

cc = sctr_plot('densiometer_mean') +
  labs(title = bquote('d) CC'))
# cc

lai = sctr_plot('lai_mean')  +
  labs(title = bquote('f) LAI'))
# lai

cbd = sctr_plot('cbd_mean')  +
  labs(title = bquote('e) CBD'))
# cbd


sdl_fig <- ggarrange(
  biomass, h, cbh, cc, cbd, lai,
  nrow = 2,
  ncol = 3,
  widths = c(1, 1, 1),
  align = "hv") %>%
  annotate_figure(
    left = text_grob('Predicted values', family = 'serif', size = 16, rot = 90),
    bottom = text_grob('Field measurements', family = 'serif', size = 16))


# sdl_fig







ggsave(
  plot = sdl_fig,
  filename = 'figures/manuscript/rf_transfer_learning_scatter.png',
  width = 9,
  height = 5,
  units = 'in',
  dpi = 700
)

