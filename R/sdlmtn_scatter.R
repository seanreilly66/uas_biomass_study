

predictions <- 'data/ml_output/sdlmtn_transfer_learning_predictions_20220404_1018.csv' %>%
  read_csv()

stats <- 'data/ml_output/sdlmtn_transfer_learning_test_results_20220404_1018.csv' %>%
  read_csv()


sctr_plot = function(response_var_input, .data = predictions) {
  
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
    geom_abline(linetype = 'dashed', size = 0.6) +
    labs(x = NULL, y = NULL) +
    lims(x = c(min_lim, max_lim), y = c(min_lim, max_lim))
  
}


biomass = sctr_plot('biomass_sum') +
  labs(title = bquote('a) Biomass (R'^2~'= 0.69)'))

h = sctr_plot('h_mean')  +
  labs(title = bquote('b) Mean height (R'^2~'= 0.82)'))


cbh = sctr_plot('cbh') +
  labs(title = bquote('c) CBH (R'^2~'= 0.65)'))


cc = sctr_plot('cc') +
  labs(title = bquote('d) CC (R'^2~'= 0.05)'))


lai = sctr_plot('lai_mean')  +
  labs(title = bquote('f) LAI (R'^2~'= 0.31)'))


cbd = sctr_plot('cbd_mean')  +
  labs(title = bquote('e) CBD (R'^2~'= 0.22)'))




.data = predictions
response_var_input = 'biomass_sum'


sdl_fig <- ggarrange(
  biomass, h, cbh, cc, cbd, lai,
  nrow = 2,
  ncol = 3,
  widths = c(1, 1, 1),
  align = "hv") %>%
  annotate_figure(
    left = text_grob('Predicted values', family = 'serif', size = 16, rot = 90),
    bottom = text_grob('Field measurements', family = 'serif', size = 16))


sdl_fig







ggsave(
  filename = 'figures/manuscript/sdlmtn_scatter.png',
  width = 9,
  height = 5,
  units = 'in',
  dpi = 700
)
