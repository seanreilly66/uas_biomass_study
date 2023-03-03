library(tidyverse)
# library(lemon)

# ==============================================================================
# ================================= Data Prep ================================== 
# ==============================================================================

struct_df <- 'data/ml_output/rf_structural_metric_results_best_20230301_2210.csv' %>%
  read_csv() %>%
  add_column(data_type = 'Structural')

spec_df <- 'data/ml_output/rf_spectral_metric_results_best_20230301_1150.csv' %>%
  read_csv() %>%
  add_column(data_type = 'Raster-based spectral')

hspec_df <- 'data/ml_output/rf_spectral_height_metric_results_best_20230301_1648.csv' %>%
  read_csv() %>%
  add_column(data_type = 'Height band spectral')

input_df <- struct_df %>%
  add_row(spec_df) %>%
  add_row(hspec_df) %>%
  select(response_var, Rsquared, data_type) %>%
  rename(r2 = Rsquared) %>%
  mutate(across(c('response_var', 'data_type'), as.factor)) %>%
  mutate(response_var = fct_recode(
    response_var,
    'Biomass' = 'biomass_sum',
    'CBD' = 'cbd_mean',
    'CBH' = 'cbh',
    'CC' = 'densiometer_mean',
    'Mean height' = 'h_mean',
    'LAI' = 'lai_mean'
  ),
  response_var = fct_relevel(
    response_var,
    'LAI', 'CBD', 'CC', 'CBH', 'Mean height', 'Biomass'))

min_max <- input_df %>%
  group_by(response_var) %>%
  summarize(min_r2 = min(r2, na.rm = T),
            max_r2 = max(r2, na.rm = T))

full_dataset_df <- 'data/ml_output/rf_spatial_cluster_results_best_20230227_1528.csv' %>%
  read_csv() %>%
  add_column(data_type = 'Full dataset') %>%
  select(response_var, Rsquared, data_type) %>%
  rename(r2 = Rsquared) %>%
  mutate(across(c('response_var', 'data_type'), as.factor)) %>%
  mutate(response_var = fct_recode(
    response_var,
    'Biomass' = 'biomass_sum',
    'CBD' = 'cbd_mean',
    'CBH' = 'cbh',
    'CC' = 'densiometer_mean',
    'Mean height' = 'h_mean',
    'LAI' = 'lai_mean'
  ),
  response_var = fct_relevel(
    response_var,
    'LAI', 'CBD', 'CC', 'CBH', 'Mean height', 'Biomass'))

input_df <- input_df %>%
  add_row(full_dataset_df) %>%
  mutate(data_type = fct_relevel(
    data_type,
    'Full dataset', 'Structural', 'Raster-based spectral', 'Height band spectral')
  )


# ==============================================================================
# ================================= Plot Theme =================================
# ==============================================================================

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

# ==============================================================================
# ========================= Figure: Method comparison ========================== 
# ==============================================================================

fig_method = ggplot(data = input_df) +
  geom_segment(
    data = min_max,
    mapping = aes(
      x = min_r2,
      y = response_var,
      xend = max_r2,
      yend = response_var
    ),
    linetype = 'dashed',
    linewidth = 1
  ) +
  geom_point(mapping = aes(
    x = r2,
    y = response_var,
    color = data_type,
    shape = data_type,
    size = data_type
  )) +
  scale_color_manual(values = c('black', 'firebrick', '#40B0A6', '#DDCC77'),
                     name = NULL,
                     guide = guide_legend(nrow = 2)) +
  scale_shape_manual(values = c(4, 16, 16, 16),
                     name = NULL) +
  scale_size_manual(values = c(4, 5, 4, 4),
                    name = NULL) +
  labs(x = bquote(italic(R)^2),
       y = NULL) +
  lims(x = c(0,1)) +
  theme(
    legend.position = 'bottom'
  )

fig_method

ggsave(filename = 'figures/manuscript/ml_data_type_comparison.png', plot = fig_method, height = 4.5, width = 4.5, dpi = 700)
