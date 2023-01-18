library(tidyverse)
library(lemon)

csv_files <- list.files('data/ml_output', pattern = 'lm_testing_results_best', full.names = TRUE)

df <- read_csv(csv_files[1]) %>%
  add_row(read_csv(csv_files[2])) %>%
  add_row(read_csv(csv_files[3])) %>%
  add_row(read_csv(csv_files[4])) %>%
  add_row(read_csv(csv_files[5])) %>%
  add_row(read_csv(csv_files[6])) %>%
  mutate(across(c('response_var', 'method', 'pre_process', 'cor_threshold'), as.factor)) %>%
  mutate(Metric = fct_recode(
    response_var,
    'LAI' = 'lai_mean',
    'Mean height' = 'h_mean',
    'Densiometer' = 'densiometer_mean',
    'CBH' = 'cbh',
    'CBD' = 'cbd_mean',
    'Biomass' = 'biomass_sum'
  )) %>%
  filter(pre_process == 'none') %>%
  mutate(cor_threshold = fct_recode(
    cor_threshold,
    'R Threshold: 0.75' = '0.75'
  ))

# df_2 <- df  %>%
#   pivot_wider(names_from = Method, values_from = r2)
# 
# df_2 <- df_2 %>%
#   select(-UAS) %>%
#   filter(!is.na(ALS)) %>%
#   left_join(
#     
#     df_2 %>%
#       select(-ALS) %>%
#       filter(!is.na(UAS))
#     
#   ) %>%
#   mutate(dif = (UAS > ALS))
# 
# 
# 
# 
# df_3 <- df  %>%
#   pivot_wider(names_from = ml, values_from = r2) %>%
#   rowwise() %>%
#   mutate(xmin = min(ols, rf, svm, na.rm = TRUE),
#          xmax = max(ols, rf, svm, na.rm = TRUE))
  


theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(size = 1),
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


fig_ml = ggplot(data = df) + 
  # geom_segment(
  #   data = df_3 %>%
  #     filter(site == 'all'),
  #   mapping = aes(
  #     x = xmin,
  #     y = Metric,
  #     xend = xmax,
  #     yend = Metric
  #   ),
  #   linetype = 'dashed',
  #   size = 0.6
  # ) +
  geom_point(
    mapping = aes(
      x = Rsquared,
      y = Metric,
      color = method
    ),
    size = 4
  ) +
  scale_color_brewer(type = 'qual') +
  # scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
                     # labels = c('OLS', 'Random Forest', 'SVM')) +
  xlim(0,1) +
  labs(x = bquote('R'^2),
       color = 'LM Method') +
  facet_rep_wrap(~cor_threshold, nrow = 2, ncol = 3) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )

fig_ml

ggsave(filename = 'figures/linear_r2_comparison.png', plot = fig_ml, height = 4, width =13, dpi = 700)




fig_09 = ggplot(data = df %>%
                  filter(cor_threshold == 0.9)) +
  geom_point(mapping = aes(x = Rsquared,
                           y = Metric,
                           color = method),
             alpha = 0.5,
             size = 4) +
  scale_color_brewer(type = 'qual') +
  # scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
  # labels = c('OLS', 'Random Forest', 'SVM')) +
  xlim(0, 1) +
  labs(x = bquote('R' ^ 2),
       color = 'LM Method') +
  facet_rep_wrap( ~ cor_threshold, nrow = 2, ncol = 3) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )

fig_09


ggsave(filename = 'figures/linear_09_r2_comparison.png', plot = fig_09, height = 4, width =6, dpi = 700)

