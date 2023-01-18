library(tidyverse)
library(lemon)

df <- 'data/ml_output/ml_result_comparison_method_20220311.csv' %>%
  read_csv() %>%
  mutate(across(c('metric', 'site', 'method', 'ml'), as.factor)) %>%
  mutate(Metric = fct_recode(
    metric,
    'LAI' = 'lai',
    'Mean height' = 'height',
    'Densiometer' = 'densiometer',
    'CBH' = 'cbh',
    'CBD' = 'cbd',
    'Biomass' = 'biomass'
  )) %>%
  mutate(
    Method = fct_recode(
      method,
      'ALS' = 'als',
      'UAS' = 'uas'
    )
  ) %>%
  mutate(
    site = fct_recode(
      site,
      'Jackson' = 'jcksn',
      'LaTour' = 'ltr',
      'Saddle Mountain' = 'sdlmtn',
      'Pepperwood' = 'ppwd'
    )
  ) %>%
  select(-method, - metric, -rmse)

df_2 <- df  %>%
  pivot_wider(names_from = Method, values_from = r2)

df_2 <- df_2 %>%
  select(-UAS) %>%
  filter(!is.na(ALS)) %>%
  left_join(
    
    df_2 %>%
      select(-ALS) %>%
      filter(!is.na(UAS))
    
  ) %>%
  mutate(dif = (UAS > ALS))




df_3 <- df  %>%
  pivot_wider(names_from = ml, values_from = r2) %>%
  rowwise() %>%
  mutate(xmin = min(ols, rf, svm, na.rm = TRUE),
         xmax = max(ols, rf, svm, na.rm = TRUE))
  


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

fig_full = ggplot(data = df %>%
         filter(site %in% c('Jackson', 'LaTour'))) + 
  geom_segment(
    data = df_2 %>%
      filter(site %in% c('Jackson', 'LaTour')),
    mapping = aes(
      x = ALS,
      y = Metric,
      xend = UAS,
      yend = Metric,
      color = dif
    ),
    linetype = 'dashed',
    size = 1
  ) +
  geom_point(
    mapping = aes(
      x = r2,
      y = Metric,
      shape = Method
    ),
    size = 4
  ) +
  scale_color_manual(values = c('firebrick', 'black'), guide = 'none') +
  xlim(0,1) +
  facet_rep_wrap(~ml+ site, nrow = 3, ncol = 2) +
  labs(x = bquote('R'^2)) +
  theme(
    legend.position = c(0.08, 0.1),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )

fig_full

ggsave(filename = 'figures/method_r2_comparison.png', plot = fig_full, height = 10, width = 8, dpi = 700)


fig_rf = ggplot(data = df %>%
                    filter(site %in% c('Jackson', 'LaTour'),
                           ml == 'rf')) + 
  geom_segment(
    data = df_2 %>%
      filter(site %in% c('Jackson', 'LaTour'),
             ml == 'rf'),
    mapping = aes(
      x = ALS,
      y = Metric,
      xend = UAS,
      yend = Metric,
      color = dif
    ),
    linetype = 'dashed',
    size = 1
  ) +
  geom_point(
    mapping = aes(
      x = r2,
      y = Metric,
      shape = Method
    ),
    size = 4
  ) +
  scale_color_manual(values = c('firebrick', 'black'), guide = 'none') +
  xlim(0,1) +
  facet_rep_wrap(~site, nrow = 1, ncol = 2) +
  labs(x = bquote('R'^2)) +
  theme(
    legend.position = c(0.08, 0.1),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )

fig_rf

ggsave(filename = 'figures/method_r2__rf_comparison.png', plot = fig_rf, height = 5, width = 10, dpi = 700)





fig_ols = ggplot(data = df %>%
                  filter(site %in% c('Jackson', 'LaTour'),
                         ml == 'ols')) + 
  geom_segment(
    data = df_2 %>%
      filter(site %in% c('Jackson', 'LaTour'),
             ml == 'ols'),
    mapping = aes(
      x = ALS,
      y = Metric,
      xend = UAS,
      yend = Metric,
      color = dif
    ),
    linetype = 'dashed',
    size = 1
  ) +
  geom_point(
    mapping = aes(
      x = r2,
      y = Metric,
      shape = Method
    ),
    size = 4
  ) +
  scale_color_manual(values = c('firebrick', 'black'), guide = 'none') +
  xlim(0,1) +
  facet_rep_wrap(~site, nrow = 1, ncol = 2) +
  labs(x = bquote('R'^2)) +
  theme(
    legend.position = c(0.08, 0.1),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )

fig_ols

ggsave(filename = 'figures/method_r2__ols_comparison.png', plot = fig_ols, height = 5, width = 10, dpi = 700)


fig_svm = ggplot(data = df %>%
                  filter(site %in% c('Jackson', 'LaTour'),
                         ml == 'svm')) + 
  geom_segment(
    data = df_2 %>%
      filter(site %in% c('Jackson', 'LaTour'),
             ml == 'svm'),
    mapping = aes(
      x = ALS,
      y = Metric,
      xend = UAS,
      yend = Metric,
      color = dif
    ),
    linetype = 'dashed',
    size = 1
  ) +
  geom_point(
    mapping = aes(
      x = r2,
      y = Metric,
      shape = Method
    ),
    size = 4
  ) +
  scale_color_manual(values = c('firebrick', 'black'), guide = 'none') +
  xlim(0,1) +
  facet_rep_wrap(~site, nrow = 1, ncol = 2) +
  labs(x = bquote('R'^2)) +
  theme(
    legend.position = c(0.08, 0.1),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )

fig_svm

ggsave(filename = 'figures/method_r2__svm_comparison.png', plot = fig_svm, height = 5, width = 10, dpi = 700)





fig_ml = ggplot(data = df %>%
                   filter(Method == 'UAS',
                          site == 'all')) + 
  geom_segment(
    data = df_3 %>%
      filter(site == 'all'),
    mapping = aes(
      x = xmin,
      y = Metric,
      xend = xmax,
      yend = Metric
    ),
    linetype = 'dashed',
    size = 0.6
  ) +
  geom_point(
    mapping = aes(
      x = r2,
      y = Metric,
      color = ml
    ),
    size = 4
  ) +
  scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
                     labels = c('OLS', 'Random Forest', 'SVM')) +
  xlim(0,1) +
  labs(x = bquote('R'^2),
       color = 'ML Method')

fig_ml

ggsave(filename = 'figures/ml_r2_comparison.png', plot = fig_ml, height = 3, width =6, dpi = 700)


fig_ml_site = ggplot(data = df %>%
                  filter(Method == 'UAS',
                         site != 'all')) + 
  geom_segment(
    data = df_3 %>%
      filter(Method == 'UAS',
             site != 'all'),
    mapping = aes(
      x = xmin,
      y = Metric,
      xend = xmax,
      yend = Metric
    ),
    linetype = 'dashed',
    size = 0.6
  ) +
  geom_point(
    mapping = aes(
      x = r2,
      y = Metric,
      color = ml
    ),
    size = 4
  ) +
  scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
                     labels = c('OLS', 'Random Forest', 'SVM')) +
  xlim(0,1) +
  labs(x = bquote('R'^2),
       color = 'ML Method') +
  facet_rep_wrap(~site, nrow = 2, ncol = 2) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    strip.text.x = element_text(hjust = 0.05)
  )
  

fig_ml_site

ggsave(filename = 'figures/ml_r2_site_comparison.png', plot = fig_ml_site, height = 6, width =10, dpi = 700)

