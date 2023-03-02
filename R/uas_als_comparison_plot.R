library(tidyverse)
library(lemon)

input_df <- 'data/ml_output/ml_result_comparison_method_20220311.csv' %>%
  read_csv() %>%
  filter(ml == 'rf',
         site %in% c('jcksn', 'ltr')) %>%
  mutate(
    metric = metric %>%
      str_replace('biomass', '6') %>%
      str_replace('height', '5')%>%
      str_replace('cbh', '4') %>%
      str_replace('densiometer', '3') %>%
      str_replace('cbd', '2') %>%
      str_replace('lai', '1') %>%
      as.numeric(),
    site = site %>%
      str_replace('jcksn', '-0.15') %>%
      str_replace('ltr', '0.15') %>%
      as.numeric(),
    plot_index = metric + site
  ) %>%
  mutate(method = as.factor(method) %>%
           fct_recode(
             'ALS' = 'als',
             'UAS SfM' = 'uas'
           )) %>%
  rename(Method = method,
         Site = site)

min_max <- input_df %>%
  group_by(metric, Site) %>%
  summarize(min_r2 = min(r2, na.rm = T),
            max_r2 = max(r2, na.rm = T)) %>%
  mutate(plot_index = metric + Site) %>%
  mutate(Site = as.factor(Site))

input_df <- input_df %>%
  mutate(Site = as.factor(Site),
         Site = fct_relevel(Site, '0.15', '-0.15'))



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

fig_rf = ggplot(data = input_df) +
  geom_segment(
    data = min_max,
    mapping = aes(
      x = min_r2,
      y = plot_index,
      xend = max_r2,
      yend = plot_index,
      color = Site
    ),
    linetype = 'dashed',
    size = 1
  ) +
  geom_point(mapping = aes(
    x = r2,
    y = plot_index,
    shape = Method,
    color = Site
  ),
  size = 4) +
  scale_color_manual(values = c('firebrick', 'black'), 
                     breaks = c('0.15', '-0.15'),
                     labels = c('LaTour', 'Jackson'),
                     name = NULL) +
  scale_shape(name = NULL) +
  xlim(0,1) +
  scale_y_continuous(
    name = NULL,
    breaks = 1:6,
    labels = c('LAI', 'CBD', 'CC', 'CBH', 'Mean height', 'Biomass')) +
  guides(col = guide_legend(
    nrow = 2
  )) +
  labs(x = bquote(italic(R)^2)) +
  theme(
    legend.position = c(0.2, 0.86),
    legend.box = 'vertical'
  )

fig_rf

ggsave(filename = 'figures/manuscript/uas_als_rf_comparison.png', plot = fig_rf, height = 4.5, width = 4.5, dpi = 700)
