# ==============================================================================
#
# ML metric plots
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 31 March 2022
# Last commit:
#
# Status: Needs documentation
#
# Created as part of 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Produces a series of plots 
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# tidyverse
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(tidyverse)
library(lemon)

# ================================= User inputs ================================

input_df_file <- ('data/ml_result_master.csv') 


# ==============================================================================
# ================================= Data Prep ================================== 
# ==============================================================================

input_df <- read_csv(input_df_file) %>%
  mutate(across(c('Field_Metric', 'Site', 'Method'), as.factor)) %>%
  mutate(Field_Metric  = fct_relevel(Field_Metric,
                                     'LAI', 'CBD', 'CC', 'CBH', 'Mean height', 'Biomass'))
min_max <- input_df %>%
  group_by(Field_Metric, Site) %>%
  summarize(min_r2 = min(R2, na.rm = T),
            max_r2 = max(R2, na.rm = T),
            min_rmse = min(RMSE, na.rm = T),
            max_rmse = max(RMSE, na.rm = T))


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
# ===================== Figure: All site metric comparison ===================== 
# ==============================================================================

ggplot() +
  geom_segment(
    data = min_max %>%
      filter(Site == 'All'),
    mapping = aes(
      x = min_r2,
      y = Field_Metric,
      xend = max_r2,
      yend = Field_Metric
    ),
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  geom_point(
    data = input_df %>%
      filter(Site == 'All'),
    mapping = aes(x = R2,
                  y = Field_Metric,
                  color = Method
    ),
    size = 4
  ) +
  scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
                     labels = c('LM Forward', 'Random Forest', 'SVM')) +
  xlim(0,1) +
  labs(x = bquote(italic(R)^2),
       color = NULL,
       y = NULL) +
  theme(legend.position = c(0.25, 0.8))

ggsave(
  filename = 'figures/manuscript/ml_r2_all_comparison.png',
  width = 5,
  height = 3.5,
  units = 'in',
  dpi = 700
)


# ==============================================================================
# ===================== Figure: Individual site metric comparison ===================== 
# ==============================================================================

ggplot(data = input_df %>%
         filter(Site != 'All')) +
  geom_segment(
    data = min_max %>%
      filter(Site != 'All'),
    mapping = aes(
      x = min_r2,
      y = Field_Metric,
      xend = max_r2,
      yend = Field_Metric
    ),
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  geom_point(
    data = min_max %>%
      filter(Site == 'All') %>%
      select(-Site),
    mapping = aes(x = max_r2,
                  y = Field_Metric,
                  shape = 'All site RF max'
    ),
    size = 4
  ) +
  geom_point(
    data = input_df %>%
      filter(Site != 'All'),
    mapping = aes(x = R2,
                  y = Field_Metric,
                  color = Method
    ),
    size = 4
  ) +
  scale_color_manual(values = c('black', 'firebrick', '#DDCC77'),
                     labels = c('LM Forward', 'Random Forest', 'SVM')) +
  xlim(0,1) +
  labs(x = bquote('R'^2),
       color = NULL,
       y = NULL) +
  theme(legend.position = 'bottom') +
  facet_rep_wrap(~ Site, nrow = 3, ncol = 2) +
  scale_shape_manual(values = 4, name = NULL) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    strip.text.x = element_text(hjust = 0.05, vjust = 1),
    panel.background = element_blank(),
  )

ggsave(
  filename = 'figures/manuscript/ml_r2_site_comparison.png',
  width = 9,
  height = 7,
  units = 'in',
  dpi = 700
)




