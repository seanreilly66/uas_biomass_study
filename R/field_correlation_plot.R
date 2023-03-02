# ==============================================================================
#
# Field data Correlation plot
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
# Produces a correlation plot for variable relationships. Based on ggcorrplot
# function but exploded out to allow for additional control over labels and 
# font types
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# tidyverse, reshape2
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(tidyverse)

# ================================= User inputs ================================

field_data_file <- 'data/field/plot_field_metrics.csv'


# ==============================================================================
# =============================== Data cleaning ================================ 
# ==============================================================================

# ---------------------------- Input data cleaning ----------------------------- 

input_df <- field_data_file %>%
  read_csv() %>%
  select(-ends_with(c('_n', '_na'))) %>%
  select(-plot, -campaign, -site) %>%
  rename(LAI = lai_mean,
         CBH = cbh,
         CBD = cbd_mean,
         Densiometer = densiometer_mean,
         Biomass = biomass_sum,
         Height = h_mean) %>%
  mutate(CC = 1-Densiometer) %>%
  select(-Densiometer)

# ==============================================================================
# ======================== Correlation matrix data prep ======================== 
# ==============================================================================


cormat <- input_df %>%
  cor(use = 'na.or.complete')

pmat <- input_df %>%
  psych::corr.test(use = 'na.or.complete')

dd <- stats::as.dist((1 - cormat)/2)
hc <- stats::hclust(dd, method = 'complete')

cormat <- cormat[hc$order, hc$order]

cormat[upper.tri(cormat)] <- NA
diag(cormat) <- NA

cormat <- reshape2::melt(cormat, na.rm = TRUE)

cormat <- cormat %>%
  rowwise() %>%
  mutate(
    labels = sprintf("%.2f", value)
  )



# ==============================================================================
# ================================= Plot Theme =================================
# ==============================================================================

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


# ==============================================================================
# ============================== Correlation plot ==============================
# ==============================================================================


ggplot(data = cormat,
       mapping = aes(
         x = Var1,
         y = Var2,
         fill = value,
         label = labels
       )) +
  geom_tile(color = 'white') +
  geom_text(
    family = 'serif',
    fontface = 'plain',
    size = 5
  ) +
  scale_fill_gradient(
    low = "white",  
    high = "#E46726",
    limit = c(0,1),
    name = 'R'
  ) +
  scale_x_discrete(limits = rev) +
  coord_fixed() +
  theme(legend.position = c(0.82, 0.75)) +
  labs(x = NULL, y = NULL)
 

ggsave(
  filename = 'figures/manuscript/metric_correlation.png',
  width = 4.2,
  height = 4.2,
  units = 'in',
  dpi = 700
)
