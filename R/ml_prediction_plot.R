# =================================== Libraries ================================
library(tidyverse)
library(randomForest)
library(caret)
library(foreach)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

ml_models <- readRDS(
  'data/ml_output/rf_spatial_cluster_model_20230414_1423.Rdata')

response_csv <- 'data/field/plot_field_metrics.csv'
uas_csv <- 'data/las/metrics/uas_plot_metrics.csv'
spec_csv <- 'data/las/metrics/spectral_plot_metrics.csv'

# ==============================================================================
# ============================== Data preparation ==============================
# ==============================================================================

# Load inputs

uas_df <- read_csv(uas_csv)
spec_df <- read_csv(spec_csv)

predictor_df <- uas_df %>%
  left_join(spec_df) %>%
  mutate(across(
    .cols = everything(),
    .fns = ~replace_na(.x, replace = -9999)
  ))
  
response_df <- read_csv(response_csv)

# Generate model predictions

biomass_pred <- extractPrediction(
  models = list(rf = ml_models$biomass_sum_rf_rfe_spatial_folds))

# ==============================================================================
# ================================== Plotting ==================================
# ==============================================================================

# ================================ ggplot theme ================================ 

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

# ============================= Plotting function ============================== 

.data = biomass_pred

ggplot(data = .data,
       mapping = aes(
         x = pred,
         y = obs
       )) +
  geom_abline(
    linetype = 'dashed',
    linewidth = 0.6,
    color = 'grey60'
  ) + 
  geom_smooth(
    method = 'lm',
    se = FALSE,
    color = 'firebrick') +
  geom_point(
    color = 'black',
    size = 2
  ) +
  coord_fixed()

  
