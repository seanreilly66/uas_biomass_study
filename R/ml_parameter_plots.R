# ==============================================================================
#
# Model parameter importance and partial dependence plots
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 21 Feb 2022
# Last commit:
#
# Status: Needs documentation
#
# Created by Sean Reilly
# for 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Reads in R workspace files containing machine learning outputs from
# ml_regression.R and generates .
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(tidyverse)
library(glue)

# ============================== Set ggplot theme ==============================

x = list.files(path = 'data/ml_output', pattern = '.RData')


for (y in x) {
  load(glue('data/ml_output/{y}'))
  
  type = str_extract(y, '^.+(?=_by)')
  
  
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
  
  rf_varimp <- rf_varimp %>%
    arrange(desc(Overall)) %>%
    mutate(var = fct_reorder(var, Overall))
  
  
  ggplot(data = rf_varimp,
         mapping = aes(y = Overall,
                       x = var)) +
    geom_point() +
    geom_segment(aes(xend = var), yend = 0) +
    expand_limits(y = 0) +
    coord_flip() +
    labs(title = glue('{type} random forest'),
         x = 'Top variables',
         y = 'Overall importance')
  
  ggsave(
    filename = glue('data/ml_output/figures/{type}_rf_varimp.png'),
    width = 4,
    height = 5,
    units = 'in',
    dpi = 700
  )
  
  
  svm_varimp <- svm_varimp %>%
    arrange(desc(Overall)) %>%
    mutate(var = fct_reorder(var, Overall))
  
  
  ggplot(data = svm_varimp,
         mapping = aes(y = Overall,
                       x = var)) +
    geom_point() +
    geom_segment(aes(xend = var), yend = 0) +
    expand_limits(y = 0) +
    coord_flip() +
    labs(title = glue('{type} SVM'),
         x = 'Top variables',
         y = 'Overall importance')
  
  ggsave(
    filename = glue('data/ml_output/figures/{type}_svm_varimp.png'),
    width = 4,
    height = 5,
    units = 'in',
    dpi = 700
  )
  
  lm_varimp <- lm_varimp %>%
    arrange(desc(Overall)) %>%
    mutate(var = fct_reorder(var, Overall))
  
  
  ggplot(data = lm_varimp,
         mapping = aes(y = Overall,
                       x = var)) +
    geom_point() +
    geom_segment(aes(xend = var), yend = 0) +
    expand_limits(y = 0) +
    coord_flip() +
    labs(title = glue('{type} OLS'),
         x = 'Top variables',
         y = 'Overall importance')
  
  ggsave(
    filename = glue('data/ml_output/figures/{type}_ols_varimp.png'),
    width = 4,
    height = 5,
    units = 'in',
    dpi = 700
  )
  
}
