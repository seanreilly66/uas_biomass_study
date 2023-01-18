# ==============================================================================
#
# GAMS model testing
#
# ==============================================================================
#
# Authors: Dr. Matthew Clark, matthew.clark@sonoma.edu
#         Sean Reilly, sean.reilly66@gmail.com
#
# Created: September 17, 2019
# Last commit:
#
# Status: Needs documentation
#
# Original framework created by Dr. Clark. Adapted and updated by Sean Reilly
# for 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Performs GAMS testing
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

library(doParallel)
library(caret)
library(tidyverse)
library(glue)

library(mgcv)

# ================================= User inputs ================================

response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/uas_plot_metrics.csv'

output_file <- 'gam_testing_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

cor_threshold = 0.75

k_folds <- 5
rep <- 100

# pre_process_text = c('center', 'scale')
pre_process_text = 'none'

set_seed_val = 111

# ==============================================================================
# =========================== Model setup and inputs ===========================
# ==============================================================================

response_df <- read_csv(response_csv)

response_var <- response_df %>%
  select(-site, -campaign, -plot) %>%
  colnames() %>%
  str_subset('_n', negate = TRUE)


predictor_df <- read_csv(predictor_csv)


cor_var <- predictor_df %>%
  select(-campaign, -plot, -method) %>%
  cor(use = 'na.or.complete') %>%
  findCorrelation(cutoff = cor_threshold,
                  exact = TRUE,
                  names = TRUE)

predictor_df <- predictor_df %>%
  select(-all_of(cor_var))


predictor_var <- predictor_df %>%
  select(-campaign, -plot, -method) %>%
  colnames()

n_predictors <- length(predictor_var)

model_df <- response_df %>%
  left_join(predictor_df)

rm(response_df, predictor_df)


# ==============================================================================
# ========================== Response initialization ===========================
# ==============================================================================

# ------------------------------- Log file setup ------------------------------- 

timestamp <- Sys.time()

log_text <- glue(
  '=====================================================================
GAM testing for canopy fuels prediction from UAS metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

n predictors: {length(predictor_var)}

data preprocessing: {glue_collapse(pre_process_text, sep= ", ")}
correlation threshold: {cor_threshold}

========================== Model parameters =========================

repeats: {rep}
set seed: {set_seed_val}
k folds: {k_folds}

=====================================================================
============================== Results ==============================
=====================================================================

'
)

# ------------------------------- Modelling run -------------------------------- 

# Testing setup
# log_text = log_text + '
# #### ###### TESTING RUN ###### ####
# 
# '
# response_var = response_var[1]
# predictor_var <- predictor_var[1:5]
# n_predictors = length(predictor_var)
# 
# response_i = response_var[1]
# gam_i = 'gam'











if (pre_process_text[1] == 'none') {
  pre_process = NULL
} else {
  pre_process = pre_process_text
}

results = data.frame(fold = 1:10)

for (response_i in response_var) {
  
  message('Response variable: ', response_i)
  
  log_text <- log_text + '
============================== {response_i} =============================
'
  
  # ---------------------------- Dataframe setup -----------------------------
  
  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i))) %>%
    select(all_of(c(response_i, predictor_var))) %>%
    as.data.frame()
  
  input_df[is.na(input_df)] <-  -9999
  
  
  set.seed(set_seed_val)
  
  gam_formula = paste(predictor_var, collapse = ', bs = "cr", k = 3) + s(') %>%
    paste(response_i, ' ~ s(', ., ', bs = "cr", k = 3)', sep = '') %>%
    as.formula()
  
  # gam_formula = paste(predictor_var, collapse = ') + s(') %>%
  #   paste(response_i, ' ~ s(', ., ')', sep = '') %>%
  #   as.formula()
  
  folds_index <- rep(1:k_folds,length.out = nrow(input_df)) %>%
    sample()
  
  fold_result = list()
  

  
  for (fold in 1:k_folds) {
    
    train <- input_df[folds_index != fold, ]
    test <- input_df[folds_index == fold, ]
    
    fold_gam <- gam(gam_formula, data = train)
    fold_predict <- predict(fold_gam, test)
    
    x <- lm(fold_predict ~ pull(test, response_i))
    r2 <- summary(x)$r.squared
    
    fold_result[[fold]] = r2

  }

  
  fold_result <- as.data.frame(fold_result) %>%
    t()
  colnames(fold_result) = response_i
  
  results <- results %>%
    add_column(fold_result)
  
  r2_mean = mean(fold_result)
  
  log_text <- log_text +
    '\n
R2: {r2_mean}

  '

  
}
  
  
  
  
  
  
  

# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

write_csv(results, glue('data/ml_output/{glue(output_file, type = "results")}.csv'))

log_text <- log_text +
  '\n
---------------------------------------------------------------------

writing stats to: 
data/ml_output/{glue(output_file, type = "results")}.csv

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'

write(log_text, glue('data/log_files/{glue(output_file, type = "log")}.txt'))

# ==============================================================================