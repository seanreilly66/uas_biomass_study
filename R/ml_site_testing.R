# ==============================================================================
#
# Pick best machine learning
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
# Performs a Random Forest, Support Vector Machine and ordinary least-squares
# regression analysis and a 5-fold cross validation with 10 repeats used to
# calculate RMSE and r-squared.
#
# Methods generally follow:
# de Almeida, C. T., Galv?o, L. S., Ometto, J. P. H. B., Jacon, A. D., de Souza
# Pereira, F. R., Sato, L. Y., ... & Longo, M. (2019). Combining LiDAR and
# hyperspectral data for aboveground biomass modeling in the Brazilian Amazon
# using different regression algorithms. Remote Sensing of Environment, 232, 111323.
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


# ================================= User inputs ================================

# Cluster file setup:

# setwd('data/cluster_mirror')

# response_csv <- 'data/plot_field_metrics.csv'
# predictor_csv <- 'data/uas_plot_metrics.csv'

# Local file setup: 
response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/uas_plot_metrics.csv'

output_file <-
  'ml_site_testing_{type}_{format(timestamp, "%Y%m%d_%H%M")}'


k_folds <- 5
rep <- 50

set_seed_val = 111

n_core = 12

# ==============================================================================
# =========================== Model setup and inputs ===========================
# ==============================================================================

response_df <- read_csv(response_csv)

response_var <- response_df %>%
  select(-site, -campaign, -plot) %>%
  colnames() %>%
  str_subset('_n', negate = TRUE)


predictor_df <- read_csv(predictor_csv)

predictor_var <- predictor_df %>%
  select(-campaign, -plot, -method) %>%
  colnames()

model_df <- response_df %>%
  left_join(predictor_df)

sites <- unique(model_df$site)

rm(response_df, predictor_df)




# ==============================================================================
# ========================== Response initialization ===========================
# ==============================================================================

# -------------------------------- Control setup -------------------------------

train_ctrl <- trainControl(method = "repeatedcv",
                           number = k_folds,
                           repeats = rep)

rfe_ctrl <- trainControl(method = "cv",
                         number = k_folds)

rf_ctrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

svm_ctrl <- rfeControl(
  functions = caretFuncs,
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

# ------------------------------- Log file setup -------------------------------

timestamp <- Sys.time()

log_text <- glue(
  '=====================================================================
Site level model testing for canopy fuels prediction from UAS metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

rf and svm preprocessing: center and scale

lm preprocessing: multicolinearity filter with R threshold = 0.9

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

                # # # Testing setup
                # log_text = log_text + '
                #             #### ###### TESTING RUN ###### ####
                # 
                #             '
                # response_var = response_var[1:2]
                # sites = sites[1:2]
                # predictor_var <- predictor_var[1:5]
                # lm_predictor_var <- predictor_var[1:3]




ml_rfe = list()
ml_models = list()
ml_results = list()
ml_best = list()

for (response_i in response_var) {
  message('Response variable: ', response_i)
  
  log_text <- log_text + '
-------------------------------------------------------------------------
------------------------------ {response_i} ------------------------------
-------------------------------------------------------------------------

'
  
  for (site_i in sites) {
    message('Site: ', site_i)
    
    # ---------------------------- Dataframe setup -----------------------------
    
    input_df <- model_df %>%
      filter(site == site_i) %>%
      filter(!is.na(!!sym(response_i))) %>%
      select(all_of(c(response_i, predictor_var))) %>%
      as.data.frame()
    
    input_df[is.na(input_df)] <-  -9999
    
    cor_var <- input_df %>%
      select(all_of(predictor_var)) %>%
      cor(use = 'na.or.complete') %>%
      findCorrelation(cutoff = 0.9,
                      exact = TRUE,
                      names = TRUE)
    
    lm_predictor_var <- predictor_var[!(predictor_var %in% cor_var)]
    
    log_text <- log_text + '
================================ {site_i} ================================= 
rf and svm n predictors: {length(predictor_var)}
lm forward predictors: {length(lm_predictor_var)}

'
    
    # ----------------------------------------------------------------------------
    # -------------------------- Random Forest with RFE --------------------------
    # ----------------------------------------------------------------------------
    
    # ----------------------------------- RFE ------------------------------------
    
    cl <- makeCluster(n_core)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('Random forest RFE')
    
    ml_profile <- rfe(
      x = input_df %>%
        select(all_of(predictor_var)),
      y = input_df %>%
        pull(response_i),
      sizes = c(2:length(predictor_var)),
      rfeControl = rf_ctrl,
      preProcess = c('center', 'scale'),
      metric = "RMSE",
      trControl = rfe_ctrl
    )
    
    stopCluster(cl)
    
    ml_var <- predictors(ml_profile)
    
    ml_rfe[[glue('{response_i}_{site_i}_rf_rfe')]] <- ml_profile
    
    # -------------------------------- Training ----------------------------------
    
    cl <- makeCluster(n_core)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('Random forest w rfe model training')
    
    ml_train <- train(
      x = input_df %>%
        select(all_of(ml_var)),
      y = input_df %>%
        pull(response_i),
      method = "rf",
      preProcess = c('center', 'scale'),
      trControl = train_ctrl,
      ntree = 1000,
      tuneLength = 100,
      metric = "RMSE"
    )
    
    stopCluster(cl)
    
    ml_models[[glue('{response_i}_{site_i}_rf_rfe')]] <- ml_train
    
    ml_results[[glue('{response_i}_{site_i}_rf_rfe')]] <-
      ml_train$results %>%
      add_column(
        response_var = response_i,
        site = site_i,
        method = 'rf_rfe',
        .before = 1
      )
    
    ml_stats <- ml_train$results %>%
      semi_join(ml_train$bestTune) %>%
      add_column(
        response_var = response_i,
        site = site_i,
        method = 'rf_rfe',
        .before = 1
      )
    
    ml_best[[glue('{response_i}_{site_i}_rf_rfe')]] <- ml_stats
    
    log_text <- log_text +
      '\n
random forest with RFE
---------------------------------------------------------------------
n samples: {nrow(input_df)}

RFE:
n variables: {length(ml_var)}

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$Rsquared}
MAE: {ml_stats$MAE}

'
    
#     # ----------------------------------------------------------------------------
#     # --------------------------- Random Forest w/o RFE --------------------------
#     # ----------------------------------------------------------------------------
#     
#     # -------------------------------- Training ----------------------------------
#     
#     cl <- makeCluster(n_core)
#     registerDoParallel(cl)
#     
#     set.seed(set_seed_val)
#     
#     message('Random forest training')
#     
#     ml_train <- train(
#       x = input_df %>%
#         select(all_of(predictor_var)),
#       y = input_df %>%
#         pull(response_i),
#       method = "rf",
#       preProcess = c('center', 'scale'),
#       trControl = train_ctrl,
#       ntree = 1000,
#       tuneLength = 100,
#       metric = "RMSE"
#     )
#     
#     stopCluster(cl)
#     
#     ml_models[[glue('{response_i}_{site_i}_rf')]] <- ml_train
#     
#     ml_results[[glue('{response_i}_{site_i}_rf')]] <-
#       ml_train$results %>%
#       add_column(
#         response_var = response_i,
#         site = site_i,
#         method = 'rf',
#         .before = 1
#       )
#     
#     ml_stats <- ml_train$results %>%
#       semi_join(ml_train$bestTune) %>%
#       add_column(
#         response_var = response_i,
#         site = site_i,
#         method = 'rf',
#         .before = 1
#       )
#     
#     ml_best[[glue('{response_i}_{site_i}_rf')]] <- ml_stats
#     
#     log_text <- log_text +
#       '\n
# random forest, no prior feature selection
# ---------------------------------------------------------------------
# 
# n samples: {nrow(input_df)}
# n variables: {length(predictor_var)}
# 
# Results:
# 
# RMSE: {ml_stats$RMSE}
# R2: {ml_stats$Rsquared}
# MAE: {ml_stats$MAE}
# 
# '
    
    # ----------------------------------------------------------------------------
    # ------------------------------- SVM with RFE -------------------------------
    # ----------------------------------------------------------------------------
    
    # ----------------------------------- RFE ------------------------------------
    
    cl <- makeCluster(n_core)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('SVM RFE')
    
    ml_profile <- rfe(
      x = input_df %>%
        select(all_of(predictor_var)),
      y = input_df %>%
        pull(response_i),
      sizes = c(1:length(predictor_var)),
      rfeControl = svm_ctrl,
      method = 'svmRadial',
      preProcess = c('center', 'scale'),
      metric = "RMSE",
      trControl = rfe_ctrl
    )
    
    stopCluster(cl)
    
    ml_var <- predictors(ml_profile)
    
    ml_rfe[[glue('{response_i}_{site_i}_svm_rfe')]] <- ml_profile
    
    # -------------------------------- Training ----------------------------------
    
    cl <- makeCluster(n_core)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('svm w rfe model training')
    
    ml_train <- train(
      x = input_df %>%
        select(all_of(ml_var)),
      y = input_df %>%
        pull(response_i),
      method = "svmRadial",
      preProcess = c('center', 'scale'),
      trControl = train_ctrl,
      tuneLength = 100,
      metric = "RMSE"
    )
    
    stopCluster(cl)
    
    ml_models[[glue('{response_i}_{site_i}_svm_rfe')]] <- ml_train
    
    ml_results[[glue('{response_i}_{site_i}_svm_rfe')]] <-
      ml_train$results %>%
      add_column(
        response_var = response_i,
        site = site_i,
        method = 'svm_rfe',
        .before = 1
      )
    
    ml_stats <- ml_train$results %>%
      semi_join(ml_train$bestTune) %>%
      add_column(
        response_var = response_i,
        site = site_i,
        method = 'svm_rfe',
        .before = 1
      )
    
    ml_best[[glue('{response_i}_{site_i}_svm_rfe')]] <- ml_stats
    
    log_text <- log_text +
      '\n
SVM with RFE
---------------------------------------------------------------------
n samples: {nrow(input_df)}

RFE:
n variables: {length(ml_var)}

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$Rsquared}
MAE: {ml_stats$MAE}

'
    
#     # ----------------------------------------------------------------------------
#     # -------------------------------- SVM w/o RFE -------------------------------
#     # ----------------------------------------------------------------------------
#     
#     # -------------------------------- Training ----------------------------------
#     
#     cl <- makeCluster(n_core)
#     registerDoParallel(cl)
#     
#     set.seed(set_seed_val)
#     
#     message('SVM training')
#     
#     ml_train <- train(
#       x = input_df %>%
#         select(all_of(predictor_var)),
#       y = input_df %>%
#         pull(response_i),
#       method = "svmRadial",
#       preProcess = c('center', 'scale'),
#       trControl = train_ctrl,
#       tuneLength = 100,
#       metric = "RMSE"
#     )
#     
#     stopCluster(cl)
#     
#     ml_models[[glue('{response_i}_{site_i}_svm')]] <- ml_train
#     
#     ml_results[[glue('{response_i}_{site_i}_svm')]] <-
#       ml_train$results %>%
#       add_column(
#         response_var = response_i,
#         site = site_i,
#         method = 'svm',
#         .before = 1
#       )
#     
#     ml_stats <- ml_train$results %>%
#       semi_join(ml_train$bestTune) %>%
#       add_column(
#         response_var = response_i,
#         site = site_i,
#         method = 'svm',
#         .before = 1
#       )
#     
#     ml_best[[glue('{response_i}_{site_i}_svm')]] <- ml_stats
#     
#     log_text <- log_text +
#       '\n
# SVM, no prior feature selection
# ---------------------------------------------------------------------
# 
# n samples: {nrow(input_df)}
# n variables: {length(predictor_var)}
# 
# Results:
# 
# RMSE: {ml_stats$RMSE}
# R2: {ml_stats$Rsquared}
# MAE: {ml_stats$MAE}
# 
# '
    
    # ----------------------------------------------------------------------------
    # -------------------------------- LM w forward selection -------------------------------
    # ----------------------------------------------------------------------------
    
    # -------------------------------- Training ----------------------------------
    
    message('leapForward')
    
    cl <- makeCluster(n_core)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    ml_train <-  train(
      input_df %>%
        select(all_of(lm_predictor_var)),
      input_df %>%
        pull(response_i),
      method = 'leapForward',
      preProcess = NULL,
      trControl = train_ctrl
    )
    
    stopCluster(cl)
    
    ml_models[[glue('{response_i}_{site_i}_leapForward')]] <-
      ml_train
    
    ml_results[[glue('{response_i}_{site_i}_leapForward')]] <-
      ml_train$results %>%
      add_column(response_var = response_i,
                 site = site_i,
                 method = 'leapForward',
                 .before = 1)
    
    ml_stats <- ml_train$results %>%
      semi_join(ml_train$bestTune) %>%
      add_column(response_var = response_i,
                 site = site_i,
                 method = 'leapForward',
                 .before = 1)
    
    ml_best[[glue('{response_i}_{site_i}_leapForward')]] <- ml_stats
    
    log_text <- log_text +
      '\n
leapForward
---------------------------------------------------------------------

n samples: {nrow(input_df)}

Multicolinearity reduction:
n variables: {length(lm_predictor_var)}

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$Rsquared}
MAE: {ml_stats$MAE}

'
  }
}


# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(ml_models,
        glue('data/ml_output/{glue(output_file, type = "model")}.RData'))
saveRDS(ml_rfe,
        glue('data/ml_output/{glue(output_file, type = "rfe")}.RData'))

bind_rows(ml_best) %>%
  write_csv(glue(
    'data/ml_output/{glue(output_file, type = "results_best")}.csv'
  ))

bind_rows(ml_results) %>%
  write_csv(glue(
    'data/ml_output/{glue(output_file, type = "results_full")}.csv'
  ))

log_text <- log_text +
  '\n
---------------------------------------------------------------------

writing models to:
data/ml_output/{glue(output_file, type = "model")}.RData

writing stats to:
data/ml_output/{glue(output_file, type = "results_best")}.csv
data/ml_output/{glue(output_file, type = "results_full")}.csv

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'



write(log_text,
      glue('data/log_files/{glue(output_file, type = "log")}.txt'))

# ==============================================================================