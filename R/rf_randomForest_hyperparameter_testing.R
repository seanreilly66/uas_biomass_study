# ==============================================================================
#
# Random Forest Hyperparameter Grid Search Regression
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
# Performs a Random Forest hyperparameter grid search for model optimization
#
# RF with and without RFE
#
# Random forest package
# 
# 
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

# setwd('data/cluster_mirror')

# response_csv <- 'data/plot_field_metrics.csv'
# predictor_csv <- 'data/uas_plot_metrics.csv'

# Local file setup: 
response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/uas_plot_metrics.csv'

output_file <- 'rf_randomForest_opt_{type}_{format(timestamp, "%Y%m%d_%H%M")}'


k_folds <- 10
rep <- 50

pre_process = c('center', 'scale')

set_seed_val = 111

ntree = seq(250, 5000, 250)

n_cores = 18

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

n_predictors <- length(predictor_var)

model_df <- response_df %>%
  left_join(predictor_df)

rm(response_df, predictor_df)


# -------------------------------- Control setup -------------------------------

train_ctrl <- trainControl(
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

rfe_ctrl <- trainControl(
  method = "cv",
  number = k_folds)

rf_ctrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

# ------------------------------- Log file setup ------------------------------- 

timestamp <- Sys.time()

log_text <- glue(
'=====================================================================
Random Forest hyperparameter canopy fuels prediction from UAS metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

package = randomForest

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

n predictors: {n_predictors}
k folds: {k_folds}

data preprocessing: {glue_collapse(pre_process, sep= ", ")}

========================== Model parameters =========================

repeats: {rep}
set seed: {set_seed_val}

Random Forest from randomForest package with and without RFE trained 
across mtry and ntree grid. Optimal model based on minimum RMSE value 
from cross validation.

'
)

# ==============================================================================
# ========================== Response initialization ===========================
# ==============================================================================

rfe_profile = list()
ml_models = list()
ml_best = list()
ml_results = list()



# # Testing setup


for (response_i in response_var) {
  
  message('Response variable: ', response_i)
  
  # ----------------------------- Background setup -----------------------------
  
  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i))) %>%
    select(all_of(c(response_i, predictor_var))) %>%
    as.data.frame()
  
  input_df[is.na(input_df)] <-  -9999
  
  # ---------------------------------- RFE ----------------------------------- 
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  set.seed(set_seed_val)
  
  message('Random forest RFE')
  
  ml_profile <- rfe(
    x = input_df %>%
      select(all_of(predictor_var)),
    y = input_df %>%
      pull(response_i),
    sizes = c(1:n_predictors),
    rfeControl = rf_ctrl,
    preProcess = pre_process,
    metric = "RMSE",
    trControl = rfe_ctrl
  )
  
  stopCluster(cl)
  
  ml_var <- predictors(ml_profile)
  
  rfe_profile[[glue('{response_i}_rf_rfe')]] <- ml_profile
  
  
  for (ntree_i in ntree) {
    
    message('n tree: ', ntree_i)
    
    # --------------------------------------------------------------------------
    # ------------------------- Random Forest with RFE -------------------------
    # --------------------------------------------------------------------------
    
    # ------------------------------- Training ---------------------------------
    
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('Random forest w rfe model training')
    
    ml_train <- train(
      x = input_df %>%
        select(all_of(ml_var)),
      y = input_df %>%
        pull(response_i),
      method = "rf",
      preProcess = pre_process,
      trControl = train_ctrl,
      ntree = ntree_i,
      tuneLength = length(ml_var)-1,
      metric = "RMSE"
    )
    
    stopCluster(cl)
    
    ml_models[[glue('{response_i}_rf_rfe_{ntree_i}')]] <- ml_train
    
    ml_results[[glue('{response_i}_rf_rfe_{ntree_i}')]] <- ml_train$results %>%
      add_column(response_var = response_i,
                 method = 'rf_rfe',
                 rfe = TRUE,
                 ntree = ntree_i,
                 .before = 1)
    
    # # ----------------------------------------------------------------------------
    # # --------------------------- Random Forest w/o RFE --------------------------
    # # ----------------------------------------------------------------------------
    # 
    # # -------------------------------- Training ----------------------------------
    # 
    # cl <- makeCluster(n_cores)
    # registerDoParallel(cl)
    # 
    # set.seed(set_seed_val)
    # 
    # message('Random forest training')
    # 
    # ml_train <- train(
    #   x = input_df %>%
    #     select(all_of(predictor_var)),
    #   y = input_df %>%
    #     pull(response_i),
    #   method = "rf",
    #   preProcess = pre_process,
    #   trControl = train_ctrl,
    #   ntree = ntree_i,
    #   tuneLength = length(predictor_var)-1,
    #   metric = "RMSE"
    # )
    # 
    # stopCluster(cl)
    # 
    # ml_models[[glue('{response_i}_rf')]] <- ml_train
    # 
    # ml_results[[glue('{response_i}_rf')]] <- ml_train$results %>%
    #   add_column(response_var = response_i,
    #              method = 'rf',
    #              rfe = FALSE,
    #              ntree = ntree_i,
    #              .before = 1)

  }
}
  
  
# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(ml_models, glue('data/ml_output/{glue(output_file, type = "model")}.RData'))

bind_rows(ml_results) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "results")}.csv'))

  log_text <- log_text +
    '\n
---------------------------------------------------------------------

writing models to: 
data/ml_output/{glue(output_file, type = "model")}.RData

writing results to: 
data/ml_output/{glue(output_file, type = "results")}.csv

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'

write(log_text, glue('data/log_files/{glue(output_file, type = "log")}.txt'))

# ==============================================================================