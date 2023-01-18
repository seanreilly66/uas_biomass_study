# ==============================================================================
#
# Saddle Mountain transfer learning test
#
# ==============================================================================
#
# Authors: Sean Reilly, sean.reilly66@gmail.com
#
# Created: September 17, 2019
# Last commit:
#
# Status: Needs documentation
#
#
# ==============================================================================
#
# Description:
#
# Generates a random forest model from all sites except saddle mountain, then 
# tests that model on saddle mountain data
# 
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

# Local file setup: 
response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/uas_plot_metrics.csv'

output_file <- 'sdlmtn_transfer_learning_{type}_{format(timestamp, "%Y%m%d_%H%M")}'


k_folds <- 10
rep <- 100
n_tree <- 2000

pre_process = c('center', 'scale')

set_seed_val = 111

n_cores = 18

# ==============================================================================
# =========================== Model setup and inputs ===========================
# ==============================================================================

response_df <- read_csv(response_csv) %>%
  mutate(cc = 100-densiometer_mean) %>%
  select(-densiometer_mean)

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
  left_join(predictor_df) %>%
  filter(site != 'sdlmtn')

test_df <- response_df %>%
  left_join(predictor_df) %>%
  filter(site == 'sdlmtn')

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
Saddle Mountain transfer learning testing with RF from UAS metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

package = randomForest

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

n predictors: {n_predictors}


data preprocessing: {glue_collapse(pre_process, sep= ", ")}

========================== Model parameters =========================

repeats: {rep}
set seed: {set_seed_val}
k folds: {k_folds}
ntree: {n_tree}

Random Forest from randomForest package with RFE trained 
across mtry and ntree grid. Optimal model based on minimum RMSE value 
from cross validation.

'
)

# ==============================================================================
# =============================== Model Testing ================================
# ==============================================================================

rfe_profile = list()
ml_models = list()
ml_best = list()
ml_results = list()
test_predictions = list()
test_results = list()



# # Testing setup
# 
# response_i = response_var[4]
# predictor_var = predictor_var[1:10]


for (response_i in response_var) {
  
  message('Response variable: ', response_i)
  
  # ----------------------------------------------------------------------------
  # -------------------------- Random Forest with RFE --------------------------
  # ----------------------------------------------------------------------------
  
  # ----------------------------- Data input setup -----------------------------
  
  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i))) %>%
    select(all_of(c(response_i, predictor_var))) %>%
    as.data.frame()
  
  input_df[is.na(input_df)] <-  -9999
  
  # ----------------------------------- RFE ------------------------------------ 
  
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

  # ------------------------------- Training ---------------------------------
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  set.seed(set_seed_val)
  
  message('RF model training')
  
  ml_train <- train(
    x = input_df %>%
      select(all_of(ml_var)),
    y = input_df %>%
      pull(response_i),
    method = "rf",
    preProcess = pre_process,
    trControl = train_ctrl,
    ntree = n_tree,
    tuneLength = length(ml_var) - 1,
    metric = "RMSE"
  )
  
  stopCluster(cl)
  
  ml_models[[glue('{response_i}_sdlmtn_transfer')]] <- ml_train
  
  ml_results[[glue('{response_i}_sdlmtn_transfer')]] <-
    ml_train$results %>%
    add_column(
      response_var = response_i,
      method = 'rf',
      rfe = TRUE,
      ntree = n_tree,
      tune_length = length(ml_var) - 1,
      k_folds = k_folds,
      repeats = rep,
      seed = set_seed_val,
      .before = 1
    )
  
  ml_stats <- ml_train$results %>%
    semi_join(ml_train$bestTune) %>%
    add_column(
      response_var = response_i,
      method = 'rf',
      rfe = TRUE,
      ntree = n_tree,
      tune_length = length(ml_var) - 1,
      k_folds = k_folds,
      repeats = rep,
      seed = set_seed_val,
      .before = 1
    )

  ml_best[[glue('{response_i}_sdlmtn_transfer')]] <- ml_stats
  
  log_text <- log_text +
    '\n
random forest with RFE model results
---------------------------------------------------------------------
n samples: {nrow(input_df)}

RFE:
n variables: {length(ml_var)}

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$Rsquared}
MAE: {ml_stats$MAE}
'
  
  # ----------------------------------------------------------------------------
  # ------------------------------- Model Testing ------------------------------
  # ----------------------------------------------------------------------------
  
  # ----------------------------- Data input setup -----------------------------
  
  test_input_df <- test_df %>%
    filter(!is.na(!!sym(response_i))) %>%
    select(all_of(c(response_i, predictor_var))) %>%
    as.data.frame()
  
  test_input_df[is.na(test_input_df)] <-  -9999
  
  # ---------------------------------- Predict ---------------------------------
  
  predictions = predict(ml_train, test_input_df)
  
  test_stats = postResample(predictions, pull(test_input_df, response_i)) %>%
    as.list() %>%
    as_tibble()
  
  test_results[[glue('{response_i}_sdlmtn_transfer')]] <- test_stats %>%
    add_column(
      response_var = response_i,
      .before = 1
    )
  
  test_predictions[[glue('{response_i}_sdlmtn_transfer')]] <- tibble(
    response_var = response_i,
    predict_val = predictions,
    field_val = pull(test_input_df, response_i)
  )
  
  log_text <- log_text +
    '\n
transfer learning testing
---------------------------------------------------------------------
Results:

RMSE: {test_stats$RMSE}
R2: {test_stats$Rsquared}
MAE: {test_stats$MAE}
'
  
  
}
  
  
# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(rfe_profile, glue('data/ml_output/{glue(output_file, type = "rfe")}.RData'))
saveRDS(ml_models, glue('data/ml_output/{glue(output_file, type = "model")}.RData'))

bind_rows(ml_results) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "model_results_full")}.csv'))
bind_rows(ml_best) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "model_results_best")}.csv'))

bind_rows(test_predictions) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "predictions")}.csv'))
bind_rows(test_results) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "test_results")}.csv'))


  log_text <- log_text +
    '\n
---------------------------------------------------------------------

writing models to: 
data/ml_output/{glue(output_file, type = "datatype")}.RData

writing results to: 
data/ml_output/{glue(output_file, type = "datatype")}.csv

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'

write(log_text, glue('data/log_files/{glue(output_file, type = "log")}.txt'))

# ==============================================================================