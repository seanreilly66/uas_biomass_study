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
# Generates a forward and sequential lm model from all sites except saddle mountain, then 
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
library(tidyverse)
library(sf)
library(caret)
library(glue)

# ================================= User inputs ================================

# Input files
response_csv <- 'data/field/plot_field_metrics.csv'
uas_csv <- 'data/las/metrics/uas_plot_metrics.csv'
spec_csv <- 'data/las/metrics/spectral_plot_metrics.csv'

spatial_cluster_file <- 'data/temp/field_plots/field_plots_clusters.shp'
cluster_lookup_file <- 'data/temp/field_plots/field_spcorrelation_cluster_lookup.csv'

# Output

output_file <- 'lm_transfer_learning_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

# Model training parameters

k_folds <- 10
rfe_rep <- 10
training_rep <- 100
n_tree <- 2000

cor_threshold = 0.9

pre_process = c('center', 'scale')

set_seed_val = 111

n_cores <- detectCores() - 3

# ==============================================================================
# ============================== Data preparation ==============================
# ==============================================================================

# Load inputs

spatial_cluster <- read_sf(spatial_cluster_file) %>%
  mutate(across(c('campaign', 'plot'), as.numeric)) %>%
  st_drop_geometry() %>%
  as_tibble() 

cluster_lookup <- read_csv(cluster_lookup_file)

response_df <- read_csv(response_csv) 

uas_df <- read_csv(uas_csv)
spec_df <- read_csv(spec_csv)
predictor_df <- uas_df %>%
  left_join(spec_df) 

# Remove correlated variables

cor_var <- predictor_df %>%
  select(-campaign, -plot, -method) %>%
  cor(use = 'na.or.complete') %>%
  findCorrelation(cutoff = cor_threshold,
                  exact = TRUE,
                  names = TRUE)

predictor_df <- predictor_df %>%
  select(-all_of(cor_var))

# Extract variable names

response_var <- response_df %>%
  select(-site, -campaign, -plot) %>%
  colnames() %>%
  str_subset('_n', negate = TRUE)

predictor_var <- predictor_df %>%
  select(-campaign, -plot, -method) %>%
  colnames()

n_predictors <- length(predictor_var)

# Generate combined df for modelling

model_df <- response_df %>%
  left_join(predictor_df) %>%
  left_join(spatial_cluster) %>%
  filter(site != 'sdlmtn')

test_df <- response_df %>%
  left_join(predictor_df) %>%
  filter(site == 'sdlmtn')

rm(response_df, predictor_df, spatial_cluster, cor_var, uas_df, spec_df)


# ==============================================================================
# =============================== Log file setup ===============================
# ==============================================================================

timestamp <- Sys.time()

log_text <- glue(
'=====================================================================
LM transfer learning testing from UAS metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {uas_csv}, {spec_csv}
response df: {response_csv}

n predictors: {n_predictors}

data preprocessing: {glue_collapse(pre_process, sep= ", ")}

========================== Model parameters =========================

test site: Saddle Mountain

rfe repeats: {rfe_rep}
training repeats: {training_rep}
set seed: {set_seed_val}
k folds: {k_folds}

lm forward and sequential from leaps package. Optimal model based on 
minimum RMSE value from cross validation using spatial clusters.

'
)

# ==============================================================================
# ================================== Modelling ================================= 
# ==============================================================================

ml_rfe = list()
ml_models = list()
ml_results = list()
ml_best = list()
test_predictions = list()
test_results = list()



# # Testing setup
# 
# response_i = response_var[4]
# predictor_var = predictor_var[1:10]


for (response_i in response_var) {
  
  message('Response variable: ', response_i)
  
  log_text <- log_text + '
  
_____________________________________________________________________
{response_i}
_____________________________________________________________________
'
  # ---------------------------- Model input setup -----------------------------
  
  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i)))
  
  input_df[is.na(input_df)] <-  -9999
  
  ml_predictor <- input_df %>%
    select(all_of(predictor_var)) %>%
    as.data.frame()
  
  ml_response <- input_df %>%
    pull(response_i)
  
  # ---------------------------- Testing data setup ----------------------------
  
  test_input_df <- test_df %>%
    filter(!is.na(!!sym(response_i))) %>%
    select(all_of(c(response_i, predictor_var))) %>%
    as.data.frame()
  
  test_input_df[is.na(test_input_df)] <-  -9999
  
  # --------------------- Repeated grouped K fold indexing ---------------------
  
  cluster_name <- cluster_lookup %>%
    filter(variable == response_i) %>%
    pull(cluster)
  
  cluster_index <- pull(input_df, cluster_name)
  
  set.seed(set_seed_val)
  
  rfe_folds <- list()
  
  for(i in 1:rfe_rep) {
    
    i_folds <- groupKFold(group = cluster_index, k = k_folds)
    
    pad_rep <- str_pad(i, nchar(rfe_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)
    
    rfe_folds <- append(rfe_folds, i_folds)
    
  }
  
  train_folds <- list()
  
  for(i in 1:training_rep) {
    
    i_folds <- groupKFold(group = cluster_index, k = k_folds)
    
    pad_rep <- str_pad(i, nchar(training_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)
    
    train_folds <- append(train_folds, i_folds)
    
  }
  
  # ----------------------------------------------------------------------------
  # -------------------------------- LM modelling ------------------------------ 
  # ----------------------------------------------------------------------------
  
  # ------------------------------- Training ---------------------------------
  
  lm_method = c('leapForward', 'leapSeq')
  
  for (lm_i in lm_method) {
    message(lm_i, ' initiated: ', Sys.time())
    
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    ml_train <- train(
      x = ml_predictor,
      y = ml_response,
      method = lm_i,
      preProcess = pre_process,
      trControl = trainControl(index = train_folds),
      metric = "RMSE"
    )
    
    stopCluster(cl)
    
    ml_models[[glue('{response_i}_{lm_i}_transfer_spatial_cluster')]] <-
      ml_train
    
    ml_results[[glue('{response_i}_{lm_i}_transfer_spatial_cluster')]] <-
      ml_train$results %>%
      add_column(
        response_var = response_i,
        method = glue('{lm_i}_spatial_folds'),
        .before = 1
      )
    
    ml_stats <- ml_train$results %>%
      semi_join(ml_train$bestTune) %>%
      add_column(
        response_var = response_i,
        method = glue('{lm_i}_spatial_folds'),
        .before = 1
      )
    
    ml_best[[glue('{response_i}_{lm_i}_spatial_cluster')]] <- ml_stats
    
    log_text <- log_text + '
---------------------------------------------------------------------
{lm_i}

n samples: {nrow(input_df)}

MODEL RESULTS:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$Rsquared}
MAE: {ml_stats$MAE}
'
    
    # ------------------------------- Model Testing ------------------------------
    
    predictions = predict(ml_train, test_input_df)
    
    test_stats = postResample(predictions, pull(test_input_df, response_i)) %>%
      as.list() %>%
      as_tibble()
    
    test_results[[glue('{response_i}_rf_sdlmtn_transfer')]] <-
      test_stats %>%
      add_column(response_var = response_i,
                 method = 'rf_rfe',
                 .before = 1)
    
    test_predictions[[glue('{response_i}_rf_sdlmtn_transfer')]] <-
      tibble(
        response_var = response_i,
        predict_val = predictions,
        field_val = pull(test_input_df, response_i)
      )
    
    log_text <- log_text +
      '

TRANSFER LEARNING RESULTS:

RMSE: {test_stats$RMSE}
R2: {test_stats$Rsquared}
MAE: {test_stats$MAE}
'
  } 
}
  
  
# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

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