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

response_csv <- 'data/plot_field_metrics.csv'
predictor_csv <- 'data/uas_plot_metrics.csv'

output_file <- 'rf_rfe_opt_{type}_{format(timestamp, "%Y%m%d_%H%M")}'


k_folds <- 10
rep <- 10

pre_process = c('center', 'scale')

set_seed_val = 111


splitrule = c('variance', 'extratrees', 'maxstat', 'beta')
min.node.size = 1:5


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

rfe_ctrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

# ------------------------------- Log file setup ------------------------------- 

timestamp <- Sys.time()

log_text <- glue(
'=====================================================================
Random Forest + RFE hyperparameter canopy fuels prediction from UAS
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

n predictors: {n_predictors}
k folds: {k_folds}

data preprocessing: {glue_collapse(pre_process, sep= ", ")}

========================== Model parameters =========================

repeats: {rep}
set seed: {set_seed_val}

Random Forest w Recursive Feature Elimination from Ranger package with
grid search as defined by following parameters. Optimal model based on 
minimum RMSE value from repeated cross validation.


=====================================================================
============================== Results ==============================
=====================================================================

'
)

# ==============================================================================
# ========================== Response initialization ===========================
# ==============================================================================


models = list()
rfe = list()
beststats = list()
fullstats = list()

for (response_i in response_var) {
  
  # ----------------------------- Background setup -----------------------------
  
  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i))) %>%
    select(all_of(c(response_i, predictor_var))) %>%
    as.data.frame()
  
  input_df[is.na(input_df)] <-  -9999
  
  cl <- makeCluster(15)
  registerDoParallel(cl)
  
  
  # ------------------------------- RFE ----------------------------- 
  
  set.seed(set_seed_val)
  
  rf_profile <- rfe(
    x = input_df %>%
      select(all_of(predictor_var)),
    y = input_df %>%
      pull(response_i),
    sizes = c(1:n_predictors),
    rfeControl = rfe_ctrl,
    preProcess = pre_process,
    metric = "RMSE",
    trControl = rfe_ctrl
  )
  
  stopCluster(cl)

  rf_var <- predictors(rf_profile)
  
  # ----------------------------- Search grid setup ----------------------------
  
  mtry = 2:length(rf_var)
  
  rf_grid <- expand.grid(mtry = mtry,
                         splitrule = splitrule,
                         min.node.size = min.node.size)
  
  # ------------------------------ Model training ------------------------------ 

  cl <- makeCluster(15)
  registerDoParallel(cl)
  
  set.seed(set_seed_val)
  
  rf_train <- train(
    x = input_df %>%
      select(all_of(rf_var)),
    y = input_df %>%
      pull(response_i),
    method = "ranger",
    preProcess = pre_process,
    trControl = train_ctrl,
    tuneGrid = rf_grid,
    metric = "RMSE"
  )
  
  stopCluster(cl)
  
  rf_stats <- rf_train$results %>%
    semi_join(rf_train$bestTune) %>%
    add_column(response_var = response_i, .before = 1)
  
  log_text <- log_text + '
=============================== {response_i} =============================

n samples: {nrow(input_df)}


RFE:

n variables: {length(rf_var)}


Grid search parameters:

mtry: 2 to {length(rf_var)} by 1
split rule: {glue_collapse(splitrule, sep= ", ")}
min node size: {glue_collapse(min.node.size, sep= ", ")}

n grid permutations: {nrow(rf_grid)}


Training results:

mtry: {rf_stats$mtry}
split rule: {rf_stats$splitrule}
min node size: {rf_stats$min.node.size}

RMSE: {rf_stats$RMSE}
R2: {rf_stats$Rsquared}
MAE: {rf_stats$RMSE}

  
' 
  rfe[[response_i]] = rf_profile
  models[[response_i]] = rf_train
  beststats[[response_i]] = rf_stats
  fullstats[[response_i]] = rf_train$results %>%
    add_column(response_var = response_i, .before = 1)

}
  
  
  
# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(models, glue('ml_output/{glue(output_file, type = "model")}.RData'))
saveRDS(models, glue('ml_output/{glue(output_file, type = "rfe")}.RData'))

do.call(rbind, beststats) %>%
  write_csv(glue('ml_output/{glue(output_file, type = "beststats")}.csv'))

do.call(rbind, fullstats) %>%
  write_csv(glue('ml_output/{glue(output_file, type = "fullstats")}.csv'))

  log_text <- log_text +
    '\n
---------------------------------------------------------------------

writing models to: 
ml_output/{glue(output_file, type = "rfe")}.RData
ml_output/{glue(output_file, type = "model")}.RData

writing stats to: 
ml_output/{glue(output_file, type = "beststats")}.csv
ml_output/{glue(output_file, type = "fullstats")}.csv

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'

write(log_text, glue('log_files/{glue(output_file, type = "log")}.txt'))

# ==============================================================================