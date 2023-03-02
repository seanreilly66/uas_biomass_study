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

# Input files

response_csv <- 'data/field/plot_field_metrics.csv'
uas_csv <- 'data/las/metrics/uas_plot_metrics.csv'
spec_csv <- 'data/las/metrics/spectral_plot_metrics.csv'

# Output
output_file <- 'ml_site_testing_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

# Model training parameters

k_folds <- 5
rfe_rep <- 10
training_rep <- 100

pre_process <- c('center', 'scale')

set_seed_val <- 111

n_cores <- detectCores() - 5

# ==============================================================================
# ============================== Data preparation ==============================
# ==============================================================================

# Load inputs

response_df <- read_csv(response_csv)

uas_df <- read_csv(uas_csv)
spec_df <- read_csv(spec_csv)

predictor_df <- uas_df %>%
  left_join(spec_df)

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

model_df <- 
  response_df %>%
  left_join(predictor_df)

sites <- unique(model_df$site)

rm(response_df, predictor_df, uas_df, spec_df)

# ==============================================================================
# =============================== Log file setup ===============================
# ==============================================================================

timestamp <- Sys.time()

log_text <- glue(
  '=====================================================================
Random Forest w/ RFE canopy fuels prediction from UAS metrics
Individual site models
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {uas_csv}, {spec_csv}
response df: {response_csv}

N predictors: {length(predictor_var)}

Preprocessing: center and scale


========================== Model parameters =========================

random forest with RFE and spatial folds

rfe repeats: {rfe_rep}
training repeats: {training_rep}
set seed: {set_seed_val}
k folds: {k_folds}

=====================================================================
============================== Results ==============================
=====================================================================

'
)


# ==============================================================================
# ================================== Modelling ================================= 
# ==============================================================================

# ##### Testing setup #####
# log_text = log_text + '
# #### ###### TESTING RUN ###### ####
# 
# '
# model_df <- model_df %>%
#   add_column(foo = 1)
# 
# response_var = response_var[1:2]
# predictor_var <- predictor_var[1:5] %>%
#   append('foo')
# n_predictors = length(predictor_var)
# sites = sites[1:2]
# 
# response_i = response_var[1]
# site_i = sites[1]

# #####


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
    
    log_text <- log_text + '
================================ {site_i} ================================= 

'
    
    # ---------------------------- Model input setup -----------------------------
    
    input_df <- model_df %>%
      filter(site == site_i) %>%
      filter(!is.na(!!sym(response_i)))
    
    input_df[is.na(input_df)] <-  -9999
    
    ml_predictor <- input_df %>%
      select(all_of(predictor_var))
    
    ml_response <- input_df %>%
      pull(response_i)
    
    # ----------------------------------- RFE ------------------------------------
    
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('RFE initiated ', Sys.time())
    
    ml_profile <- rfe(
      x = ml_predictor,
      y = ml_response,
      sizes = c(1:n_predictors),
      rfeControl = rfeControl(
        functions = rfFuncs,
        method = "repeatedcv",
        number = k_folds,
        repeats = rfe_rep
      ),
      preProcess = pre_process,
      metric = "RMSE"
    )
    
    stopCluster(cl)
    
    ml_var <- predictors(ml_profile)
    
    ml_rfe[[glue('{response_i}_{site_i}_rf_rfe')]] <- ml_profile
    
    # -------------------------------- Training ----------------------------------
    
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    message('Random forest initiated: ', Sys.time())
    
    ml_train <- train(
      x = ml_predictor %>%
        select(all_of(ml_var)),
      y = ml_response,
      method = "rf",
      preProcess = c('center', 'scale'),
      trControl = trainControl(method = "repeatedcv",
                               number = k_folds,
                               repeats = training_rep),
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
n samples: {nrow(input_df)}

RFE:
n variables: {length(ml_var)}

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