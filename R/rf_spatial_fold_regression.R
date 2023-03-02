# =================================== Libraries ================================

library(doParallel)
library(tidyverse)
library(sf)
library(caret)
library(glue)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

# Input files

response_csv <- 'data/field/plot_field_metrics.csv'
# uas_csv <- 'data/las/metrics/uas_plot_metrics.csv'
# spec_csv <- 'data/las/metrics/spectral_plot_metrics.csv'

spatial_cluster_file <- 'data/temp/field_plots/field_plots_clusters.shp'
cluster_lookup_file <- 'data/temp/field_plots/field_spcorrelation_cluster_lookup.csv'

# Output

# output_file <- 'rf_spatial_cluster_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

output_file <- 'rf_spectral_height_metric_{type}_{format(timestamp, "%Y%m%d_%H%M")}'
uas_csv = 'data/las/metrics/uas_plot_spectral_height_metrics.csv'
spec_csv = 'ommitted'
pred_type = 'Spectral height'

# Model training parameters

k_folds <- 10
rfe_rep <- 10
training_rep <- 100

pre_process <- c('center', 'scale')

set_seed_val <- 111

n_cores <- detectCores() - 5

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

# uas_df <- read_csv(uas_csv)
# spec_df <- read_csv(spec_csv)
# 
# predictor_df <- uas_df %>%
#   left_join(spec_df)

predictor_df <- read_csv(uas_csv)

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
  left_join(predictor_df) %>%
  left_join(spatial_cluster)


rm(response_df, predictor_df, spatial_cluster, uas_df, spec_df)


# ==============================================================================
# =============================== Log file setup ===============================
# ==============================================================================

timestamp <- Sys.time()

log_text <- glue(
  '=====================================================================
Random Forest w/ RFE canopy fuels prediction from UAS metrics
Folds generated with spatial grouping
{pred_type} predictors only
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
# 
# response_i = response_var[1]
# 
# #####

ml_rfe = list()
ml_models = list()
ml_results = list()
ml_best = list()

for (response_i in response_var) {
  
  message('Response variable: ', response_i)
  
  log_text <- log_text + '
---------------------------------------------------------------------
------------------------------ {response_i} ------------------------------
---------------------------------------------------------------------
'
  # ---------------------------- Model input setup -----------------------------

  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i)))

  input_df[is.na(input_df)] <-  -9999

  ml_predictor <- input_df %>%
    select(all_of(predictor_var))

  ml_response <- input_df %>%
    pull(response_i)

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

  # ----------------------------------- RFE ------------------------------------

  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  set.seed(set_seed_val)

  message('RFE initiated: ', Sys.time())

  ml_profile <- rfe(
    x = ml_predictor,
    y = ml_response,
    sizes = c(1:n_predictors),
    rfeControl = rfeControl(
      functions = rfFuncs,
      index = rfe_folds
    ),
    preProcess = pre_process,
    metric = "RMSE"
  )

  stopCluster(cl)

  ml_var <- predictors(ml_profile)

  ml_rfe[[glue('{response_i}_rf_rfe_spatial_folds')]] <- ml_profile

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
    preProcess = pre_process,
    trControl = trainControl(
      index = train_folds),
    ntree = 1000,
    tuneLength = 100,
    metric = "RMSE"
  )

  stopCluster(cl)

  ml_models[[glue('{response_i}_rf_rfe_spatial_folds')]] <- ml_train

  ml_results[[glue('{response_i}_rf_rfe_spatial_folds')]] <-
    ml_train$results %>%
    add_column(response_var = response_i,
               method = 'rf_rfe_spatial_folds',
               .before = 1)

  ml_stats <- ml_train$results %>%
    semi_join(ml_train$bestTune) %>%
    add_column(response_var = response_i,
               method = 'rf_rfe_spatial_folds',
               .before = 1)

  ml_best[[glue('{response_i}_rf_rfe_spatial_folds')]] <- ml_stats

  log_text <- log_text +
    '\n
spatial cluster: {cluster_name}
n samples: {nrow(input_df)}

RFE:
n variables: {length(ml_var)}

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$Rsquared}
MAE: {ml_stats$MAE}

'
}

# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(ml_models,
        glue('data/ml_output/{glue(output_file, type = "model")}.RData'))

bind_rows(ml_best) %>%
  add_column(pre_process = glue_collapse(pre_process, sep = ", ")) %>%
  write_csv(glue(
    'data/ml_output/{glue(output_file, type = "results_best")}.csv'
  ))

bind_rows(ml_results) %>%
  add_column(pre_process = glue_collapse(pre_process, sep = ", ")) %>%
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

