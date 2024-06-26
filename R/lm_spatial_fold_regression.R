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
uas_csv <- 'data/las/metrics/uas_plot_metrics.csv'
spec_csv <- 'data/las/metrics/spectral_plot_metrics.csv'

spatial_cluster_file <- 'data/temp/field_plots/field_plots_clusters.shp'
cluster_lookup_file <- 'data/temp/field_plots/field_spcorrelation_cluster_lookup.csv'

# Output

output_file <- 'lm_spatial_cluster_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

# Model training parameters

k_folds <- 10
rfe_rep <- 10
training_rep <- 100

pre_process <- c('center', 'scale')

remove_cor = TRUE
cor_threshold = 0.9

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

model_df <- 
  response_df %>%
  left_join(predictor_df) %>%
  left_join(spatial_cluster)

rm(response_df, predictor_df, spatial_cluster, cor_var, uas_df, spec_df)

# ==============================================================================
# =============================== Log file setup ===============================
# ==============================================================================

timestamp <- Sys.time()

log_text <- glue(
  '=====================================================================
LM canopy fuels prediction from UAS metrics
Folds generated with spatial grouping
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {uas_csv}, {spec_csv}
response df: {response_csv}

N predictors: {length(predictor_var)}

preprocessing: center and scale

remove correlated variables: {remove_cor}
correlation threshold: {cor_threshold}


========================== Model parameters =========================

LM and spatial folds

selection methods: RFE, none, backwards, forwards, sequential

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

##### Testing setup #####
# log_text = log_text + '
# #### ###### TESTING RUN ###### ####
# 
# '
# response_var = response_var[1:2]
# predictor_var <- predictor_var[1:5]
# n_predictors = length(predictor_var)
# 
# response_i = response_var[1]

#####

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
    select(all_of(predictor_var)) %>%
    as.data.frame()

  ml_response <- input_df %>%
    pull(response_i)
  
  # ----------------------------- Summary function ----------------------------- 
  
  summary_func <- function(data, lev = NULL, model = NULL) {
    c(
      MAPE = MLmetrics::MAPE(data$pred, data$obs),
      RMSE = MLmetrics::RMSE(data$pred, data$obs),
      MSE = MLmetrics::MSE(data$pred, data$obs),
      MAE = MLmetrics::MAE(data$pred, data$obs),
      R2 = summary(lm(pred ~ obs, data))$r.squared
    )
  }

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

  # ------------------------------ LM modelling ------------------------------ 
  
  # lm_method = c('lm', 'leapBackward', 'leapForward', 'leapSeq')
  lm_method = c('leapForward', 'leapSeq', 'leapBackward')
  
  for (lm_i in lm_method) {
    
    message(lm_i, ' initiated: ', Sys.time())
    
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    set.seed(set_seed_val)
    
    ml_train <- train(
      x = ml_predictor,
      y = ml_response,
      method = lm_i,
      preProcess = pre_process,
      trControl = trainControl(
        index = train_folds,
        summaryFunction = summary_func),
      metric = "RMSE"
    )
    
    stopCluster(cl)
    
    ml_models[[glue('{response_i}_{lm_i}_spatial_folds')]] <- ml_train
    
    ml_results[[glue('{response_i}_{lm_i}_spatial_folds')]] <-
      ml_train$results %>%
      add_column(response_var = response_i,
                 method = glue('{lm_i}_spatial_folds'),
                 .before = 1)
    
    ml_stats <- ml_train$results %>%
      semi_join(ml_train$bestTune) %>%
      add_column(response_var = response_i,
                 method = glue('{lm_i}_spatial_folds'),
                 .before = 1,)
    
    ml_best[[glue('{response_i}_{lm_i}_spatial_folds')]] <- ml_stats

    log_text <- log_text +
      '\n
---------------------------------------------------------------------
{lm_i}
---------------------------------------------------------------------

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$R2}
  
MAPE: {ml_stats$MAPE}
MAE: {ml_stats$MAE}
MSE: {ml_stats$MSE}

'
  }
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

