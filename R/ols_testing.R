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

response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/uas_plot_metrics.csv'

output_file <- 'lm_testing_{type}_{format(timestamp, "%Y%m%d_%H%M")}'


k_folds <- 10
rep <- 100

# pre_process_text = c('center', 'scale')
pre_process_text = 'none'

set_seed_val = 111

remove_cor = TRUE
cor_threshold = 0.75

# ==============================================================================
# =========================== Model setup and inputs ===========================
# ==============================================================================

response_df <- read_csv(response_csv)

response_var <- response_df %>%
  select(-site, -campaign, -plot) %>%
  colnames() %>%
  str_subset('_n', negate = TRUE)


predictor_df <- read_csv(predictor_csv)



# ------------------------------- Correlation var removal

if (remove_cor) {
  
  cor_var <- predictor_df %>%
    select(-campaign, -plot, -method) %>%
    cor(use = 'na.or.complete') %>%
    findCorrelation(cutoff = cor_threshold,
                    exact = TRUE,
                    names = TRUE)
  
  predictor_df <- predictor_df %>%
    select(-all_of(cor_var))
  
}

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

# -------------------------------- Control setup -------------------------------

train_ctrl <- trainControl(
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

# ------------------------------- Log file setup ------------------------------- 

timestamp <- Sys.time()

log_text <- glue(
  '=====================================================================
Linear model testing for canopy fuels prediction from UAS metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

n predictors: {length(predictor_var)}

data preprocessing: {glue_collapse(pre_process_text, sep= ", ")}
remove correlated variables: {remove_cor}
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



if (pre_process_text[1] == 'none') {
  pre_process = NULL
} else {
  pre_process = pre_process_text
}


lm_models = list()
lm_results = list()
lm_best = list()

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

    # ------------------------------ LM modelling ------------------------------ 
    
    lm_method = c('lm', 'leapBackward', 'leapForward', 'leapSeq', 'glm', 'bayesglm', 'glmboost')
    
    for (lm_i in lm_method) {
      
      message('lm method: ', lm_i)
    
      cl <- makeCluster(detectCores() - 2)
      registerDoParallel(cl)
      
      set.seed(set_seed_val)
      
      lm_train <-  train(
        input_df %>%
          select(all_of(predictor_var)),
        input_df %>%
          pull(response_i),
        method = lm_i,
        preProcess = pre_process,
        trControl = train_ctrl
      )
      
      stopCluster(cl)
      
      lm_models[[glue('{response_i}_{lm_i}')]] <- lm_train
      
      lm_results[[glue('{response_i}_{lm_i}')]] <- lm_train$results %>%
        add_column(response_var = response_i,
                   method = lm_i,
                   .before = 1)
      
      lm_stats <- lm_train$results %>%
        semi_join(lm_train$bestTune) %>%
        add_column(response_var = response_i,
                   method = lm_i,
                   .before = 1)
      
      lm_best[[glue('{response_i}_{lm_i}')]] <- lm_stats
      
      log_text <- log_text +
        '\n
---------------------------------------------------------------------
{lm_i}
---------------------------------------------------------------------

RMSE: {lm_stats$RMSE}
R2: {lm_stats$Rsquared}
MAE: {lm_stats$MAE}

'
    }

}

# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(lm_models, glue('data/ml_output/{glue(output_file, type = "model")}.RData'))

bind_rows(lm_best) %>%
  add_column(pre_process = glue_collapse(pre_process_text, sep= ", "),
             cor_threshold = cor_threshold) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "results_best")}.csv'))

bind_rows(lm_results) %>%
  add_column(pre_process = glue_collapse(pre_process_text, sep= ", "),
             cor_threshold = cor_threshold) %>%
  write_csv(glue('data/ml_output/{glue(output_file, type = "results_full")}.csv'))

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

write(log_text, glue('data/log_files/{glue(output_file, type = "log")}.txt'))

# ==============================================================================