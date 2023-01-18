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


# 
# x = c(0.75, 0.9, 1)
# 
# for (cor_threshold in x) {



# ================================= User inputs ================================

response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/als_plot_metrics.csv'

log_file <-
  'data/log_files/{response_i}_by_{method}_ml_log_{format(timestamp, "%Y%m%d_%H%M")}.txt'
ml_output <- 'data/ml_output/{response_i}_by_{method}_ml'


k_folds_all <- 10
k_folds_site <- 5
rep <- 10
final_rep <- 100

pre_process = c('center', 'scale')

set_seed_val = 111

remove_cor = FALSE
cor_threshold = NA

# ==============================================================================
# =========================== Model setup and inputs ===========================
# ==============================================================================

response_df <- read_csv(response_csv)

response_var <- response_df %>%
  select(-site, -campaign, -plot) %>%
  colnames() %>%
  str_subset('_n', negate = TRUE)


predictor_df <- read_csv(predictor_csv)

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

method <- predictor_df$method[1]


# ==============================================================================
# ========================== Response initialization ===========================
# ==============================================================================

for (response_i in response_var) {
  
  timestamp <- Sys.time()
  
  log_text <- glue(
    '=====================================================================
Machine learning results for canopy fuels prediction from {method} metrics
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

========================== Response variable ========================

{response_i}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {predictor_csv}
response df: {response_csv}

n predictors: {length(predictor_var)}


data preprocessing: {glue_collapse(pre_process, sep= ", ")}

========================== Model parameters =========================

repeats: {rep}
final repeats: {final_rep}
set seed: {set_seed_val}
remove correlated predictors: {remove_cor}
correlation threshold: {cor_threshold}



=====================================================================
============================== Results ==============================
=====================================================================


============================= All sites ============================= 

n samples: {nrow(filter(model_df, !is.na(!!sym(response_i))))}
k folds: {k_folds_all}
'
  )
  
  # ---------------------------- Initiate site loop ---------------------------- 
  
  sites = c('all', unique(model_df$site))
  
  sites = c('jcksn', 'ltr')
  
  for (sites_i in sites) {
    
    # ----------------------------- Background setup -----------------------------
    
    input_df <- model_df %>%
      filter(!is.na(!!sym(response_i))) %>%
      as.data.frame()
    
    input_df[is.na(input_df)] <-  -9999
    
    k_folds <- k_folds_all
    
    if (sites_i != 'all') {
      
      input_df <- input_df %>%
        filter(site == sites_i)
      
      k_folds <- k_folds_site
      
      log_text = log_text + 
        '\n

=========================== Site: {sites_i} ============================== 

n samples = {nrow(input_df)}
k folds {k_folds_site}
'
      
    }
    
    
    cl <- makeCluster(detectCores() - 5)
    registerDoParallel(cl)
    
    # ------------------------------- Control setup ------------------------------
    
    rfe_ctrl <- trainControl(method = "cv",
                             number = k_folds,
                             allowParallel = TRUE)
    
    train_ctrl <- trainControl(
      method = "repeatedcv",
      number = k_folds,
      repeats = final_rep,
      allowParallel = TRUE
    )
    

    # ============================================================================
    # ============================== Random Forests ==============================
    # ============================================================================

    # ----------------------------------- RFE ------------------------------------

    set.seed(set_seed_val)

    rf_profile <- rfe(
      x = input_df %>%
        select(all_of(predictor_var)),
      y = input_df %>%
        pull(response_i),
      sizes = c(2:n_predictors),
      rfeControl = rfeControl(
        functions = rfFuncs,
        method = "repeatedcv",
        number = k_folds,
        repeats = rep,
        allowParallel = TRUE
      ),
      preProcess = pre_process,
      ntree = 1000,
      tuneLength = 10,
      metric = "RMSE",
      trControl = rfe_ctrl
    )

    rf_size <- pickSizeBest(rf_profile$results,
                            metric = "RMSE",
                            maximize = FALSE)

    rf_var <- pickVars(rf_profile$variables, rf_size)

    # -------------------------------- Training ----------------------------------

    set.seed(set_seed_val)

    rf_train <- train(
      x = input_df %>%
        select(all_of(rf_var)),
      y = input_df %>%
        pull(response_i),
      method = "rf",
      preProcess = c("center", "scale"),
      trControl = train_ctrl,
      ntree = 1000,
      tuneLength = 10,
      metric = "RMSE",
      allowParallel = TRUE
    )

    rf_stats <- rf_train$results %>%
      filter(mtry == rf_train$bestTune$mtry)

    rf_varimp = varImp(rf_train)$importance %>%
      arrange(desc(Overall)) %>%
      rownames_to_column(var = 'var')

    if (nrow(rf_varimp) > 10) {
      rf_varimp = rf_varimp[1:10,]
    }

    log_text <- log_text +
      '\n
--------------------------- Random Forest ---------------------------

RMSE: {rf_stats$RMSE}
Rsquared: {rf_stats$Rsquared}
mtry: {rf_train$bestTune$mtry}
n variables: {rf_size}

top variables by importance:
{glue_collapse(rf_varimp$var, sep = ", ")}'

    write_csv(rf_stats,
              file = glue('{glue(ml_output)}_{sites_i}_rf_pickbest_stats.csv'))



    # ============================================================================
    # =================================== SVM ====================================
    # ============================================================================

    # ----------------------------------- RFE ------------------------------------

    set.seed(set_seed_val)

    svm_profile <- rfe(
      x = input_df %>%
        select(all_of(predictor_var)),
      y = input_df %>%
        pull(response_i),
      sizes = c(2:n_predictors),
      rfeControl = rfeControl(
        functions = caretFuncs,
        method = "repeatedcv",
        number = k_folds,
        repeats = rep,
        allowParallel = TRUE
      ),
      method = "svmRadial",
      preProcess = pre_process,
      tuneLength = 10,
      metric = "RMSE",
      trControl = rfe_ctrl
    )

    svm_size <- pickSizeBest(svm_profile$results,
                             metric = "RMSE",
                             maximize = FALSE)

    svm_var <- pickVars(svm_profile$variables, svm_size)

    # -------------------------------- Training ----------------------------------

    set.seed(set_seed_val)

    svm_train <- train(
      x = input_df %>%
        select(all_of(svm_var)),
      y = input_df %>%
        pull(response_i),
      method = "svmRadial",
      preProcess = pre_process,
      trControl = train_ctrl,
      tuneLength = 10,
      metric = "RMSE",
      allowParallel = TRUE
    )

    svm_stats <- svm_train$results %>%
      filter(C == svm_train$bestTune$C)

    svm_varimp = varImp(svm_train)$importance %>%
      arrange(desc(Overall)) %>%
      rownames_to_column(var = 'var')

    if (nrow(svm_varimp) > 10) {
      svm_varimp = svm_varimp[1:10,]
    }

    log_text <- log_text +
      '\n
-------------------------------- SVM --------------------------------

RMSE: {svm_stats$RMSE}
Rsquared: {svm_stats$Rsquared}
cost: {svm_train$bestTune$C}
sigma: {svm_train$bestTune$sigma}
n variables: {svm_size}

top variables by importance:
{glue_collapse(svm_varimp$var, sep = ", ")}'

    write_csv(svm_stats,
              file = glue('{glue(ml_output)}_{sites_i}_svm_pickbest_stats.csv'))
    
    
    
    # ==========================================================================
    # ================================== OLS ===================================
    # ==========================================================================
    
    # ---------------------------------- RFE -----------------------------------
    
    set.seed(set_seed_val)
    
    lm_profile <- rfe(
      x = input_df %>%
        select(all_of(predictor_var)),
      y = input_df %>%
        pull(response_i),
      sizes = c(2:n_predictors),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "repeatedcv",
        number = k_folds,
        repeats = rep,
        allowParallel = TRUE
      ),
      preProcess = pre_process,
      metric = "RMSE",
      trControl = rfe_ctrl
    )
    
    
    lm_size <- pickSizeBest(lm_profile$results,
                            metric = "RMSE",
                            maximize = FALSE)
    
    lm_var <- pickVars(lm_profile$variables, lm_size)
    
    # ------------------------------- Training ---------------------------------
    
    set.seed(set_seed_val)
    
    lm_train <-  train(
      input_df %>%
        select(all_of(lm_var)),
      input_df %>%
        pull(response_i),
      method = "lm",
      preProcess = c("center", "scale"),
      trControl = train_ctrl
    )
    
    lm_stats <- lm_train$results
    
    lm_varimp = varImp(lm_train)$importance %>%
      arrange(desc(Overall)) %>%
      rownames_to_column(var = 'var')
    
    if (nrow(lm_varimp) > 10) {
      lm_varimp = lm_varimp[1:10,]
    }
    
    log_text <- log_text +
      '\n
------------------------------- OLS ---------------------------------

RMSE: {lm_stats$RMSE}
Rsquared: {lm_stats$Rsquared}
n variables: {lm_size}

top variables by importance:
{glue_collapse(lm_varimp$var, sep = ", ")}

vif:
{glue_collapse(round(car::vif(lm_train$finalModel),1), sep = ", ")}

'
    
    write_csv(lm_stats,
              file = glue('{glue(ml_output)}_{sites_i}_ols_pickbest_stats.csv'))
    
    save.image(file = glue('{glue(ml_output)}_{sites_i}.RData'))
    
  
  
  # ============================================================================
  # ========================== Complete processing run ========================= 
  # ============================================================================
  
  log_text <- log_text +
    '\n
---------------------------------------------------------------------

writing R workspace to: {glue(ml_output)}_{sites_i}.RData
finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'
  
  
  stopCluster(cl)
  
  }
  
  log_text <- log_text +
    '\n
============================== Complete =============================

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

====================================================================='
  
  write(log_text, glue(log_file), append = FALSE)
  
}

# }
