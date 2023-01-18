# ==============================================================================
#
# Random Forest Hyperparameter Grid Search Regression REPREX
#
# ==============================================================================
#
# Description:
#
# Lightweight reprex using mtcars dataset for purpose of parallel processing
# bug detection
#
# Code performs a grid search of random forest hyperparameters.
#
# Grid is created in line 60
#
# Parallel processing parameters are set at line 84
#
# Model training and grid search is performed at line 89
#
# ==============================================================================

library(doParallel)
library(caret)
library(tidyverse)
library(glue)

# =================================== Setup ==================================== 

# -------------------------------- Dataset prep -------------------------------- 

df <- mtcars

response_var <- 'mpg'

predictor_var <- df %>%
  select(-mpg) %>%
  colnames()

n_predictors <- length(predictor_var)


# ------------------------------ Model parameters ------------------------------ 

k_folds <- 10
rep <- 10

pre_process = c('center', 'scale')

splitrule = c('variance', 'extratrees')
min.node.size = c(1,3,5)
mtry = 2:n_predictors

set_seed_val = 111


# ------------------------------ Search grid setup -----------------------------

rf_grid <- expand.grid(mtry = mtry,
                       splitrule = splitrule,
                       min.node.size = min.node.size)



# ==============================================================================  
# ----------------------------- Grid model training ---------------------------- 
# ==============================================================================
  
# -------------------------------- Control setup -------------------------------

train_ctrl <- trainControl(
  method = "repeatedcv",
  number = k_folds,
  repeats = rep
)

# -------------------------- Parallel processing setup ------------------------- 

# This is the extent of the control I exert over the current setup of the parallel
# processing. I have it set to use all cores but 5 but besides that, I am letting
# doParallel take care of the rest.

cl <- makeCluster(detectCores() - 5)
registerDoParallel(cl)

# ---------------------------------- Training ---------------------------------- 
  
rf_train <- train(
  x = df %>%
    select(all_of(predictor_var)),
  y = df %>%
    pull(response_var),
  method = "ranger",
  preProcess = pre_process,
  trControl = train_ctrl,
  tuneGrid = rf_grid,
  metric = "RMSE"
)
  
stopCluster(cl)

# ==============================================================================