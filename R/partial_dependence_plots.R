library(tidyverse)
library(caret)
library(randomForest)
library(ggpubr)

ml <- readRDS('data/ml_output/rf_svm_testing_model_20220323_1124.RData')

# ==============================================================================
# =========================== Training data ===========================
# ==============================================================================

response_csv <- 'data/field/plot_field_metrics.csv'
predictor_csv <- 'data/las/metrics/uas_plot_metrics.csv'


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


# ==============================================================================
training_model = ml$lai_mean_rf_rfe 
response_stub = 'lai'


pdp_plot <- function(training_model,
                     response_stub,
                     training_data = model_df,
                     response = response_var,
                     predictor = predictor_var) {
  
  response_name = str_subset(response, response_stub)
  
  input_df <- training_data %>%
    filter(!is.na(!!sym(response_name))) %>%
    select(all_of(c(response_name, predictor))) %>%
    as.data.frame()
  
  input_df[is.na(input_df)] <-  -9999
  
  input_df <- predict(preProcess(input_df, method = c("center", "scale")), input_df)
  
  ml_varimp <- training_model %>%
    varImp() %>%
    .$importance %>%
    rownames_to_column() %>%
    tibble() %>%
    rename(Predictor = rowname,
           Importance = Overall) %>%
    slice_max(order_by = Importance, n = 5)
  
  
  
  var1 = partialPlot(training_model$finalModel,
                     pred.data = input_df,
                     x.var = ml_varimp$Predictor[1]) %>%
    as.data.frame() %>%
    add_column(var = ml_varimp$Predictor[1])
  
  var2 = partialPlot(training_model$finalModel,
                     pred.data = input_df,
                     x.var = ml_varimp$Predictor[2]) %>%
    as.data.frame() %>%
    add_column(var = ml_varimp$Predictor[2])
  
  var3 = partialPlot(training_model$finalModel,
                     pred.data = input_df,
                     x.var = ml_varimp$Predictor[3]) %>%
    as.data.frame() %>%
    add_column(var = ml_varimp$Predictor[3])
  
  var4 = partialPlot(training_model$finalModel,
                     pred.data = input_df,
                     x.var = ml_varimp$Predictor[4]) %>%
    as.data.frame() %>%
    add_column(var = ml_varimp$Predictor[4])
  
  var5 = partialPlot(training_model$finalModel,
                     pred.data = input_df,
                     x.var = ml_varimp$Predictor[5]) %>%
    as.data.frame() %>%
    add_column(var = ml_varimp$Predictor[5])
  
  pdp_df <- var1 %>%
    add_row(var2) %>%
    add_row(var3) %>%
    add_row(var4) %>%
    add_row(var5)
  
  var_df <- training_data %>%
    select(ml_varimp$Predictor[1:5]) %>%
    add_column(y = mean(pdp_df$y)) %>%
    pivot_longer(
      cols = all_of(ml_varimp$Predictor[1:5]),
      names_to = 'var',
      values_to = 'x'
    )
  
  plot_output = ggplot(mapping = aes(x = x,
                       y = y,
                       color = var)) +
    geom_line(data = pdp_df) +
    geom_rug(data = var_df,
             sides = 'b') +
    scale_color_brewer(type = 'qual')
  
  return(plot_output)
  
}


theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(size = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = ggplot2::margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)

lai <- ml$lai_mean_rf_rfe %>%
  pdp_plot(response_stub = 'lai')
# 
# cbd <- ml$cbd_mean_rf_rfe
# 
# cbh <- ml$cbh_rf_rfe
# 
# cc <- ml$densiometer_mean_rf_rfe
# 
# h <- ml$h_mean_rf_rfe
# 
# biomass <- ml$biomass_sum_rf_rfe
# 
# 
# 
# data = biomass
# var_name = 'biomass'

