library(tidyverse)
library(ggflowchart)


work_flow <- tribble(
  ~from, ~to,
  'UAS imagery', 'UAS-SfM\npoint cloud',
  'UAS imagery', 'Orthomosaic',
  'ALS DTM', 'Height normalized\npoint cloud',
  'UAS-SfM\npoint cloud','Height normalized\npoint cloud',
  'Height normalized\npoint cloud', 'Vertical distribution\npredictors',
  'Height normalized\npoint cloud', 'Height band spectral\npredictors',
  'Orthomosaic', 'Height band spectral\npredictors',
  'Orthomosaic',  'Raster spectral\npredictors',
  'Field fuel\nparameters', 'Machine learning',
  'Vertical distribution\npredictors', 'Machine learning',
  'Height band spectral\npredictors', 'Machine learning',
  'Raster spectral\npredictors', 'Machine learning',
  'Machine learning with\nspatial fold CV', 'LM',
  'Machine learning with\nspatial fold CV', 'SVM with RFE',
  'Machine learning with\nspatial fold CV', 'RF with RFE',
  'RF with RFE', 'Novel site testing',
  'RF with RFE', 'Prediction maps',
  'RF with RFE', 'Predictor subset\nmodel testing',
  'RF with RFE', 'ALS model comparison',
  'RF with RFE', 'Variable importance')

node_data <- tribble(
  ~name, ~x, ~y,
  'UAS imagery', 1.5, 7,
  'UAS-SfM\npoint cloud', 2, 6,
  'Orthomosaic', 1, 6,
  'ALS DTM', 3, 6,
  'Height normalized\npoint cloud', 2.5, 5,
  'Vertical distribution\npredictors', 3, 4,
  'Height band spectral\npredictors', 2, 4,
  'Raster spectral\npredictors', 1, 4,
  'Field fuel\nparameters', 4, 4,
  'Machine learning with\nspatial fold CV', 2, 3,
  'LM', 1, 2,
  'SVM with RFE', 2, 2,
  'RF with RFE', 3, 2,
  'Prediction maps', 1, 1,
  'Variable importance', 2, 1,
  'Predictor subset\nmodel testing', 3, 1,
  'ALS model comparison', 4, 1,
  'Novel site testing', 5, 1
)

ggflowchart(data = work_flow, 
            node_data = node_data, 
            layout = 'custom',
            family = 'serif',
            arrow_size = 0.2)

ggsave(
  filename = 'figures/manuscript/work_flow.png',
  width = 8,
  height = 8,
  units = 'in',
  dpi = 700,
  bg = 'white'
)

