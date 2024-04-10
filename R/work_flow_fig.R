library(tidyverse)
library(ggflowchart)


work_flow <- tribble(
  ~from, ~to,
  'UAS imagery', 'UAS-SfM\npoint cloud',
  'UAS imagery', 'Orthomosaic',
  'ALS LAS', 'ALS DTM\n(All sites)',
  'ALS LAS', 'Vertical distribution\n(Jackson & LaTour)',
  'Vertical distribution\n(Jackson & LaTour)', 'ALS RF',
  'Field fuel\nparameters', 'ALS RF',
  'ALS RF', 'ALS model\ncomparison',
  'ALS DTM\n(All sites)', 'Height normalized\npoint cloud',
  'UAS-SfM\npoint cloud','Height normalized\npoint cloud',
  'Height normalized\npoint cloud', 'Vertical distribution\npredictors',
  'Height normalized\npoint cloud', 'Height band spectral\npredictors',
  'Orthomosaic', 'Height band spectral\npredictors',
  'Orthomosaic',  'Raster spectral\npredictors',
  'Field fuel\nparameters', 'Machine learning with\nspatial fold CV',
  'Vertical distribution\npredictors', 'Machine learning with\nspatial fold CV',
  'Height band spectral\npredictors', 'Machine learning with\nspatial fold CV',
  'Raster spectral\npredictors', 'Machine learning with\nspatial fold CV',
  'Machine learning with\nspatial fold CV', 'LM',
  'Machine learning with\nspatial fold CV', 'SVM\nwith RFE',
  'Machine learning with\nspatial fold CV', 'RF\nwith RFE',
  'RF\nwith RFE', 'Novel site\ntesting',
  'RF\nwith RFE', 'Prediction\nmaps',
  'RF\nwith RFE', 'Predictor subset\nmodel testing',
  'RF\nwith RFE', 'ALS model\ncomparison',
  'RF\nwith RFE', 'Variable\nimportance')

node_data <- tribble(
  ~name, ~x, ~y, ~type
  'UAS imagery', 1.5, 7, 'uas',
  'UAS-SfM\npoint cloud', 2, 6, 'uas',
  'Orthomosaic', 1, 6, 'uas',
  'ALS LAS', 3.5, 7, 'als',
  'ALS DTM\n(All sites)', 3, 6, 'als',
  'Vertical distribution\n(Jackson & LaTour)', 5, 5, 'als',
  'ALS RF', 5, 3, 'als',
  'Height normalized\npoint cloud', 2.5, 5, 'uas',
  'Vertical distribution\npredictors', 3, 4, 'vert',
  'Height band spectral\npredictors', 2, 4, 'hband',
  'Raster spectral\npredictors', 1, 4, 'rast',
  'Field fuel\nparameters', 4, 4, 'field',
  'Machine learning with\nspatial fold CV', 2, 3, 'analysis',
  'LM', 1, 2, 'lm',
  'SVM\nwith RFE', 2, 2, 'svm',
  'RF\nwith RFE', 3, 2, 'rf',
  'Prediction\nmaps', 1, 1, 'analysis',
  'Variable\nimportance', 2, 1, 'analysis',
  'Predictor subset\nmodel testing', 3, 1, 'analysis',
  'ALS model\ncomparison', 5, 1, 'analysis', 
  'Novel site\ntesting', 4, 1, 'analysis'
)

col_pal = c(
  'uas' = 'white', 
  'als' = 
)

ggflowchart(data = work_flow, 
            node_data = node_data, 
            layout = 'custom',
            family = 'serif',
            arrow_size = 0.2,
            x_nudge = 0.45)

ggsave(
  filename = 'figures/manuscript/work_flow.png',
  width = 9,
  height = 9,
  units = 'in',
  dpi = 700,
  bg = 'white'
)

