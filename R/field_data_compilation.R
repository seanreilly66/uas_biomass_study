library(tidyverse)

plot_list <- read_csv('data/las/plots/plot_list.csv')


lai <- read_csv('data/field/lai.csv') %>%
  rename(lai_n = n,
         lai_mean = mean_lai)

cbd <- read_csv('data/field/cbd.csv') %>%
  rename(cbd_n = n,
         cbd_mean = mean_cbd)

cbh <- read_csv('data/field/cbh.csv') %>%
  rename(cbh_n = n,
         cbh_na = n_na)

dens <- read_csv('data/field/densiometer.csv') %>%
  rename(campaign = Campaign,
         plot = Plot,
         densiometer_mean = mean_densiometer) %>%
  select(campaign, plot, densiometer_mean)



biomass <- read_csv('data/field/biomass/tree_biomass.csv') %>%
  select(campaign, plot, biomass) %>%
  group_by(campaign, plot) %>%
  summarize(
    biomass_sum = sum(biomass, na.rm = TRUE),
    biomass_na = sum(is.na(biomass)),
    biomass_n = length(biomass)
  )

height <- read_csv('data/field/height.csv')

field_data <- plot_list %>%
  left_join(lai) %>%
  left_join(cbd) %>%
  left_join(cbh) %>%
  left_join(dens) %>%
  left_join(biomass) %>%
  left_join(height)

write_csv(field_data, 'data/field/plot_field_metrics.csv')

