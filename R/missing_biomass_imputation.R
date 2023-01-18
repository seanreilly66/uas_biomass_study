library(tidyverse)

tree_data <- read_csv('data/field/biomass/tree_biomass.csv')

height_model <- lm(HT_m ~ DBH_cm, data = tree_data)

tree_data <- tree_data %>%
  mutate(HT_m = if_else(
    is.na(HT_m),
    true = predict(height_model, newdata = .),
    false = HT_m
  ))

biomass_model <- lm(biomass ~ HT_m * DBH_cm,
               data = tree_data)

tree_data <- tree_data %>%
  mutate(biomass = if_else(
    is.na(biomass),
    true = predict(biomass_model, newdata = .),
    false = biomass
  ))

write_csv(tree_data, 'data/field/biomass/tree_biomass_imput.csv')







plot_list <- read_csv('data/las/plots/plot_list.csv')

tree_data <- tree_data %>%
  semi_join(plot_list, by = c('campaign', 'plot'))

species <- unique(tree_data$species)

tree_data <- tree_data %>%
  mutate(campaign = as.factor(campaign))

for (i in species) {
  
  ggplot(data = tree_data %>%
           filter(species == i),
         mapping = aes(x = species,
                       y = biomass)) +
    geom_jitter(aes(color = campaign)) +
    geom_boxplot(fill = NA) +
    labs(y = 'Biomass (Mg)',
         x = NULL) +
    scale_color_brewer(type = 'qual', palette = 'Pastel1') +
    theme(legend.position = 'bottom')
  
  ggsave(glue('figures/biomass/{i}.png'),
         width = 3,
         height = 5,
         units = 'in',
         dpi = 300)
  
  
}

l_species <- c('ARCMAN', 'HETARB', 'MYRCAL', 'QUEAGR', NA)

ggplot(data = tree_data %>%
         filter(species %in% l_species),
       mapping = aes(x = species,
                     y = biomass)) +
  geom_jitter(aes(color = campaign)) +
  geom_boxplot(fill = NA) +
  labs(y = 'Biomass (Mg)',
       x = NULL) +
  scale_color_brewer(type = 'qual', palette = 'Pastel1') +
  theme(legend.position = 'bottom')

ggsave('figures/biomass/shrub_species.png', width = 7, height = 4, units = 'in', dpi = 300)

ggplot(data = tree_data,
       mapping = aes(x = species,
                     y = biomass)) +
  geom_jitter(aes(color = campaign)) +
  geom_boxplot(fill = NA) +
  labs(y = 'Biomass (Mg)',
       x = NULL) +
  scale_color_brewer(type = 'qual', palette = 'Pastel1') +
  theme(legend.position = 'bottom')

ggsave(glue('figures/biomass/all_species.png'),
       width = 25,
       height = 10,
       units = 'in',
       dpi = 300)



