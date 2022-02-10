library(tidyverse)

source('R/biomass_equations.R')

# ================================= Pepperwood ================================= 

ppwd_t <- 'data/field/ppwd_c1_field_biomass_input.csv' %>%
  read_csv() %>%
  filter(DBH_cm > 10) %>%
  rowwise() %>%
  mutate(biomass = biomass(species = species, DBH_cm = DBH_cm, HT_m = HT_m)) %>%
  add_column(campaign = 1, .before = 1)

write_csv(ppwd_t, 'data/field/biomass/ppwd_c1_tree_biomass.csv')

ppwd_p <- ppwd_t %>%
  group_by(plot) %>%
  summarize(
    n_trees = n(),
    total_biomass = sum(biomass, na.rm = TRUE),
    na_trees = sum(is.na(biomass))
  ) %>%
  add_column(campaign = 1, .before = 1)

write_csv(ppwd_p, 'data/field/biomass/ppwd_c1_plot_biomass.csv')

# =================================== LaTour =================================== 

ltr_t <- 'data/field/ltr_c4_field_biomass_input.csv' %>%
  read_csv() %>%
  filter(DBH_cm > 10) %>%
  rowwise() %>%
  mutate(biomass = biomass(species = species, DBH_cm = DBH_cm, HT_m = HT_m)) %>%
  add_column(campaign = 4, .before = 1)

write_csv(ltr_t, 'data/field/biomass/ltr_c4_tree_biomass.csv')

ltr_p <- ltr_t %>%
  group_by(plot) %>%
  summarize(
    n_trees = n(),
    total_biomass = sum(biomass, na.rm = TRUE),
    na_trees = sum(is.na(biomass))
  ) %>%
  add_column(campaign = 4, .before = 1)

write_csv(ltr_p, 'data/field/biomass/ltr_c4_plot_biomass.csv')

# =================================== Jackson ================================== 

jck_t <- 'data/field/jcksn_field_biomass_input.csv' %>%
  read_csv() %>%
  filter(DBH_cm > 10) %>%
  rowwise() %>%
  mutate(biomass = biomass(species = species, DBH_cm = DBH_cm, HT_m = HT_m))

write_csv(jck_t, 'data/field/biomass/jcksn_tree_biomass.csv')

jck_p <- jck_t %>%
  group_by(plot) %>%
  summarize(
    n_trees = n(),
    total_biomass = sum(biomass, na.rm = TRUE),
    na_trees = sum(is.na(biomass))
  ) %>%
  add_column(campaign = 3, .before = 1)

write_csv(jck_p, 'data/field/biomass/jcksn_plot_biomass.csv')

# =============================== Saddle Mountain ============================== 

sdl_t <- 'data/field/sdlmtn_c6_field_biomass_input.csv' %>%
  read_csv() %>%
  filter(DBH_cm > 10) %>%
  rowwise() %>%
  mutate(biomass = biomass(species = species, DBH_cm = DBH_cm, HT_m = HT_m)) %>%
  add_column(campaign = 6, .before = 1)

write_csv(sdl_t, 'data/field/biomass/sdlmtn_c6_tree_biomass.csv')

sdl_p <- sdl_t %>%
  group_by(plot) %>%
  summarize(
    n_trees = n(),
    total_biomass = sum(biomass, na.rm = TRUE),
    na_trees = sum(is.na(biomass))
  ) %>%
  add_column(campaign = 6, .before = 1)

write_csv(sdl_p, 'data/field/biomass/sdlmtn_c6_plot_biomass.csv')

# =============================== Combined data ================================ 

tree_data <- ppwd_t %>%
  add_row(jck_t) %>%
  add_row(ltr_t) %>%
  add_row(sdl_t)

write_csv(tree_data, 'data/field/biomass/tree_biomass.csv')

plot_data <- ppwd_p %>%
  add_row(jck_p) %>%
  add_row(ltr_p) %>%
  add_row(sdl_p)

write_csv(plot_data, 'data/field/biomass/plot_biomass.csv')




tree_data <- read_csv('data/field/biomass/tree_biomass.csv')
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

ggplot(data = tree_data %>%
         filter(!is.na(species)),
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


