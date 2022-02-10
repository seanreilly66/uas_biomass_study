library(tidyverse)


x <- 'data/field/data_cleaning/ppwd_c1_field_ssu.csv'
y <- 'data/field/data_cleaning/ppwd_c1_field_ucb.csv'

bent <- read_csv(x) 
ack <- read_csv(y)

# b_plot <- unique(bent$plot)
# a_plot <- unique(ack$plot)
# 
# b_tag <- unique(bent$tag)
# a_tag <- unique(ack$tag)
# 
# bent %>%
#   group_by(tag) %>%
#   summarize(
#     count = n()
#   ) %>% 
#   filter(count > 1)
# 
# ack %>%
#   group_by(tag) %>%
#   summarize(
#     count = n()
#   ) %>% 
#   filter(count > 1)

combo <- inner_join(bent, ack, by = c('plot', 'tag'), suffix = c('_ssu', '_ucb'))
write_csv(combo, 'data/field/data_cleaning/ppwd_c1_field_inner_join.csv')


bent_dif <- anti_join(bent, ack, by = c('plot', 'tag')) %>%
  rename(species_ssu = species,
         dbh_cm_ssu = dbh_cm)
write_csv(bent_dif, 'data/field/data_cleaning/ppwd_c1_field_ssu_anti_join.csv')

ack_dif <- anti_join(ack, bent, by = c('plot', 'tag')) %>%
  filter(dbh_cm >= 10)
write_csv(ack_dif, 'data/field/data_cleaning/ppwd_c1_field_ucb_anti_join.csv')


joined <- left_join(bent, ack, by = c('plot', 'tag'), suffix = c('_ssu', '_ucb')) %>%
  arrange(plot, tag) %>%
  mutate(
    dbh_cm = coalesce(dbh_cm_ucb, dbh_cm_ssu),
    species = coalesce(species_ucb, species_ssu)
  )
write_csv(joined, 'data/field/ppwd_c1_field_joined.csv')
