library(tidyverse)

c4_file <- 'data/field/data_cleaning/ltr_c4_field_ssu.csv'
cfi <- 'data/field/data_cleaning/ltr_c4_field_cfi.csv'

c4 <- read_csv(c4_file, na = c('', 'NA', 'na'))

cfi <- read_csv(cfi)


# ---------------------------- Missing plot numbers ---------------------------- 

c4_plot <- unique(c4$plot) 
cfi_plot <- unique(cfi$plot)

missing_c4 <- c4_plot[!(c4_plot %in% cfi_plot)]
missing_cfi <- cfi_plot[!(cfi_plot %in% c4_plot)]


# --------------------------- Duplicate tag testing ----------------------------

c4 %>%
  group_by(plot, tag) %>%
  summarize(
    count = n()
  ) %>% 
  filter(count > 1)


# --------------------------- Joining data together ----------------------------

combo <- inner_join(c4, cfi, by = c('plot', 'tag'), suffix = c('_c4', '_cfi'))
write_csv(combo, 'data/field/data_cleaning/ltr_c4_field_inner_join.csv')


c4_miss <- anti_join(c4, cfi, by = c('plot', 'tag'))
write_csv(c4_miss, 'data/field/data_cleaning/ltr_c4_field_ssu_anti_join.csv')

cfi_miss <- anti_join(cfi, c4, by = c('plot', 'tag'))
write_csv(cfi_miss, 'data/field/data_cleaning/ltr_c4_field_cfi_anti_join.csv')

