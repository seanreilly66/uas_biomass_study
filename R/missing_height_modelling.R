# Height modelling using linear regression

library(tidyverse)

# LTR setup

input = 'data/field/ltr_c4_field_joined.csv'
output = 'data/field/ltr_c4_field_biomass_input.csv'

df <- input %>%
  read_csv() %>%
  mutate(DBH_cm = dbh_cfi*2.54,
         HT_m = height_cfi*0.3048) %>%
  dplyr::select(plot, tag, species, DBH_cm, HT_m)

# Pepperwood setup

input = 'data/field/ppwd_c1_field_joined.csv'
output = 'data/field/ppwd_c1_field_biomass_input.csv'

df <- input %>%
  read_csv() %>%
  rename(DBH_cm = dbh_cm,
         HT_m = height_m) %>%
  select(plot, tag, species, DBH_cm, HT_m)







regression_table <- df %>%
  nest_by(species) %>%
  mutate(lm = list(lm(HT_m ~ DBH_cm, data = data))) 


h_model <- function(sp, dbh_val, ht, reg_table) {
  
  if (!is.na(ht) | is.na(sp)) {
    
    h = ht
    
  } else {
    
    lm_model = regression_table %>%
      filter(species == !!sp)
    
    lm_model = lm_model$lm[[1]]
    
    dbh_df = as.data.frame(dbh_val) %>%
      rename(DBH_cm = 1)
    
    h = predict.lm(lm_model, newdata = dbh_df)
    
  }
  
  return(h)
}

df <- df %>%
  rowwise() %>%
  mutate(HT_m = h_model(sp = species, dbh_val = DBH_cm, ht = HT_m, reg_table = regression_table))

write_csv(df, output)
