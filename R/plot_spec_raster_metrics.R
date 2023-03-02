# Raster plot value extraction

# =================================== Libraries ================================

library(tidyverse)
library(terra)
library(sf)
library(foreach)
library(glue)
library(doParallel)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

shp_gdb <- 'data/boundaries/ssu_3dforests.gdb'
shp_layer <- 'field_plots'

spectral_folder <- 'data/spectral'

c_output <- 'data/las/metrics/spectral_plot_metrics_c{i_c}.csv'
final_output <- 'data/las/metrics/spectral_plot_metrics_temp.csv'

n_cores <- detectCores() - 2

# ==============================================================================
# ================================== Data prep =================================
# ==============================================================================


shp <- read_sf(shp_gdb, shp_layer)

c = unique(shp$campaign)

band_files <- list.files(path = spectral_folder,
                         pattern = glue('.tif$'),
                         full.names = TRUE)

# ==============================================================================
# ============================== Raster extraction =============================
# ==============================================================================

# c <- c[3:4]

# campaign loop

r_values <- foreach (
  i_c = c,
  .combine = 'rbind'
) %do% {
  
  i_band_files <- str_subset(band_files, pattern = glue('_c{i_c}'))
  
  z <- str_extract(i_band_files, pattern = '(?<=_z)[:digit:]+') %>%
    as.numeric() %>%
    unique()
  
  i_shp <- shp %>%
    filter(campaign == i_c)
  
  # z <- z[13:17]

  # zone loop
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  r_campaign <- foreach (
    j_z = z,
    .packages = c('tidyverse', 'terra', 'sf', 'glue'),
    .combine = 'rbind'
  ) %dopar% {
    
    r = i_band_files %>%
      str_subset(pattern = glue('_z{j_z}_')) %>%
      rast()
    
    names(r) <- str_extract(names(r), pattern = '[:alpha:]+$')
    
    ndvi <- (r$nir - r$red) / (r$nir + r$red)
    names(ndvi) <- 'ndvi'
    ndre <- (r$nir - r$rededge) / (r$nir + r$rededge)
    names(ndre) <- 'ndre'
    gndvi <- (r$nir - r$green) / (r$nir + r$green)
    names(gndvi) <- 'gndvi'
    
    r <- c(r, ndvi, ndre, gndvi)
    
    r_mean <-  extract(r, vect(i_shp), fun = mean) %>%
      add_column(i_shp %>%
                   st_drop_geometry %>%
                   select(campaign, plot),
                 .before = 1) %>%
      drop_na() %>%
      select(-ID) %>%
      rename_with(.cols = names(r),
                  .fn = ~ glue('{names(r)}_mean'))
    
    r_sd <- extract(r, vect(i_shp), fun = sd) %>%
      add_column(i_shp %>%
                   st_drop_geometry %>%
                   select(campaign, plot),
                 .before = 1) %>%
      drop_na() %>%
      select(-ID) %>%
      rename_with(.cols = names(r),
                  .fn = ~ glue('{names(r)}_sd'))
    
    r_join <- left_join(r_mean, r_sd) %>%
      add_column(zone = j_z)
    
  }
  
  stopCluster(cl)
  
  write_csv(r_campaign, glue(c_output))
  
  r_campaign <- r_campaign
 
}

r_values  

write_csv(r_values, glue(final_output))
