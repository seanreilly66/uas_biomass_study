library(tidyverse)
library(sf)
library(geoR)
library(gstat)
library(glue)
library(ggpubr)

# ================================ GGplot theme ================================ 
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(linewidth = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)

# ================================= Data prep ================================== 

field_data <- read_csv('data/field/plot_field_metrics.csv')
field_loc <- read_sf('data/boundaries/ssu_3dforests.gdb', 'field_plots') %>%
  mutate(plot = as.numeric(plot)) %>%
  rename(x = Easting, y = Northing)

field_data <- left_join(field_data, field_loc)


var_names = c('biomass_sum', 'h_mean', 'cbh', 'densiometer_mean', 'cbd_mean', 'lai_mean')

field_data <- field_data %>%
  select(x, y, all_of(var_names)) 

var_labels = tibble(col_name = var_names,
                    label = c('Biomass', 'Mean height', 'CBH', 'CC', 'CBD', 'LAI'))



# ============================== gstat variogram ===============================

plots_list <- list()
plot_label <- list()

i = 1
b_width = 100
mdl = 'Exp'





if (exists('gstat_fit')) rm('gstat_fit')

field_var <- var_names[i]
field_var

var_data <- field_data %>%
  filter(if_any(field_var, ~!is.na(.)))

gstat_var <- variogram(as.formula(glue('{field_var} ~ 1')),
                       locations = ~ x + y,
                       data = var_data,
                       cutoff = 5000,
                       width = b_width)

plot(gstat_var)

gstat_fit <- fit.variogram(gstat_var, vgm(39, mdl, 50))
gstat_fit

gstat_fit_line <- variogramLine(gstat_fit, maxdist = 5000)

plot(gstat_var, model = gstat_fit)

plot_label[[field_var]] <- tibble(
  x = min(gstat_var$dist),
  y = max(gstat_var$gamma),
  label = glue('{var_labels %>%
  filter(col_name == field_var) %>%
  pull(label)}
Partial sill: {signif(gstat_fit$psill, 3)}
Range: {signif(gstat_fit$range, 3)}'))

plots_list[[field_var]] <- ggplot(mapping = aes(x = dist, y = gamma)) +
  geom_point(data = gstat_var) +
  geom_line(data = gstat_fit_line, color = 'firebrick') +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = gstat_fit$range, linetype = 'dashed') +
  geom_label(data = plot_label[[field_var]], aes(x = x, y = y, label = label), 
            hjust = 'inward', 
            vjust = 'inward', 
            family = 'serif', 
            fontface = 'plain',
            label.size = NA,
            size = 5)

i = i+1



gstat_plot <- ggarrange(plotlist = plots_list, align = 'hv', ncol = 2, nrow = 3) %>%
  annotate_figure(left = text_grob('Semivariance', family = 'serif', size = 16, rot = 90),
                bottom = text_grob('Distance (m)', family = 'serif', size = 16),
                top = text_grob(glue('Model: {mdl}  Bin: {b_width}'), family = 'serif', size = 16))

gstat_plot


ggsave(gstat_plot,
       filename = glue('figures/gstat_variog_{mdl}_bin{b_width}.png'), width = 8.5, height = 10, units = 'in', dpi = 700)


# =============================== geoR variogram ===============================

# 
# field_geodata <- field_data %>%
#   select(x, y, all_of(field_var)) %>%
#   as.geodata(coords.col = 1:2)
# 
# plot(field_geodata)
# 
# br = seq(1, 5000, 100)
# 
# i_variog <- variog(
#   field_geodata,
#   breaks = br,
#   lamda = lmda,
#   # max.dist = 5000,
#   # pairs.min = 5,
#   # estimator.type = 'modulus'
# )
# 
# plot(i_variog)
# 
# i_ml <- variofit(
#   i_variog)
# 
# i_ml
# 
# plot(i_variog, main = 'geoR'); lines(i_ml, col='red')
# 
#   
