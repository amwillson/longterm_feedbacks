## Plotting fractional composition over time Upper Midwest

rm(list = ls())

library(tidyr)
library(ggplot2)
library(dplyr)

# Load data
load('FossilPollen/Data/full_melt.RData')

## format data

full_format <- full_melt |>
  pivot_longer(cols = c(ash, beech, birch, elm, hemlock, maple, oak, other_conifer,
                        other_hardwood, pine, spruce, tamarack))
colnames(full_format) <- c('time', 'long', 'lat', 'loc_time', 'taxon', 'fc')

## 1. stacked bars

# full dataset

full_format |>
  rename(Taxon = taxon) |>
  mutate(Taxon = if_else(Taxon == 'ash', 'Ash', Taxon),
         Taxon = if_else(Taxon == 'beech', 'Beech', Taxon),
         Taxon = if_else(Taxon == 'birch', 'Birch', Taxon),
         Taxon = if_else(Taxon == 'elm', 'Elm', Taxon),
         Taxon = if_else(Taxon == 'hemlock', 'Hemlock', Taxon),
         Taxon = if_else(Taxon == 'maple', 'Maple', Taxon),
         Taxon = if_else(Taxon == 'oak', 'Oak', Taxon),
         Taxon = if_else(Taxon == 'other_conifer', 'Other conifer', Taxon),
         Taxon = if_else(Taxon == 'other_hardwood', 'Other hardwood', Taxon),
         Taxon = if_else(Taxon == 'pine', 'Pine', Taxon),
         Taxon = if_else(Taxon == 'spruce', 'Spruce', Taxon),
         Taxon = if_else(Taxon == 'tamarack', 'Tamarack', Taxon)) |>
  group_by(time, Taxon) |>
  summarize(fc = mean(fc)) |>
  ggplot(aes(x = time, y = fc, fill = Taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition over time in the Upper Midwest') +
  xlab('Time (years before 1950)') +
  ylab('Relative Composition') +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# sample locations
full_format <- full_format |>
  mutate(loc = paste0(long,'_',lat))
locs = unique(full_format$loc)

full_format |>
  mutate(taxon = if_else(taxon == 'ash', 'Ash', taxon),
         taxon = if_else(taxon == 'beech', 'Beech', taxon),
         taxon = if_else(taxon == 'birch', 'Birch', taxon),
         taxon = if_else(taxon == 'elm', 'Elm', taxon),
         taxon = if_else(taxon == 'hemlock', 'Hemlock', taxon),
         taxon = if_else(taxon == 'maple', 'Maple', taxon),
         taxon = if_else(taxon == 'oak', 'Oak', taxon),
         taxon = if_else(taxon == 'other_conifer', 'Other Conifer', taxon),
         taxon = if_else(taxon == 'other_hardwood', 'Other Hardwood', taxon),
         taxon = if_else(taxon == 'pine', 'Pine', taxon),
         taxon = if_else(taxon == 'spruce', 'Spruce', taxon),
         taxon = if_else(taxon == 'tamarack', 'Tamarack', taxon)) |>
  rename(Taxon = taxon) |>
  filter(loc == sample(locs, 1)) |>
  ggplot(aes(x = time, y = fc, fill = Taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Relative community composition in one grid cell') +
  xlab('Time (years before present)') +
  ylab('Relative Composition') +
  scale_fill_manual(breaks = c('Ash', 'Beech', 'Birch', 'Elm',
                               'Hemlock', 'Maple', 'Oak', 'Other Conifer',
                               'Other Hardwood', 'Pine', 'Spruce', 'Tamarack'),
                    values = c('#d1bbd7', '#ae76a3', '#882e72', '#1965b0',
                               '#5289c7', '#7bafde', '#4eb265', '#90c987',
                               '#cae0ab', '#f7f056', '#f6c141', '#f1932d')) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

## 2. Lines of composition per species

full_format |>
  rename(Taxon = taxon) |>
  mutate(Taxon = if_else(Taxon == 'ash', 'Ash', Taxon),
         Taxon = if_else(Taxon == 'beech', 'Beech', Taxon),
         Taxon = if_else(Taxon == 'birch', 'Birch', Taxon),
         Taxon = if_else(Taxon == 'elm', 'Elm', Taxon),
         Taxon = if_else(Taxon == 'hemlock', 'Hemlock', Taxon),
         Taxon = if_else(Taxon == 'maple', 'Maple', Taxon),
         Taxon = if_else(Taxon == 'oak', 'Oak', Taxon),
         Taxon = if_else(Taxon == 'other_conifer', 'Other conifer', Taxon),
         Taxon = if_else(Taxon == 'other_hardwood', 'Other hardwood', Taxon),
         Taxon = if_else(Taxon == 'pine', 'Pine', Taxon),
         Taxon = if_else(Taxon == 'spruce', 'Spruce', Taxon),
         Taxon = if_else(Taxon == 'tamarack', 'Tamarack', Taxon)) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme_minimal() +
  xlab('Time (years before 1950)') + ylab('Relative composition') +
  scale_color_viridis_d() +
  ylim(0, 1) +
  facet_wrap(~Taxon) +
  theme(legend.position = 'none',
        strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

states <- map_data('state') |>
  filter(region %in% c('michigan', 'wisconsin', 'minnesota'))

full_melt |>
  select(time, ash, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP', '1700 YBP',
                                        '1600 YBP', '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                                        '1100 YBP', '1000 YBP', '900 YBP', '800 YBP', '700 YBP',
                                        '600 YBP', '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = ash)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nash') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, beech, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = beech)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nbeech') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, birch, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = birch)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nbirch') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, elm, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = elm)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  labs(color = 'Proportion\nelm') +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, hemlock, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                                        '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                                        '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                                        '700 YBP', '600 YBP', '500 YBP', '400 YBP',
                                        '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = hemlock)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nhemlock') +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, maple, lat, long) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = maple)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nmaple') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, oak, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = oak)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\noak') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, other_conifer, lat, long) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = other_conifer)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nother conifer') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, other_hardwood, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = other_hardwood)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nother hardwood') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, pine, lat, long) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = pine)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\npine') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, spruce, lat, long) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = spruce)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\nspruce') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

full_melt |>
  select(time, tamarack, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = factor(time, levels = c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP',
                                        '1700 YBP', '1600 YBP', '1500 YBP', '1400 YBP',
                                        '1300 YBP', '1200 YBP', '1100 YBP', '1000 YBP',
                                        '900 YBP', '800 YBP', '700 YBP', '600 YBP',
                                        '500 YBP', '400 YBP', '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = tamarack)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  coord_map(projection = 'albers', lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal', limits = c(0, 1)) +
  labs(color = 'Proportion\ntamarack') +
  theme(strip.text = element_text(size = 14, face = 'bold', hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))