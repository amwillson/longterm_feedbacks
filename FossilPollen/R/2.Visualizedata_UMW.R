## Plotting fractional composition over time
## Upper Midwest

rm(list = ls())

library(tidyverse)

# Load data
load('FossilPollen/Data/full_melt_UMW.RData')

## format data

full_format <- full_melt |>
  pivot_longer(cols = c(ash, beech, birch, elm, hemlock, maple, oak, other_conifer,
                        other_hardwood, pine, spruce, tamarack))
#colnames(full_format) <- c('x', 'y', 'time', 'loc', 'loc_time', 'taxon', 'fc')
colnames(full_format) <- c('time', 'long', 'lat', 'loc_time', 'taxon', 'fc')

## 1. stacked bars

# full dataset

full_format |>
  ggplot(aes(x = time * 100, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition over time in the Upper Midwest') +
  xlab('Time (years before 1950)') +
  ylab('Relative Composition') +
  scale_fill_viridis_d()

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
  filter(loc == sample(locs, 1)) |>
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition in one 8 x 8 km grid cell') +
  xlab('Time (years before present)') +
  ylab('Fractional Composition') +
  scale_fill_manual(breaks = c('Ash', 'Beech', 'Birch', 'Elm',
                               'Hemlock', 'Maple', 'Oak', 'Other Conifer',
                               'Other Hardwood', 'Pine', 'Spruce', 'Tamarack'),
                    values = c('#d1bbd7', '#ae76a3', '#882e72', '#1965b0',
                               '#5289c7', '#7bafde', '#4eb265', '#90c987',
                               '#cae0ab', '#f7f056', '#f6c141', '#f1932d'),
                    name = 'Taxon') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

## 2. Lines of composition per species

specs = unique(full_format$taxon)

full_format |>
  filter(taxon == specs[1]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Ash fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[2]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Beech fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[3]) |>
  ggplot(aes(x = time , y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Birch fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[4]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Elm fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[5]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Hemlock fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[6]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Maple fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[7]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Oak fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[8]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Other conifer fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[9]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Other hardwood fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[10]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Pine fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[11]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Spruce fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format |>
  filter(taxon == specs[12]) |>
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Tamarack fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

states <- map_data('state') |>
  filter(region %in% c('michigan', 'wisconsin', 'minnesota'))

full_melt |>
  select(time, hemlock, long, lat) |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = if_else(time == '2100 YBP', '2,100 YBP', time),
        time = if_else(time == '2000 YBP', '2,000 YBP', time),
        time = if_else(time == '1900 YBP', '1,900 YBP', time),
        time = if_else(time == '1800 YBP', '1,800 YBP', time),
        time = if_else(time == '1700 YBP', '1,700 YBP', time),
        time = if_else(time == '1600 YBP', '1,600 YBP', time),
        time = if_else(time == '1500 YBP', '1,500 YBP', time),
        time = if_else(time == '1400 YBP', '1,400 YBP', time),
        time = if_else(time == '1300 YBP', '1,300 YBP', time),
        time = if_else(time == '1200 YBP', '1,200 YBP', time),
        time = if_else(time == '1100 YBP', '1,100 YBP', time),
        time = if_else(time == '1000 YBP', '1,000 YBP', time)) |>
  mutate(time = factor(time, levels = c('2,100 YBP', '2,000 YBP', '1,900 YBP', '1,800 YBP', '1,700 YBP', '1,600 YBP',
                                        '1,500 YBP', '1,400 YBP', '1,300 YBP', '1,200 YBP',
                                        '1,100 YBP', '1,000 YBP', '900 YBP', '800 YBP',
                                        '700 YBP', '600 YBP', '500 YBP', '400 YBP',
                                        '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = hemlock)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal') +
  labs(color = 'Proportion\nHemlock') +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# One time period
full_melt |>
  select(time, hemlock, long, lat) |>
  filter(time == 500) |>
  mutate(time = paste0(time, ' YBP')) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = hemlock)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  scale_color_distiller(palette = 'BuGn', direction = 'horizontal') +
  labs(color = 'Proportion\nHemlock') +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

test <- full_melt |>
  select(time, hemlock, long, lat)

sub <- test |>
  filter(time == min(time))

test_out <- matrix(, nrow = nrow(sub) * 19,
                   ncol = 4)
colnames(test_out) <- colnames(test)

times <- unique(test$time)
times <- rev(times)

ind <- 1
for(i in 1:nrow(sub)){
  lon <- sub$long[i]
  lat <- sub$lat[i]
  temp <- test |> filter(long == lon) |> filter(lat == lat)
  for(t in 1:(length(times)-1)){
    present_time <- times[t+1]
    previous_time <- times[t]
    test_out[ind, 1] <- present_time
    test_out[ind, 2] <- temp$hemlock[which(temp$time == present_time)] - temp$hemlock[which(temp$time == previous_time)]
    test_out[ind, 3] <- lon
    test_out[ind, 4] <- lat
    ind <- ind + 1
  }
  print(i/nrow(sub))
}

test_out <- as.data.frame(test_out)

test_out |>
  mutate(time = paste0(time, ' YBP')) |>
  mutate(time = if_else(time == '2100 YBP', '2,100 YBP', time),
         time = if_else(time == '2000 YBP', '2,000 YBP', time),
         time = if_else(time == '1900 YBP', '1,900 YBP', time),
         time = if_else(time == '1800 YBP', '1,800 YBP', time),
         time = if_else(time == '1700 YBP', '1,700 YBP', time),
         time = if_else(time == '1600 YBP', '1,600 YBP', time),
         time = if_else(time == '1500 YBP', '1,500 YBP', time),
         time = if_else(time == '1400 YBP', '1,400 YBP', time),
         time = if_else(time == '1300 YBP', '1,300 YBP', time),
         time = if_else(time == '1200 YBP', '1,200 YBP', time),
         time = if_else(time == '1100 YBP', '1,100 YBP', time),
         time = if_else(time == '1000 YBP', '1,000 YBP', time)) |>
  mutate(time = factor(time, levels = c('2,100 YBP', '2,000 YBP', '1,900 YBP', '1,800 YBP', '1,700 YBP', '1,600 YBP',
                                        '1,500 YBP', '1,400 YBP', '1,300 YBP', '1,200 YBP',
                                        '1,100 YBP', '1,000 YBP', '900 YBP', '800 YBP',
                                        '700 YBP', '600 YBP', '500 YBP', '400 YBP',
                                        '300 YBP', '200 YBP'))) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = hemlock)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~time) +
  theme_void() +
  labs(color = 'Difference') +
  scale_color_gradientn(colors = c('#ef8a62', '#ffffff', '#67a9cf'),
                        values = scales::rescale(c(-0.4, 0, 0.04))) +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
