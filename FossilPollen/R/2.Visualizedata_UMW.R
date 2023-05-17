## Plotting fractional composition over time
## Upper Midwest

rm(list = ls())

library(tidyverse)

# Load data
load('FossilPollen/Data/full_melt_UMW.RData')

## format data

full_format <- full_melt %>%
  pivot_longer(cols = c(ash, beech, birch, elm, hemlock, maple, oak, other_conifer,
                        other_hardwood, pine, spruce, tamarack))
#colnames(full_format) <- c('x', 'y', 'time', 'loc', 'loc_time', 'taxon', 'fc')
colnames(full_format) <- c('time', 'long', 'lat', 'loc_time', 'taxon', 'fc')

## 1. stacked bars

# full dataset

full_format %>%
  ggplot(aes(x = time * 100, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition over time in the Upper Midwest') +
  xlab('Time (years before 1950)') +
  ylab('Relative Composition') +
  scale_fill_viridis_d()

# sample locations
full_format <- full_format %>%
  mutate(loc = paste0(long,'_',lat))
locs = unique(full_format$loc)

full_format %>%
  filter(loc == sample(locs, 1)) %>%
  ggplot(aes(x = time * 100, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition at a given location over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_fill_viridis_d()

## 2. Lines of composition per species

specs = unique(full_format$taxon)

full_format %>%
  filter(taxon == specs[1]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Ash fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[2]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Beech fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[3]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Birch fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[4]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Elm fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[5]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Hemlock fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[6]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Maple fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[7]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Oak fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[8]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Other conifer fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[9]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Other hardwood fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[10]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Pine fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[11]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Spruce fractional composition over time') +
  xlab('Time (years before 1950)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[12]) %>%
  ggplot(aes(x = time * 100, y = fc, color = loc)) +
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
  select(time, pine, long, lat) |>
  mutate(time = paste0(time, ' y BP')) |>
  mutate(time = factor(time, levels = c('1900 y BP', '1800 y BP', '1700 y BP', '1600 y BP',
                                        '1500 y BP', '1400 y BP', '1300 y BP', '1200 y BP',
                                        '1100 y BP', '1000 y BP', '900 y BP', '800 y BP',
                                        '700 y BP', '600 y BP', '500 y BP', '400 y BP',
                                        '300 y BP', '200 y BP'))) |>
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = 'white') +
  geom_point(aes(x = long, y = lat, color = pine)) +
  facet_wrap(~time) +
  theme_void() +
  scale_color_viridis_c() +
  labs(color = 'Proportion\nPine')

test <- full_melt |>
  select(time, pine, long, lat)

sub <- test |>
  filter(time == min(time))

test_out <- matrix(, nrow = nrow(sub) * 17,
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
    test_out[ind, 2] <- temp$pine[which(temp$time == present_time)] - temp$pine[which(temp$time == previous_time)]
    test_out[ind, 3] <- lon
    test_out[ind, 4] <- lat
    ind <- ind + 1
  }
  print(i/nrow(sub))
}

test_out <- as.data.frame(test_out)

test_out |>
  mutate(time = paste0(time, ' y BP')) |>
  mutate(time = factor(time, levels = c('1900 y BP', '1800 y BP', '1700 y BP', '1600 y BP',
                                        '1500 y BP', '1400 y BP', '1300 y BP', '1200 y BP',
                                        '1100 y BP', '1000 y BP', '900 y BP', '800 y BP',
                                        '700 y BP', '600 y BP', '500 y BP', '400 y BP',
                                        '300 y BP', '200 y BP'))) |>
  ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = 'white') +
  geom_point(aes(x = long, y = lat, color = pine)) +
  facet_wrap(~time) +
  theme_void() +
  # scale_color_viridis_c(option = '') +
  labs(color = 'Difference')
