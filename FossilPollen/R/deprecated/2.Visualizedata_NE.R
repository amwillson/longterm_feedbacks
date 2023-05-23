## Plotting fractional composition over time
## Northeast

rm(list = ls())

library(tidyverse)

# Load data
load('FossilPollen/Data/full_melt_NE.RData')

## format data

full_format <- full_melt %>%
  pivot_longer(cols = c(ash, beech, birch, chestnut, hemlock, hickory, maple, oak, other_conifer,
                        other_hardwood, pine, spruce, tamarack))
colnames(full_format) <- c('x', 'y', 'time', 'loc', 'loc_time', 'taxon', 'fc')

## 1. stacked bars

# full dataset

full_format %>%
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition over time in the Northeast') +
  xlab('Time (years BP)') +
  ylab('Relative Composition') +
  scale_fill_viridis_d()

# sample locations
locs = unique(full_format$loc)

full_format %>%
  filter(loc == sample(locs, 1)) %>%
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Fractional composition at a given location over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_fill_viridis_d()

## 2. Lines of composition per species

specs = unique(full_format$taxon)

full_format %>%
  filter(taxon == specs[1]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Ash fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[2]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Beech fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[3]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Birch fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[4]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Chestnut fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[5]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Hemlock fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[6]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Hickory fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[7]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Maple fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[8]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Oak fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[9]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Other conifers fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[10]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Other hardwoods fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[11]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Pine fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[12]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Spruce fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

full_format %>%
  filter(taxon == specs[13]) %>%
  ggplot(aes(x = time, y = fc, color = loc)) +
  geom_line() +
  scale_x_reverse() +
  theme(legend.position = 'none') +
  ggtitle('Tamarack fractional composition over time') +
  xlab('Time (years BP)') +
  ylab('Fractional Composition') +
  scale_color_viridis_d() +
  ylim(0, 1)

