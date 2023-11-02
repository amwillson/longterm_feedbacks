## Specific locations of cultural interest for Jason

rm(list = ls())

library(tidyverse)
library(fields)

# Load data
load('FossilPollen/Data/full_melt_UMW.RData')

# Make table of locations of interest
place <- matrix(, nrow = 5, ncol = 3)
place <- as.data.frame(place)
colnames(place) <- c('Name', 'Lat', 'Lon')
place$Name <- c('Red Lake Reservation',
                'Leech Lake Reservation',
                'White Earth Reservation',
                'Menominee Reservation',
                'Mille Lacs Reservation')
place$Lat <- c(48.0512, 47.3333, 47.23333,
               45.0025, 46.1072)
place$Lon <- c(-95.0010, -94.2169, -95.7670,
               -88.6668, -93.7161)

# Make table of unique locations in stepps
locs <- full_melt |>
  filter(time == min(time)) |>
  select(lat, long)

# Find the closest data points between the two tables
matched_locs <- matrix(, nrow = length(unique(full_melt$time)),
                       ncol = 5)

for(i in 1:length(unique(full_melt$time))){
  locs <- full_melt |>
    filter(time == unique(full_melt$time)[i]) |>
    select(lat, long)
  
  dists <- rdist(select(place, Lat, Lon),
                 select(locs, lat, long))
  
  closest_point <- apply(dists, 1, which.min)
  
  matched <- full_melt |>
    filter(time == unique(full_melt$time)[i]) |>
    slice(closest_point) |>
    select(loc_time)
  
  matched <- as.vector(matched$loc_time)
  matched_locs[i,] <- matched
}

colnames(matched_locs) <- place$Name

place <- cbind(place, t(matched_locs))
colnames(place) <- c('Name', 'Lat', 'Lon', 'T1', 'T2', 'T3',
                     'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10',
                     'T11', 'T12', 'T13', 'T14', 'T15', 'T16', 'T17',
                     'T18', 'T19', 'T20')
place <- place |> pivot_longer(names_to = 'Time', values_to = 'loc_time', cols = T1:T20)

# Format stepps data
full_format <- full_melt |>
  filter(loc_time %in% matched_locs) |>
  pivot_longer(cols = c(ash, beech, birch, elm, hemlock, maple, oak, other_conifer,
                        other_hardwood, pine, spruce, tamarack)) |>
  full_join(place, by = 'loc_time') |>
  select(time, long, lat, loc_time, name, value, Name)
colnames(full_format) <- c('time', 'long', 'lat', 'loc_time', 
                           'taxon', 'fc', 'Reservation')

# Plot
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
  filter(Reservation == 'Red Lake Reservation') |>
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Red Lake Reservation') +
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
  filter(Reservation == 'Leech Lake Reservation') |>
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Leech Lake Reservation') +
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
  filter(Reservation == 'White Earth Reservation') |>
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('White Earth Reservation') +
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
  filter(Reservation == 'Menominee Reservation') |>
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Menominee Reservation') +
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
  filter(Reservation == 'Mille Lacs Reservation') |>
  ggplot(aes(x = time, y = fc, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Mille Lacs Reservation') +
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

## Now load SD to plot with uncertainty
load('FossilPollen/Data/full_melt_UMW_sd.RData')

full_format_sd <- full_melt_sd |>
  mutate(time = time * 0.01) |>
  filter(loc_time %in% matched_locs) |>
  pivot_longer(cols = c(ash, beech, birch, elm, hemlock, maple, oak, other_conifer,
                        other_hardwood, pine, spruce, tamarack)) |>
  full_join(place, by = 'loc_time') |>
  select(time, long, lat, loc_time, name, value, Name)
colnames(full_format_sd) <- c('time', 'long', 'lat', 'loc_time', 
                           'taxon', 'sd', 'Reservation')

# Combine with means
full_format_tot <- full_format |>
  full_join(full_format_sd, by = c('time', 'long', 'lat',
                                   'loc_time', 'taxon', 'Reservation'))

full_format_sd |>
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
  filter(Reservation == 'Red Lake Reservation') |>
  ggplot(aes(x = time, y = sd, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Red Lake Reservation') +
  xlab('Time (years before present)') +
  ylab('Standard Deviation (fraction)') +
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

full_format_sd |>
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
  filter(Reservation == 'Leech Lake Reservation') |>
  ggplot(aes(x = time, y = sd, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Leech Lake Reservation') +
  xlab('Time (years before present)') +
  ylab('Standard Deviation (fraction)') +
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

full_format_sd |>
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
  filter(Reservation == 'White Earth Reservation') |>
  ggplot(aes(x = time, y = sd, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('White Earth Reservation') +
  xlab('Time (years before present)') +
  ylab('Standard Deviation (fraction)') +
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

full_format_sd |>
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
  filter(Reservation == 'Menominee Reservation') |>
  ggplot(aes(x = time, y = sd, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Menominee Reservation') +
  xlab('Time (years before present)') +
  ylab('Standard Deviation (fraction)') +
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

full_format_sd |>
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
  filter(Reservation == 'Mille Lacs Reservation') |>
  ggplot(aes(x = time, y = sd, fill = taxon)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_x_reverse() +
  ggtitle('Mille Lacs Reservation') +
  xlab('Time (years before present)') +
  ylab('Standard Deviation (fraction)') +
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
