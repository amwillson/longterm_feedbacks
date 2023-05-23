## Matching the entire dataset in space

rm(list = ls())

library(tidyverse)
library(sp)
library(rgeos)

### Load data ###

load('FossilPollen/Data/full_melt_UMW.RData')
load('FossilPollen/Data/sd_UMW.RData')

# For now let's join these to make things easier
pollen_data <- full_melt |>
  full_join(full_sd, by = c('time', 'long', 'lat', 'loc_time'))

load('Climate/processed_climate.RData')

### Start processing climate drivers ###

climate <- clim_av |>
  mutate(Loc = paste0(Latitude,'_',Longitude),
         Loc_Year = paste0(Loc,'_',Year)) |>
  drop_na()

### Match climate and fossil pollen data ####
spat_climate <- climate |>
  filter(Year == 1900)

coordinates(spat_climate) <- ~Longitude+Latitude

spat_pollen <- pollen_data |>
  filter(time == 1900)
coordinates(spat_pollen) <- ~long+lat

d <- gDistance(spgeom1 = spat_pollen, spgeom2 = spat_climate, byid = T)
mins <- apply(d, 2, which.min)

comb <- pollen_data |>
  filter(time == 1900)
comb2 <- climate |>
  filter(Year == 1900)

mins <- as.data.frame(mins)
mins <- cbind(comb, mins)

mins$Longitude <- NA
mins$Latitude <- NA
mins$Temperature <- NA
mins$Precipitation <- NA

for(i in 1:nrow(mins)){
  ind <- mins$mins[i]
  mins$Longitude[i] <- comb2$Longitude[ind]
  mins$Latitude[i] <- comb2$Latitude[ind]
}

for(i in 1:nrow(mins)){
  ind <- mins$mins[i]
  longitude <- mins$Longitude[i]
  latitude <- mins$Latitude[i]
  year <- mins$time[i]
  
  ii <- which(climate$Longitude == longitude &
                climate$Latitude == latitude &
                climate$Year == year)
  
  mins$Temperature[i] <- climate$Temperature[ii]
  mins$Precipitation[i] <- climate$Precipitation[ii]
}

# Check to make sure it looks good
rand <- sample(1:nrow(mins), 1)
sub <- mins |>
  slice(rand)

states <- map_data('state')
states %>%
  filter(region %in% c('illinois', 'indiana', 'michigan', 'minnesota', 'wisconsin')) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_map('albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(data = sub, aes(x = long, y = lat)) +
  geom_point(data = sub, aes(x = Longitude, y = Latitude), color = 'darkred', alpha = 0.5)

cols <- colnames(mins)
ydata_columns <- which(grepl('.x', cols, fixed = T))
edata_columns <- which(grepl('.y', cols, fixed = T))
xdata_columns <- which(cols == 'Temperature' | cols == 'Precipitation')

ydata <- mins |>
  select(all_of(ydata_columns))
edata <- mins |>
  select(all_of(edata_columns))
xdata <- mins |>
  select(all_of(xdata_columns))

colnames(ydata) <- colnames(full_melt)[2:13]
colnames(edata) <- colnames(full_melt)[2:13]

save(xdata, ydata, edata, file = 'FossilPollen/Data/spatial_data.RData')
