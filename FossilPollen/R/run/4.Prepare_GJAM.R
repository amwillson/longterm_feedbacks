## Matching the entire dataset in space

rm(list = ls())

library(tidyverse)
library(sp)
library(rgeos)

#### Load data ####

load('FossilPollen/Data/full_melt_UMW.RData')
load('FossilPollen/Data/sd_UMW.RData')

# For now let's join these to make things easier
pollen_data <- full_melt |>
  full_join(full_sd, by = c('time', 'long', 'lat', 'loc_time'))

load('Climate/processed_climate.RData')

#### Start processing climate drivers ####

climate <- clim_av |>
  mutate(Loc = paste0(Latitude, '_', Longitude),
         Loc_Year = paste0(Loc, '_', Year)) |>
  # Need to drop NA because we don't have climate drivers for some years of interest
  # So we won't model those years
  drop_na()

#### Match climate and fossil pollen data ####

# Take only one year to get unique locations
# All locations have data for all time points

spat_climate <- climate |> filter(Year == 1900)
# make spatial data object
coordinates(spat_climate) <- ~Longitude+Latitude

spat_pollen <- pollen_data |> filter(time == 1900)
coordinates(spat_pollen) <- ~long+lat

# Find distance between all points
d <- gDistance(spgeom1 = spat_pollen, spgeom2 = spat_climate, byid = T)
# Find closest climate data point to each pollen data point
mins <- apply(d, 2, which.min)
# result = 704 points corresponding to each grid cell of our pollen dataset

spat_pollen <- pollen_data |>
  filter(time == 1900) |>
  mutate(loc = paste0(lat,'_',long),
         match = mins)

spat_climate <- climate |>
  filter(Year == 1900)

full_spat_pollen <- pollen_data |>
  mutate(loc = paste0(lat,'_',long))

spat_map <- spat_pollen |>
  select(loc, match)

full_data <- matrix(, nrow = nrow(full_spat_pollen),
                    ncol = 35)

for(i in 1:nrow(full_spat_pollen)){
  loc <- full_spat_pollen$loc[i]
  match <- spat_map$match[which(spat_map$loc == loc)]
  year <- as.numeric(full_spat_pollen[i,1])
  full_data[i,1] <- match # Match between datasets
  full_data[i,2] <- loc # Location
  full_data[i,3] <- year # Year
  full_data[i,4:15] <- as.numeric(full_spat_pollen[i,2:13]) # ydata
  full_data[i,16:17] <- as.numeric(full_spat_pollen[i,14:15]) # Lat/lon of pollen
  full_data[i,18:29] <- as.numeric(full_spat_pollen[i,17:28]) # edata
  clim_loc <- spat_climate$Loc[match]
  temp <- climate |> filter(Loc == clim_loc) |> filter(Year == year)
  full_data[i,30] <- temp$Latitude # Lat of climate
  full_data[i,31] <- temp$Longitude # lon of climate
  full_data[i,32] <- temp$Mean_Temperature # temperature
  full_data[i,33] <- temp$Mean_Precipitation # precipitation
  full_data[i,34] <- temp$SD_Temperature # temperature variability
  full_data[i,35] <- temp$SD_Precipitation # precipitation variability
}

full_data <- as.data.frame(full_data)
colnames(full_data) <- c('Match', 'Location', 'Year', colnames(full_spat_pollen)[2:13],
                         'Pollen_Longitude', 'Pollen_Latitude', colnames(full_spat_pollen)[17:28],
                         'Climate_Latitude', 'Climate_Longitude', 'MeanTemperature', 'MeanPrecipitation',
                         'SDTemperature', 'SDPrecipitation')

full_data <- full_data |>
  mutate(Year = as.numeric(Year),
         ash.x = as.numeric(ash.x),
         beech.x = as.numeric(beech.x),
         birch.x = as.numeric(birch.x),
         elm.x = as.numeric(elm.x),
         hemlock.x = as.numeric(hemlock.x),
         maple.x = as.numeric(maple.x),
         oak.x = as.numeric(oak.x),
         other_conifer.x = as.numeric(other_conifer.x),
         other_hardwood.x = as.numeric(other_hardwood.x),
         pine.x = as.numeric(pine.x),
         spruce.x = as.numeric(spruce.x),
         tamarack.x = as.numeric(tamarack.x),
         Pollen_Longitude = as.numeric(Pollen_Longitude),
         Pollen_Latitude = as.numeric(Pollen_Latitude),
         ash.y = as.numeric(ash.y),
         beech.y = as.numeric(beech.y),
         birch.y = as.numeric(birch.y),
         elm.y = as.numeric(elm.y),
         hemlock.y = as.numeric(hemlock.y),
         maple.y = as.numeric(maple.y),
         oak.y = as.numeric(oak.y),
         other_conifer.y = as.numeric(other_conifer.y),
         other_hardwood.y = as.numeric(other_hardwood.y),
         pine.y = as.numeric(pine.y),
         tamarack.y = as.numeric(tamarack.y),
         Climate_Latitude = as.numeric(Climate_Latitude),
         Climate_Longitude = as.numeric(Climate_Longitude),
         MeanTemperature = as.numeric(MeanTemperature),
         MeanPrecipitation = as.numeric(MeanPrecipitation),
         SDTemperature = as.numeric(SDTemperature),
         SDPrecipitation = as.numeric(SDPrecipitation))

rand <- sample(1:nrow(full_data), 1)
sub <- full_data |>
  slice(rand)
states <- map_data('state')

states |>
  filter(region %in% c('michigan', 'wisconsin', 'minnesota')) |>
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), color = 'black', fill = 'white') +
  coord_map('albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(data = sub, aes(x = Pollen_Longitude, y = Pollen_Latitude)) +
  geom_point(data = sub, aes(x = Climate_Longitude, y = Climate_Latitude))

rand <- sample(1:length(unique(full_data$Location)), size = 1)

full_data |>
  filter(Location == unique(Location)[rand]) |>
  ggplot() +
  geom_point(aes(x = Year, y = MeanPrecipitation))

cols <- colnames(full_data)
ydata_columns <- which(grepl('.x', cols, fixed = T))
edata_columns <- which(grepl('.y', cols, fixed = T))
xdata_columns <- which(cols == 'MeanTemperature' | cols == 'MeanPrecipitation' |
                         cols == 'SDTemperature' | cols == 'SDPrecipitation')

ydata <- full_data |>
  select(all_of(ydata_columns))
edata <- full_data |>
  select(all_of(edata_columns))
xdata <- full_data |>
  select(all_of(xdata_columns))

colnames(ydata) <- colnames(full_melt)[2:13]
colnames(edata) <- colnames(full_melt)[2:13]

ydata <- ydata |> rename(conifer = other_conifer,
                         hardwood = other_hardwood)
edata <- edata |> rename(conifer = other_conifer,
                         hardwood = other_hardwood)

## Separating full data into ones for different time periods
periods <- unique(full_data$Year)

# Take periods 500 years apart
periods <- seq(from = periods[1], to = periods[length(periods)], by = 500)
# Add in most modern period (more rapid change = more change in vegetation-environment relationship?)
periods <- c(periods, 1900)

# Make separate dataframes for each period
xdata_1 <- full_data |>
  filter(Year == periods[1]) |>
  select(all_of(xdata_columns), Year)
ydata_1 <- full_data |>
  filter(Year == periods[1]) |>
  select(all_of(ydata_columns))
edata_1 <- full_data |>
  filter(Year == periods[1]) |>
  select(all_of(edata_columns))
colnames(ydata_1) <- colnames(edata_1) <- colnames(full_melt)[2:13]
ydata_1 <- ydata_1 |> rename(conifer = other_conifer,
                         hardwood = other_hardwood)
edata_1 <- edata_1 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)

xdata_2 <- full_data |>
  filter(Year == periods[2]) |>
  select(all_of(xdata_columns), Year)
ydata_2 <- full_data |>
  filter(Year == periods[2]) |>
  select(all_of(ydata_columns))
edata_2 <- full_data |>
  filter(Year == periods[2]) |>
  select(all_of(edata_columns))
colnames(ydata_2) <- colnames(edata_2) <- colnames(full_melt)[2:13]
ydata_2 <- ydata_2 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)
edata_2 <- edata_2 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)

xdata_3 <- full_data |>
  filter(Year == periods[3]) |>
  select(all_of(xdata_columns), Year)
ydata_3 <- full_data |>
  filter(Year == periods[3]) |>
  select(all_of(ydata_columns))
edata_3 <- full_data |>
  filter(Year == periods[3]) |>
  select(all_of(edata_columns))
colnames(ydata_3) <- colnames(edata_3) <- colnames(full_melt)[2:13]
ydata_3 <- ydata_3 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)
edata_3 <- edata_3 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)

xdata_4 <- full_data |>
  filter(Year == periods[4]) |>
  select(all_of(xdata_columns), Year)
ydata_4 <- full_data |>
  filter(Year == periods[4]) |>
  select(all_of(ydata_columns))
edata_4 <- full_data |>
  filter(Year == periods[4]) |>
  select(all_of(edata_columns))
colnames(ydata_4) <- colnames(edata_4) <- colnames(full_melt)[2:13]
ydata_4 <- ydata_4 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)
edata_4 <- edata_4 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)

xdata_5 <- full_data |>
  filter(Year == periods[5]) |>
  select(all_of(xdata_columns), Year)
ydata_5 <- full_data |>
  filter(Year == periods[5]) |>
  select(all_of(ydata_columns))
edata_5 <- full_data |>
  filter(Year == periods[5]) |>
  select(all_of(edata_columns))
colnames(ydata_5) <- colnames(edata_5) <- colnames(full_melt)[2:13]
ydata_5 <- ydata_5 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)
edata_5 <- edata_5 |> rename(conifer = other_conifer,
                             hardwood = other_hardwood)

save(xdata, ydata, edata, file = 'FossilPollen/Data/gjam_data_allyears.RData')
save(xdata_1, xdata_2, xdata_3, xdata_4, xdata_5,
     ydata_1, ydata_2, ydata_3, ydata_4, ydata_5,
     edata_1, edata_2, edata_3, edata_4, edata_5,
     file = 'FossilPollen/Data/gjam_data_indyears.RData')
