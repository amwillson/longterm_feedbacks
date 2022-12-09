## Working with Fossil Pollen data
## From Upper Midwest

library(ncdf4)
library(reshape2)

rm(list = ls())

# Open data
fc <- nc_open('FossilPollen/Data/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc')

# Get lon, lat, time
# x and y are flipped to get the right easting and northing directions
y <- ncvar_get(fc, 'x')
x <- ncvar_get(fc, 'y')
time <- ncvar_get(fc, 'Time')

# Fractional composition
ash <- ncvar_get(fc, 'ASH')
beech <- ncvar_get(fc, 'BEECH')
birch <- ncvar_get(fc, 'BIRCH')
elm <- ncvar_get(fc, 'ELM')
hemlock <- ncvar_get(fc, 'HEMLOCK')
maple <- ncvar_get(fc, 'MAPLE')
oak <- ncvar_get(fc, 'OAK')
other_conifer <- ncvar_get(fc, 'OTHER.CONIFER')
other_hardwood <- ncvar_get(fc, 'OTHER.HARDWOOD')
pine <- ncvar_get(fc, 'PINE')
spruce <- ncvar_get(fc, 'SPRUCE')
tamarack <- ncvar_get(fc, 'TAMARACK')

# Replace dimension names
dimnames(ash) <- list(y, x, time)
dimnames(beech) <- list(y, x, time)
dimnames(birch) <- list(y, x, time)
dimnames(elm) <- list(y, x, time)
dimnames(hemlock) <- list(y, x, time)
dimnames(maple) <- list(y, x, time)
dimnames(oak) <- list(y, x, time)
dimnames(other_conifer) <- list(y, x, time)
dimnames(other_hardwood) <- list(y, x, time)
dimnames(pine) <- list(y, x, time)
dimnames(spruce) <- list(y, x, time)
dimnames(tamarack) <- list(y, x, time)

# Reformat
ash_melt <- melt(ash)
colnames(ash_melt) <- c('y', 'x', 'time', 'ash')

beech_melt <- melt(beech)
colnames(beech_melt) <- c('y', 'x', 'time', 'beech')

birch_melt <- melt(birch)
colnames(birch_melt) <- c('y', 'x', 'time', 'birch')

elm_melt <- melt(elm)
colnames(elm_melt) <- c('y', 'x', 'time', 'elm')

hemlock_melt <- melt(hemlock)
colnames(hemlock_melt) <- c('y', 'x', 'time', 'hemlock')

maple_melt <- melt(maple)
colnames(maple_melt) <- c('y', 'x', 'time', 'maple')

oak_melt <- melt(oak)
colnames(oak_melt) <- c('y', 'x', 'time', 'oak')

other_conifer_melt <- melt(other_conifer)
colnames(other_conifer_melt) <- c('y', 'x', 'time', 'other_conifer')

other_hardwood_melt <- melt(other_hardwood)
colnames(other_hardwood_melt) <- c('y', 'x', 'time', 'other_hardwood')

pine_melt <- melt(pine)
colnames(pine_melt) <- c('y', 'x', 'time', 'pine')

spruce_melt <- melt(spruce)
colnames(spruce_melt) <- c('y', 'x', 'time', 'spruce')

tamarack_melt <- melt(tamarack)
colnames(tamarack_melt) <- c('y', 'x', 'time', 'tamarack')

# Combine data frames
full_melt <- ash_melt %>%
  full_join(beech_melt, by = c('x', 'y', 'time')) %>%
  full_join(birch_melt, by = c('x', 'y', 'time')) %>%
  full_join(elm_melt, by = c('x', 'y', 'time')) %>%
  full_join(hemlock_melt, by = c('x', 'y', 'time')) %>%
  full_join(maple_melt, by = c('x', 'y', 'time')) %>%
  full_join(oak_melt, by = c('x', 'y', 'time')) %>%
  full_join(other_conifer_melt, by = c('x', 'y', 'time')) %>%
  full_join(other_hardwood_melt, by = c('x', 'y', 'time')) %>%
  full_join(pine_melt, by = c('x', 'y', 'time')) %>%
  full_join(spruce_melt, by = c('x', 'y', 'time')) %>%
  full_join(tamarack_melt, by = c('x', 'y', 'time'))

# Combine x & y into one "location" variable
#full_melt <- full_melt %>%
#  mutate(loc = paste0(x,'_',y))

## Convert coordinates to lat, long

# Make a dataframe of just the coordinates
space <- full_melt %>%
  dplyr::select(x,y)

space <- as.data.frame(space)

# Add coordinate info to x, y dataframe
coordinates(space) <- ~y + x

# Add projection. This is the projection according to the original netcdf file
proj4string(space) <- CRS('+init=epsg:3175')

# Reproject using wgs 84
space_latlong <- spTransform(space, CRS('+init=epsg:4326'))
# This gives the same result
#space_latlong <- spTransform(space, CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

# Add lat & long to full dataframe
full_melt <- full_melt %>%
  dplyr::select(-c(x,y)) %>%
  mutate(long = space_latlong$y, lat = space_latlong$x)

# Check to make sure the points match up with our expectation
states <- map_data('state')
states %>%
  filter(region %in% c('illinois', 'indiana', 'michigan', 'minnesota', 'wisconsin')) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_map('albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(data = full_melt, aes(x = long, y = lat, color = hemlock), alpha = 0.05)

# Add in time for unique time/lat/long combinations
full_melt <- full_melt %>%
  mutate(loc_time = paste0(lat,'_',long,'_',time))

# Remove empty grid cells
for_removal <- c()

for(i in 1:nrow(full_melt)){
  if(is.na(full_melt$ash[i]) & is.na(full_melt$beech[i]) & is.na(full_melt$birch[i]) &
     is.na(full_melt$elm[i]) & is.na(full_melt$hemlock[i]) & is.na(full_melt$maple[i]) &
     is.na(full_melt$oak[i]) & is.na(full_melt$other_conifer[i]) & is.na(full_melt$other_hardwood[i]) &
     is.na(full_melt$pine[i]) & is.na(full_melt$spruce[i]) & is.na(full_melt$tamarack[i])){
    for_removal <- c(for_removal, i)
  }
}

full_melt_rm <- full_melt[-for_removal,]
full_melt <- full_melt_rm

# Check the final extent of the data
states %>%
  filter(region %in% c('illinois', 'indiana', 'michigan', 'minnesota', 'wisconsin')) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_map('albers', lat0 = 45.5, lat1 = 29.5) +
  geom_point(data = full_melt, aes(x = long, y = lat, color = hemlock), alpha = 0.05)

save(full_melt, file = 'FossilPollen/Data/full_melt_UMW.RData')
