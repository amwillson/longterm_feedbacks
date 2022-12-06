## Working with Fossil Pollen data
## From Upper Midwest

library(ncdf4)
library(reshape2)

rm(list = ls())

# Open data
fc <- nc_open('FossilPollen/Data/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc')

# Get lon, lat, time
x <- ncvar_get(fc, 'x')
y <- ncvar_get(fc, 'y')
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
dimnames(ash) <- list(x, y, time)
dimnames(beech) <- list(x, y, time)
dimnames(birch) <- list(x, y, time)
dimnames(elm) <- list(x, y, time)
dimnames(hemlock) <- list(x, y, time)
dimnames(maple) <- list(x, y, time)
dimnames(oak) <- list(x, y, time)
dimnames(other_conifer) <- list(x, y, time)
dimnames(other_hardwood) <- list(x, y, time)
dimnames(pine) <- list(x, y, time)
dimnames(spruce) <- list(x, y, time)
dimnames(tamarack) <- list(x, y, time)

# Reformat
ash_melt <- melt(ash)
colnames(ash_melt) <- c('x', 'y', 'time', 'ash')

beech_melt <- melt(beech)
colnames(beech_melt) <- c('x', 'y', 'time', 'beech')

birch_melt <- melt(birch)
colnames(birch_melt) <- c('x', 'y', 'time', 'birch')

elm_melt <- melt(elm)
colnames(elm_melt) <- c('x', 'y', 'time', 'elm')

hemlock_melt <- melt(hemlock)
colnames(hemlock_melt) <- c('x', 'y', 'time', 'hemlock')

maple_melt <- melt(maple)
colnames(maple_melt) <- c('x', 'y', 'time', 'maple')

oak_melt <- melt(oak)
colnames(oak_melt) <- c('x', 'y', 'time', 'oak')

other_conifer_melt <- melt(other_conifer)
colnames(other_conifer_melt) <- c('x', 'y', 'time', 'other_conifer')

other_hardwood_melt <- melt(other_hardwood)
colnames(other_hardwood_melt) <- c('x', 'y', 'time', 'other_hardwood')

pine_melt <- melt(pine)
colnames(pine_melt) <- c('x', 'y', 'time', 'pine')

spruce_melt <- melt(spruce)
colnames(spruce_melt) <- c('x', 'y', 'time', 'spruce')

tamarack_melt <- melt(tamarack)
colnames(tamarack_melt) <- c('x', 'y', 'time', 'tamarack')

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
full_melt <- full_melt %>%
  mutate(loc = paste0(x,'_',y))

# Add in time for unique time/x/y combinations
full_melt <- full_melt %>%
  mutate(loc_time = paste0(x,'_',y,'_',time))

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

save(full_melt, file = 'FossilPollen/Data/full_melt_UMW.RData')
