## This script combines the climate drivers and fractional composition data for one area
## To be used in GJAM and GJAM Time

rm(list = ls())

library(tidyverse)
library(sp)
library(rgeos)

#### Fractional Composition Data ####

# Load data from 1.Format_UMW.R
load('FossilPollen/Data/full_melt_UMW.RData')
load('FossilPollen/Data/sd_UMW.RData')

# Make data frame into a spatial data frame
sp.mydata <- full_melt
coordinates(sp.mydata) <- ~long+lat
class(sp.mydata)

sp.sd <- full_sd
coordinates(sp.sd) <- ~long+lat

# Make dataframe of single point of interest (= location of Hemlock Mountain, Wisconsin)
point <- c(45.3333, -90.08333)
point <- as.data.frame(t(point))
colnames(point) <- c('lat', 'long')
coordinates(point) <- ~long+lat

# Find distance between Hemlock Mountain and each location in our dataset
d <- gDistance(spgeom1 = sp.mydata, spgeom2 = point, byid = T)
d.sd <- gDistance(spgeom1 = sp.sd, spgeom2 = point, byid = T)

# Find the closest point
mind <- which.min(d)
mind.sd <- which.min(d.sd)

# Add a "location" column to the data
full_melt <- full_melt |>
  mutate(time = time * 100) |>
  mutate(loc = paste0(lat,'_',long)) |>
  mutate(loctime = paste0(loc,'_',time))
full_sd <- full_sd |>
  mutate(time = time * 100) |>
  mutate(loc = paste0(lat,'_',long)) |>
  mutate(loctime = paste0(loc,'_',time))

# Find the location that was identified as the closest
loc <- full_melt$loc[mind]
loc.sd <- full_sd$loc[mind.sd]

# Find all the instances of that location (should be one for each time step)
ids <- which(full_melt$loc == loc)
ids.sd <- which(full_sd$loc == loc.sd)

# Subset for the one location of interest
full_sub <- full_melt |>
  slice(ids)
sd_sub <- full_sd |>
  slice(ids)

# Prepare ydata for GJAM
ydata <- full_sub |>
  column_to_rownames('loctime') |>
  select(-c(time, long, lat, loc, loc_time)) |>
  rename(other.conifer = other_conifer) |>
  rename(other.hardwood = other_hardwood)

save(ydata, file = 'FossilPollen/Data/sylvania_ydata.RData')

# Prepare effort for GJAM
effort <- sd_sub |>
  column_to_rownames('loctime') |>
  select(-c(time, long, lat, loc, loc_time))

# Convert to variance
effort <- effort^2
# Convert to precision
effort <- 1/effort

effort <- effort |>
  rename(other.conifer = other_conifer) |>
  rename(other.hardwood = other_hardwood)

# Format for GJAM
elist <- list(columns = 1:ncol(ydata),
              values = effort)

save(elist, file = 'FossilPollen/Data/sylvania_edata.RData')

#### Climate Drivers ####

load('Climate/processed_climate.RData')

# Make data frame into a spatial data frame
sp.clim <- clim1
coordinates(sp.clim) <- ~Longitude+Latitude

# Find distance between Hemlock Mountain and each location in our dataset
d.clim <- gDistance(spgeom1 = sp.clim, spgeom2 = point, byid = T)

# Find the closest point
mind.clim <- which.min(d.clim)

# Add a "location" column to the data
clim1 <- clim1 |>
  mutate(loc = paste0(Latitude,'_',Longitude))

# Find the location that was identified as the closest
loc.clim <- clim1$loc[mind.clim]

# Find all the instances of that location (should be one for each time step)
ids.clim <- which(clim1$loc == loc.clim)

# Subset for the one location of interest
clim_sub <- clim1 |>
  slice(ids.clim)

# Average temperature over months
times <- unique(clim_sub$Time)
for(i in 1:length(times)){
  temp <- clim_sub |>
    filter(Time == times[i])
  if(length(unique(temp$Longitude)) == 1 &
     length(unique(temp$Latitude)) == 1 &
     length(unique(temp$Time)) == 1){
    av_temp <- mean(temp$Temperature)
    tot_precip <- sum(temp$Precipitation)
    temp <- temp[1,]
    temp$Temperature <- av_temp
    temp$Precipitation <- tot_precip
  }else{print('Error')}
  if(i == 1){clim = temp}else{clim = rbind(clim, temp)}
}

# Prepare for GJAM
xdata <- clim |>
  column_to_rownames('loc_time') |>
  select(-c(Longitude, Latitude, Time, loc))

# Save
save(xdata, file = 'Climate/xdata_sylvania.RData')
