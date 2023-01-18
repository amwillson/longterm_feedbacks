rm(list = ls())

library(ncdf4)
library(lubridate)
library(reshape2)

# Make list of files to read in
clim_pr_files <- list.files('Climate/Precipitation/')
clim_at_files <- list.files('Climate/SurfaceAT/')

# Storage
clim_pr <- list()
clim_at <- list()

# Read in files and store them in lists
for(i in 1:length(clim_at_files)){
  pr_file <- paste0('Climate/Precipitation/',clim_pr_files[i])
  clim_pr[[i]] <- nc_open(pr_file)
  
  at_file <- paste0('Climate/SurfaceAT/',clim_at_files[i])
  clim_at[[i]] <- nc_open(at_file)
}

# Storage
arr_pr <- list()
arr_at <- list()

# Reformatting data into more manageable arrays
for(i in 1:length(clim_at_files)){
  temp <- clim_pr[[i]]
  
  temp2 <- ncvar_get(temp, 'pr')
  
  lons <- ncvar_get(temp, 'lon')
  lats <- ncvar_get(temp, 'lat')
  times <- ncvar_get(temp, 'time')
  
  dimnames(temp2) <- list(lons, lats, times)
  
  arr_pr[[i]] <- temp2
  
  temp <- clim_at[[i]]
  
  temp2 <- ncvar_get(temp, 'tas')
  
  lons <- ncvar_get(temp, 'lon')
  lats <- ncvar_get(temp, 'lat')
  times <- ncvar_get(temp, 'time')
  
  dimnames(temp2) <- list(lons, lats, times)
  
  arr_at[[i]] <- temp2
  
  print(i)
}

# Removing unnecessary objects to clear up storage
rm(clim_at, clim_pr)


# Take our area of interest in the midwest
for(i in 1:length(clim_at_files)){
  temp <- arr_at[[i]]
  temp <- temp[140:150,70:76,]
  arr_at[[i]] <- temp
  
  temp <- arr_pr[[i]]
  temp <- temp[140:150,70:76,]
  arr_pr[[i]] <- temp
}

# Reformatting
melt_at = melt(arr_at)
melt_pr = melt(arr_pr)

# Remove unnecessary objects again
rm(arr_at, arr_pr)

# Join the temperature and precipitation data together
clim <- melt_at %>%
  full_join(melt_pr, by = c('Var1', 'Var2', 'Var3', 'L1'))

# Remove unnecessary objects one more time
rm(melt_at, melt_pr)

# Formatting
colnames(clim) <- c('Longitude', 'Latitude', 'Time', 'Temperature', 'L1', 'Precipitation')

clim <- clim %>%
  select(-L1)

# Formatting specific columns
clim <- clim %>%
  mutate(Longitude = if_else(Longitude > 180, -360 + Longitude, Longitude), # Make longitude between -180 and 180 degrees
         Time = as.Date(Time, origin = c('1850-01-01')),# Reformat time as the date
         Month = month(Time),
         Year = 1950 - year(ymd(Time) - years(7000)), # The dates are wrong so subtract 7000 years and convert to years BP
         Temperature = Temperature - 273.15, # Convert Temperature to Celsius
         Precipitation = Precipitation * 24 * 60 * 60 * 30) # Convert Precipitation to mm/month

## Now we need to average to the same temporal resolution as the composition reconstructions

# First, find the timesteps from reconstructions
fc <- nc_open('FossilPollen/Data/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc')
fc <- ncvar_get(fc, 'Time')
fc <- fc * 100
bins <- fc

# Simply take climate for years of interest
clim1 <- clim |>
  filter(Year %in% bins)

## Eventually we should average over all years in a range

# Save!
save(clim1, file = 'Climate/processed_climate.RData')
