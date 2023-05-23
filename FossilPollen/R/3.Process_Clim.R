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

# Finding the boundaries of our study region
test <- arr_at[[1]]

# lon, lat, time, temp
test2 <- melt(test)
colnames(test2) <- c('LON', 'LAT', 'TIME', 'TEMP')

test2 <- test2 |>
  mutate(LON = if_else(LON > 180, -360 + LON, LON))

load('FossilPollen/Data/full_melt_UMW.RData')

lons <- unique(full_melt$long)
lats <- unique(full_melt$lat)
min_lon <- min(lons) - 1
max_lon <- max(lons) + 1
min_lat <- min(lats) - 1
max_lat <- max(lats) + 1

test3 <- test2 |>
  filter(LON >= min_lon & LON <= max_lon) |>
  filter(LAT >= min_lat & LAT <= max_lat)

lons <- unique(test3$LON)
lats <- unique(test3$LAT)

dim1 <- dimnames(test)[1]
dim1 <- as.numeric(unlist(dim1))
dim1 <- if_else(dim1 > 180, -360 + dim1, dim1)
ind1 <- which(dim1 %in% lons)

dim2 <- dimnames(test)[2]
dim2 <- as.numeric(unlist(dim2))
ind2 <- which(dim2 %in% lats)

# Take our area of interest in the midwest
for(i in 1:length(clim_at_files)){
  temp <- arr_at[[i]]
  temp <- temp[ind1,ind2,]
  arr_at[[i]] <- temp
  
  temp <- arr_pr[[i]]
  temp <- temp[ind1,ind2,]
  arr_pr[[i]] <- temp
}

# Reformatting
melt_at = melt(arr_at)
melt_pr = melt(arr_pr)

# Remove unnecessary objects again
rm(arr_at, arr_pr)

# Join the temperature and precipitation data together
clim <- melt_at |>
  full_join(melt_pr, by = c('Var1', 'Var2', 'Var3', 'L1'))

# Remove unnecessary objects one more time
rm(melt_at, melt_pr)

# Formatting
colnames(clim) <- c('Longitude', 'Latitude', 'Time', 'Temperature', 'L1', 'Precipitation')

clim <- clim |>
  select(-L1)

# Formatting specific columns
clim <- clim |>
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

# Make bins for +/- 25 years around each year with fractional composition
bins <- matrix(, nrow = length(fc), ncol = 2)
for(i in 1:nrow(bins)){
  bins[i,] <- c(fc[i]-25, fc[i] + 25)
}
colnames(bins) <- c('min', 'max')
bins <- as.data.frame(bins)

# Subset for the first time period to find unique lat/lon pairs
temp <- clim |>
  filter(Time == max(Time))
pairs <- cbind(temp$Latitude, temp$Longitude)
colnames(pairs) <- c('Latitude', 'Longitude')
pairs <- as.data.frame(pairs)

# Storage
clim_av <- matrix(, nrow = nrow(pairs) * nrow(bins), ncol = 5)

# Counter for indexing
ind <- 1
# Loop through each location (lat/lon pair)
for(i in 1:nrow(pairs)){
  # Filter for specific location
  temp <- clim |>
    filter(Latitude == pairs$Latitude[i]) |>
    filter(Longitude == pairs$Longitude[i])
  
  # Loop through each time bin
  for(j in 1:nrow(bins)){
    # Filter for years within time bin
    time_sub <- temp |>
      filter(Year > bins$min[j] & Year <= bins$max[j])
    
    # Save lat/lon/year
    clim_av[ind,1] <- pairs$Latitude[i]
    clim_av[ind,2] <- pairs$Longitude[i]
    clim_av[ind,3] <- fc[j]
    
    # Find average of two climate variables
    clim_av[ind,4] <- mean(time_sub$Temperature, na.rm = T)
    clim_av[ind,5] <- mean(time_sub$Precipitation, na.rm = T)
    
    # Increment counter
    ind <- ind + 1
  }
}
clim_av <- as.data.frame(clim_av)
colnames(clim_av) <- c('Latitude', 'Longitude', 'Year', 'Temperature', 'Precipitation')

# Save!
save(clim_av, file = 'Climate/processed_climate.RData')
