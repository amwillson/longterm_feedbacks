## This script formats data for running GJAM at Harvard Forest with
## tree ring-derived aboveground biomass data
## Data to be formatted includes climate drivers, species-specific aboveground biomass increment,
## and species-specific effort (calculated as species-specific biomass precision)

rm(list = ls())

library(tidyverse)

# Load data from step 1
load('TreeRings/Data/Raw/HARVARD_dat.RData')

#### Prepare ####

## This section sets up further data processing by creating
## matrices & vectors in the correct dimensions

# Make empty matrices for data
xdata = matrix(NA, nrow = length(c(1960:2011)), ncol = 3)
ydata = matrix(NA, nrow = (nrow(xdata) + 1), ncol = 5)

# Label columns & rows of both matrices
colnames(xdata) = c('Temp', 'Precip', 'Year')
colnames(ydata) = c('ACRU', 'FAGR', 'QURU', 'QUVE', 'TSCA')

rownames(xdata) = seq(from = 1960, to = 2011, by = 1)
rownames(ydata) = c(rownames(xdata),2012)

#### Climate processing ####

## Prepare data for the years of interest
## I am using 1960:2011 because this represents a reasonable time frame without running
## too much risk of running into problems from the fading record
## 2012 is the fina lyear of tree ring data, but we are using t-1 climate data

# Truncate climate data
temp.mat = tail(temp.mat, n = 56)
temp.mat = head(temp.mat, n = 52)
precip.mat = tail(precip.mat, n = 56)
precip.mat = head(precip.mat, n = 52)

# Take the annual averages
xdata[,1] <- as.numeric(apply(temp.mat, 1, mean))
xdata[,2] <- as.numeric(apply(precip.mat, 1, mean))
xdata[,3] <- as.factor(rownames(xdata))

#### Species-specific aboveground biomass ####

# Format species-specific aboveground biomass
y <- obs.list[["obs.mean"]]
y <- unlist(y)
y_mat <- matrix(NA, nrow = length(y), ncol = 2)
y_mat[,1] = names(y)
y_mat[,2] = y

y_mat = as.data.frame(y_mat)
colnames(y_mat) <- c('Names', 'MeanAGB')

# Take species names from vector labels in the original data format
y_mat <- y_mat %>%
  mutate(Year = substr(Names, 1, 4),
         Species = if_else(grepl('Acer', Names), 'ACRU', 
                           if_else(grepl('Fagus', Names), 'FAGR',
                                   if_else(grepl('Rubra', Names), 'QURU',
                                           if_else(grepl('Velutina', Names), 'QUVE',
                                                   if_else(grepl('Tsuga', Names), 'TSCA', 'None')))))) %>%
  select(-Names) %>%
  mutate(MeanAGB = as.numeric(MeanAGB))

# Format and filter for only the years of interest
y_mat_wide <- y_mat %>%
  pivot_wider(names_from = Species, values_from = MeanAGB) %>%
  filter(Year %in% c(1960:2012)) %>%
  select(-Year)
y_mat_wide <- as.matrix(y_mat_wide)

#### Species-specific aboveground biomass increment ####

# Convert to species-specific aboveground biomass increment
yinc <- matrix(NA, nrow = nrow(ydata)-1, ncol = ncol(ydata))

for(i in 1:(nrow(ydata)-1)){
  for(j in 1:ncol(ydata)){
    yinc[i,j] <- y_mat_wide[i+1,j] - y_mat_wide[i,j]
  }  
}

colnames(yinc) <- colnames(ydata)
rownames(yinc) <- rownames(ydata[1:(nrow(ydata)-1),])

ydata = yinc

#### Biomass effort ####

## Here, I am assuming that the "effort" as defined in the GJAM documentation
## in my case can be considered proportional to uncertainty. That is, the greater the precision
## of the observation, the greater the effort for obtaining those observations.
## In general, then, effort will decrease going back in time, as we have less information
## about the structure of forests for further in the past, and during periods of uncertainty
## as to when a tree in a given plot died

# Covariance matrices from data
obs.cov <- obs.list$obs.cov

# Storage array
cov_array <- array(NA, dim = c(5, 5, 113))

# Storing covariance matrices in an array for easier processing
for(i in 1:113){
  cov_array[,,i] <- obs.cov[[i]]
}

# Storage matrix
var_mat <- matrix(NA, nrow = 113, ncol = 5)

# Take variances (diagonals in each covariance matrix)
for(i in 1:113){
  temp <- cov_array[,,i]
  var_mat[i,] <- diag(temp)
}

# Format variance matrix
rownames(var_mat) <- c(1900:2012)
colnames(var_mat) <- c('ACRU', 'FAGR', 'QURU', 'QUVE', 'TSCA')

var_df <- as.data.frame(var_mat)
var_df$year <- rownames(var_df)
var_df$year <- as.numeric(var_df$year)

## There are two ways that we could use precision to inform the effort of the observations
## 1. Plot x year effort: effort varies by year and by plot, but is the same across all species
## 2. Plot x year x species effort: effort varies by year and by plot, and is different across species
## I explore both options here

# Average variance across species
var_df <- var_df[,-6]
var_df$total <- rowMeans(var_df)
var_df$year <- rownames(var_df)
var_df$year <- as.numeric(var_df$year)

# Convert to precision
var_df$precision <- 1 / var_df$total

var_df <- var_df %>%
  filter(year %in% (1960:2011))

# Convert species-specific variances to precision
prec_df <- var_df %>%
  select(ACRU, FAGR, QURU, QUVE, TSCA) %>%
  mutate(ACRU = 1/ACRU,
         FAGR = 1/FAGR,
         QURU = 1/QURU,
         QUVE = 1/QUVE,
         TSCA = 1/TSCA)

#### Save ####

save(xdata, ydata, var_df, prec_df, file = 'TreeRings/Data/Processed/HARVARD_proc.RData')
