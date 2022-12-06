## This script loads data for use in GJAM at Harvard Forest
## Data include biomass from tree rings & climate (temperature & precipitation)

## Author: AM Willson

rm(list = ls())

library(tidyverse)

#### Loading data ####

# Tree ring-derived species-specific aboveground biomass
load('TreeRings/Data/Raw/sda.obs.HARVARD.Rdata')
  
# Temperature & precipitation
load('TreeRings/Clim/HARVARD/linkages/bcc.csm1.1_001.01.Rdata') # note that this is just one random ensemble member 

#### Save ####

# Save all data required in one file
save(obs.list, precip.mat, temp.mat, file = 'TreeRings/Data/Raw/HARVARD_dat.RData')