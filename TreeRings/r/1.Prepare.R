## Peparing data for GJAM
## Author: AM Willson

rm(list = ls())

## Data are reconstructions of species-specific aboveground biomass
## at five sites in the northeast and upper midwest USA
## Biomass is reconstructed from tree rings for timeseries of ~50 years

#### Load data ####

load('TreeRings/Data/sda.obs.GOOSE.Rdata') # Goose Egg
obs.list_GOOSE <- obs.list

load('TreeRings/Data/sda.obs.HARVARD.Rdata') # Havard Forest
obs.list_HARVARD <- obs.list

load('TreeRings/Data/sda.obs.NORTHROUND.Rdata') # Northround Pond
obs.list_NORTHROUND <- obs.list

load('TreeRings/Data/sda.obs.ROOSTER.Rdata') # Rooster Hill
obs.list_ROOSTER <- obs.list

load('TreeRings/Data/sda.obs.SYLVANIA.Rdata') # Sylvania Wilderness
obs.list_SYLVANIA <- obs.list

rm(obs.list)
