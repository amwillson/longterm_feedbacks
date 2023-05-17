## Run GJAM for the Upper Midwest

rm(list = ls())

library(gjam)
library(tidyverse)

# Load data

load('FossilPollen/Data/spatial_data.RData')

#### Set up GJAM ####

elist <- list(columns = 1:ncol(ydata),
                   values = edata)

form1 <- as.formula(~Temperature + Precipitation + Temperature * Precipitation)

#### Run GJAM ####

# Prepare & run model
mlist = list(ng = 100, burnin = 20, typeNames = 'FC',# betaPrior = prior,
             effort = elist)
out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)
