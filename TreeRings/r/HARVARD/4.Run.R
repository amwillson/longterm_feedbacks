## Run GJAM at Harvard Forest using tree ring-derived aboveground biomass increment

## Author: AM Willson

rm(list = ls())

load('TreeRings/Data/Processed/HARVARD_prep.RData')

#### Run GJAM ####

# Create list of model parameters
mlist = list(ng = 100000, burnin = 10000, typeNames = 'CA', betaPrior = prior, effort = elist)

# Run GJAM
out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

# Quick, standard plots of model output
gjamPlot(out)
